//! Parser for WACC language grammar.
//!
//! [parse](parse) is the entry point. On success, will return [Program](Program)
//! and will return a [Summary](Summary) on failure.

mod lexer;
#[cfg(test)]
mod tests;

use super::{
    ASTWrapper, AssignLhs, AssignRhs, BinOp, Expr, ExprWrap, Function, Param, Program, Stat,
    StatWrap, Type, UnOp,
};
use lexer::{parse_ident, ws, Lexer};
use nom::{
    branch::alt,
    character::complete::{char, none_of, one_of},
    combinator::{cut, eof, map, map_res, opt, success, value},
    error::{context, ParseError},
    multi::{many0, many1, separated_list0},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    IResult,
};
use nom_supreme::{
    error::{BaseErrorKind, ErrorTree, StackContext},
    final_parser::final_parser,
    multi::collect_separated_terminated,
    ParserExt,
};
use parslers_lib::reflect::Reflect;
use std::{
    collections::{HashMap, LinkedList},
    fmt,
    iter::zip,
    path::{Path, PathBuf},
};

use lexer::{parse_int, str_delimited};

/// Wraps the result of the parser in a [ASTWrapper]
///
/// Mimics the builder pattern's span in order to move building the span
/// outside of parsing the AST node itself.
///
/// * `inner` - Parser whose Result is wrapped in a [ASTWrapper].
fn span<'a, F: 'a, O: Reflect, E: ParseError<&'a str>>(
    mut inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, ASTWrapper<String, O>, E>
where
    F: FnMut(&'a str) -> IResult<&'a str, O, E>,
{
    move |input| {
        let (rest, ret) = inner(input)?;
        Ok((
            rest,
            ASTWrapper("".to_owned(), ret),
        ))
    }
}

pub fn parse_program(input: &str) -> IResult<&str, Program<String, String>> {
    map(
        delimited(
            preceded(ws(success(())), Lexer::Begin.parser()),
            tuple((many0(span(parse_func)), parse_stats(Lexer::End))),
            eof,
        ),
        |(funcs, stats)| Program(funcs, stats),
    )(input)
}

/// Parses an input string into a vector of [functions](Function).
///
/// Some examples of valid programs:
/// ```text
/// begin
///     skip
/// end
/// ```
///
/// ```text
/// begin
///     int add(int a, int b) is
///         return a + b
///     end
///     int ret = call add(5, 10);
///     println ret
/// end
/// ```
///
/// Example of invalid program:
/// ```text
/// begin
/// end
/// ```
#[allow(clippy::type_complexity)]
fn parse_module(input: &str) -> IResult<&str, Vec<ASTWrapper<String, Function<String, String>>>> {
    map(
        terminated(
            opt(delimited(
                Lexer::Begin.parser(),
                many0(span(parse_func)),
                Lexer::End.parser(),
            )),
            eof,
        ),
        |funcs| funcs.unwrap_or_default(),
    )(input)
}

/// Parses an input string into a [Function].
///
/// Cuts after parsing "(", so that error handling is well-formatted.
fn parse_func(input: &str) -> IResult<&str, Function<String, String>> {
    map(
        tuple((
            parse_type,
            parse_ident,
            preceded(
                Lexer::OpenParen.parser(),
                alt((
                    collect_separated_terminated(
                        span(parse_param),
                        Lexer::Comma.parser(),
                        Lexer::CloseParen.parser(),
                    ),
                    map(Lexer::CloseParen.parser(), |_| vec![]),
                ))
                .cut(),
            ),
            preceded(Lexer::Is.parser(), parse_stats(Lexer::End)),
        )),
        |(t, id, params, block)| Function(t, ASTWrapper(id.clone(), id.into()), params, block),
    )(input)
}

/// Combinator that takes in a terminating [Lexer] and returns a parser for
/// parsing statement blocks.
///
/// We make a new combinator as parsing statements with different terminating
/// tokens comes up a lot. This allows the process to be streamlined.
///
/// Cuts immediately, and after parsing the first [Stat], so that error
/// handling is well-formatted.
fn parse_stats<'a>(
    term: Lexer,
) -> impl FnMut(&'a str) -> IResult<&'a str, Vec<StatWrap<String, String>>> {
    cut(context(
        "Statement/Block Terminator",
        collect_separated_terminated(
            span(parse_stat).cut(),
            Lexer::SemiColon.parser(),
            term.parser(),
        ),
    ))
}

/// Parser for building a [Stat].
///
/// Two sub-parsers are built up first for brevity: parse_while and parse_if.
/// These are large parsers so it benefits us to break up the code, but would
/// not be worth moving them into new parsers as they are only used here and
/// can be surely inlined by the compiler.
///
/// Cuts in appropriate places, so that error
/// handling is well-formatted.
fn parse_stat(input: &str) -> IResult<&str, Stat<String, String>> {
    let parse_while = preceded(
        Lexer::While.parser(),
        separated_pair(parse_expr, Lexer::Do.parser(), parse_stats(Lexer::Done)).cut(),
    );
    let parse_if = preceded(
        Lexer::If.parser(),
        tuple((
            separated_pair(parse_expr, Lexer::Then.parser(), parse_stats(Lexer::Else)),
            parse_stats(Lexer::Fi),
        ))
        .cut(),
    );

    let call = context(
        "Function call",
        delimited(
            Lexer::Call.parser(),
            separated_pair(
                parse_ident,
                Lexer::OpenParen.parser(),
                separated_list0(Lexer::Comma.parser(), parse_expr).cut(),
            )
            .cut(),
            Lexer::CloseParen.parser(),
        ),
    );

    context(
        "Statement",
        alt((
            value(Stat::Skip, Lexer::Skip.parser()),
            map(
                separated_pair(
                    tuple((parse_type, parse_ident.cut())),
                    Lexer::Assign.parser().cut(),
                    parse_rhs.cut(),
                ),
                |((t, id), rhs)| Stat::Def(t, id, rhs),
            ),
            preceded(Lexer::Read.parser(), map(parse_lhs.cut(), Stat::Read)),
            preceded(Lexer::Free.parser(), map(parse_expr.cut(), Stat::Free)),
            preceded(Lexer::Return.parser(), map(opt(parse_expr), Stat::Return)),
            preceded(Lexer::Exit.parser(), map(parse_expr.cut(), Stat::Exit)),
            preceded(
                Lexer::Println.parser(),
                map(parse_expr.cut(), Stat::PrintLn),
            ),
            preceded(Lexer::Print.parser(), map(parse_expr.cut(), Stat::Print)),
            map(parse_if, |((e, st), sf)| Stat::If(e, st, sf)),
            map(parse_while, |(e, s)| Stat::While(e, s)),
            preceded(
                Lexer::Begin.parser(),
                map(parse_stats(Lexer::End).cut(), Stat::Block),
            ),
            context(
                "Assign Statement",
                map(
                    separated_pair(parse_lhs, Lexer::Assign.parser().cut(), parse_rhs.cut()),
                    |(lhs, rhs)| Stat::Assign(lhs, rhs),
                ),
            ),
            map(call, |(id, es)| {
                Stat::VoidCall(ASTWrapper(id.clone(), id.into()), es)
            }),
        )),
    )(input)
}

/// Parses a parameter for function definitions and returns a [Stat]. Cuts
/// after the type is parsed so that error messaging is well-formed.
fn parse_param(input: &str) -> IResult<&str, Param<String>> {
    map(tuple((parse_type, parse_ident.cut())), |(t, ident)| {
        Param(t, ident)
    })(input)
}

/// First of two array descriptor parsers used in parsing types.
///
/// This allows for 0 or more sets of
/// "[" "]" to be parsed. Returns [usize] of the number of pairs of well-formed array
/// descriptors, as only this is needed when parsing types.
fn parse_array_desc0(input: &str) -> IResult<&str, usize> {
    map(
        many0(pair(
            Lexer::OpenBracket.parser(),
            Lexer::CloseBracket.parser(),
        )),
        |vec| vec.len(),
    )(input)
}

/// Second of two array descriptor parsers used in parsing types.
///  
/// This allows for 1 or more sets of
/// "[" "]" to be parsed. Returns [usize] of the number of pairs of well-formed array
/// descriptors, as only this is needed when parsing types.
/// This parser was specifically designed for parsing multi-level pair-types.
fn parse_array_desc1(input: &str) -> IResult<&str, usize> {
    map(
        many1(pair(
            Lexer::OpenBracket.parser(),
            Lexer::CloseBracket.parser(),
        )),
        |vec| vec.len(),
    )(input)
}

/// Parser for the [Type] AST Node. Can parse well-formed base types, array
/// types and pair types.
fn parse_type(input: &str) -> IResult<&str, Type> {
    map(
        pair(alt((parse_base_type, parse_pair_type)), parse_array_desc0),
        |(t, arr_depth)| match arr_depth {
            0 => t,
            d => Type::Array(Box::new(t), d),
        },
    )(input)
}

/// Parser for the [Type] AST Node. Handles the case when a pair type needs
/// to be parsed.
fn parse_pair_type(input: &str) -> IResult<&str, Type> {
    map(
        preceded(
            Lexer::Pair.parser(),
            delimited(
                Lexer::OpenParen.parser(),
                separated_pair(
                    parse_pair_elem_type,
                    Lexer::Comma.parser(),
                    parse_pair_elem_type,
                ),
                Lexer::CloseParen.parser(),
            ),
        ),
        |(f, s)| Type::Pair(Box::new(f), Box::new(s)),
    )(input)
}

/// Parser for the [Type] AST Node. Handles the case where you want to parse
/// the sub-type of a [Type::Pair]. This was complicated as the spec allows
/// for pair types to be pair-elem types, but only in the case that it's also
/// an array.
fn parse_pair_elem_type(input: &str) -> IResult<&str, Type> {
    alt((
        parse_pair_type,
        value(
            Type::Pair(Box::new(Type::Any), Box::new(Type::Any)),
            Lexer::Pair.parser(),
        ),
        map(
            tuple((parse_pair_type, parse_array_desc1.cut())),
            |(t, n)| Type::Array(Box::new(t), n),
        ),
        map(
            tuple((parse_base_type, parse_array_desc0.cut())),
            |(t, n)| match n {
                0 => t,
                n => Type::Array(Box::new(t), n),
            },
        ),
    ))(input)
}

/// Parser for the [Type] AST node.
fn parse_base_type(input: &str) -> IResult<&str, Type> {
    ws(alt((
        value(Type::Void, Lexer::Void.parser()),
        value(Type::Int, Lexer::Int.parser()),
        value(Type::Bool, Lexer::Bool.parser()),
        value(Type::Char, Lexer::Char.parser()),
        value(Type::String, Lexer::String.parser()),
    )))(input)
}

/// Parser for the left-hand side of an assign expression.
fn parse_lhs(input: &str) -> IResult<&str, AssignLhs<String, String>> {
    alt((
        map(preceded(Lexer::Fst.parser(), parse_expr.cut()), |pair| {
            AssignLhs::PairFst(pair)
        }),
        map(preceded(Lexer::Snd.parser(), parse_expr.cut()), |pair| {
            AssignLhs::PairSnd(pair)
        }),
        map(
            pair(
                parse_ident,
                alt((map(parse_array_elem, Some), success(None))),
            ),
            |(id, arr)| match arr {
                Some(arr) => AssignLhs::ArrayElem(id, arr),
                None => AssignLhs::Var(id),
            },
        ),
    ))(input)
}

/// Parser for an array element.
fn parse_array_elem(input: &str) -> IResult<&str, Vec<ExprWrap<String, String>>> {
    many1(delimited(
        Lexer::OpenBracket.parser(),
        parse_expr,
        Lexer::CloseBracket.parser(),
    ))(input)
}

/// Parser for the right-hand side of an assign expression.
fn parse_rhs(input: &str) -> IResult<&str, AssignRhs<String, String>> {
    let call = context(
        "Argument List",
        delimited(
            Lexer::Call.parser(),
            separated_pair(
                parse_ident,
                Lexer::OpenParen.parser(),
                separated_list0(Lexer::Comma.parser(), parse_expr).cut(),
            )
            .cut(),
            Lexer::CloseParen.parser(),
        ),
    );
    let array_liter = context(
        "Array Literal",
        delimited(
            Lexer::OpenBracket.parser(),
            separated_list0(Lexer::Comma.parser(), parse_expr).cut(),
            Lexer::CloseBracket.parser(),
        ),
    );
    context(
        "Assign RHS",
        alt((
            map(call, |(id, es)| {
                AssignRhs::Call(ASTWrapper(id.clone(), id.into()), es)
            }),
            map(
                span(preceded(Lexer::Fst.parser(), parse_expr.cut())),
                |ASTWrapper(s, e)| {
                    AssignRhs::Expr(ASTWrapper(s, Expr::UnOp(UnOp::Fst, Box::new(e))))
                },
            ),
            map(
                span(preceded(Lexer::Snd.parser(), parse_expr.cut())),
                |ASTWrapper(s, e)| {
                    AssignRhs::Expr(ASTWrapper(s, Expr::UnOp(UnOp::Snd, Box::new(e))))
                },
            ),
            map(parse_expr, AssignRhs::Expr),
            map(span(array_liter), AssignRhs::Array),
        )),
    )(input)
}

/// Entry-point parser for building a [Expr]. Precedence and associativity of
/// operators is handled in this parser.
fn parse_expr(input: &str) -> IResult<&str, ExprWrap<String, String>> {
    context("Expression", parse_expr_or)(input)
}

/// Performs a right-fold on an expression to build up an expression tree from
/// a vector of nodes. Right-folding allows for left-associative operators.
fn fold_expr<'a>(
    expr: ExprWrap<String, String>,
    rem: Vec<(String, ExprWrap<String, String>)>,
    input: &'a str,
) -> ExprWrap<String, String> {
    rem.into_iter()
        .fold(expr, |acc, val| parse_bin_op(val, acc, input))
}

/// Parser for a `expr1 || expr2` expression.
fn parse_expr_or(input: &str) -> IResult<&str, ExprWrap<String, String>> {
    let (rest, sub_exp) = parse_expr_and(input)?;
    let (rest, exprs) = many0(tuple((Lexer::Or.parser(), parse_expr_and.cut())))(rest)?;
    Ok((rest, fold_expr(sub_exp, exprs, input)))
}

/// Parser for a `expr1 && expr2` expression.
fn parse_expr_and(input: &str) -> IResult<&str, ExprWrap<String, String>> {
    let (rest, sub_exp) = parse_expr_eq(input)?;
    let (rest, exprs) = many0(tuple((Lexer::And.parser(), parse_expr_eq.cut())))(rest)?;
    Ok((rest, fold_expr(sub_exp, exprs, input)))
}

/// Parser for a `expr1 == expr2` expression.
fn parse_expr_eq(input: &str) -> IResult<&str, ExprWrap<String, String>> {
    let (rest, sub_exp) = parse_expr_cmp(input)?;
    let (rest, exprs) = many0(tuple((
        alt((Lexer::Eq.parser(), Lexer::Ne.parser())),
        parse_expr_cmp.cut(),
    )))(rest)?;
    Ok((rest, fold_expr(sub_exp, exprs, input)))
}

/// Parser for a `expr1 (>= | > | <= | <) expr2` expression.
fn parse_expr_cmp(input: &str) -> IResult<&str, ExprWrap<String, String>> {
    let (rest, sub_exp) = parse_expr_plus(input)?;
    let (rest, exprs) = many0(tuple((
        alt((
            Lexer::Gte.parser(),
            Lexer::Lte.parser(),
            Lexer::Gt.parser(),
            Lexer::Lt.parser(),
        )),
        parse_expr_plus.cut(),
    )))(rest)?;
    Ok((rest, fold_expr(sub_exp, exprs, input)))
}

/// Parser for a `expr1 (+ | -) expr2` expression.
fn parse_expr_plus(input: &str) -> IResult<&str, ExprWrap<String, String>> {
    let (rest, sub_exp) = parse_expr_mult(input)?;
    let (rest, exprs) = many0(tuple((
        alt((Lexer::Plus.parser(), Lexer::Minus.parser())),
        parse_expr_mult.cut(),
    )))(rest)?;
    Ok((rest, fold_expr(sub_exp, exprs, input)))
}

/// Parser for a `expr1 (* | / | %) expr2` expression.
fn parse_expr_mult(input: &str) -> IResult<&str, ExprWrap<String, String>> {
    let (rest, sub_exp) = span(parse_expr_atom)(input)?;
    let (rest, exprs) = many0(tuple((
        alt((
            Lexer::Mult.parser(),
            Lexer::Div.parser(),
            Lexer::Mod.parser(),
        )),
        span(parse_expr_atom).cut(),
    )))(rest)?;
    Ok((rest, fold_expr(sub_exp, exprs, input)))
}

/// Parser for unary expressions and bool/pair literals.
fn parse_literal_keywords(input: &str) -> IResult<&str, ExprWrap<String, String>> {
    alt((
        map(Lexer::True.parser(), |t| {
            ASTWrapper(t.clone(), Expr::Bool(t.parse::<bool>().unwrap()))
        }),
        map(Lexer::False.parser(), |t| {
            ASTWrapper(t.clone(), Expr::Bool(t.parse::<bool>().unwrap()))
        }),
        map(Lexer::Null.parser(), |t| ASTWrapper(t, Expr::Null)),
        map(
            span(preceded(Lexer::Bang.parser(), parse_expr.cut())),
            |ASTWrapper(s1, e)| ASTWrapper(s1, Expr::UnOp(UnOp::Neg, Box::new(e))),
        ),
        map(
            span(preceded(Lexer::Minus.parser(), parse_expr.cut())),
            |ASTWrapper(s1, e)| ASTWrapper(s1, Expr::UnOp(UnOp::Minus, Box::new(e))),
        ),
        map(
            span(preceded(Lexer::Len.parser(), parse_expr.cut())),
            |ASTWrapper(s1, e)| ASTWrapper(s1, Expr::UnOp(UnOp::Len, Box::new(e))),
        ),
        map(
            span(preceded(Lexer::Ord.parser(), parse_expr.cut())),
            |ASTWrapper(s1, e)| ASTWrapper(s1, Expr::UnOp(UnOp::Ord, Box::new(e))),
        ),
        map(
            span(preceded(Lexer::Chr.parser(), parse_expr.cut())),
            |ASTWrapper(s1, e)| ASTWrapper(s1, Expr::UnOp(UnOp::Chr, Box::new(e))),
        ),
    ))(input)
}

#[derive(Debug, Clone)]
struct CharLiteralError;

impl fmt::Display for CharLiteralError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "a character")
    }
}
impl std::error::Error for CharLiteralError {}

/// Parser for atomic expressions such as newpair, integers, string/character
/// literals, unary expressions and '('expr')'.
fn parse_expr_atom(input: &str) -> IResult<&str, Expr<String, String>> {
    let new_pair = delimited(
        pair(Lexer::Newpair.parser(), Lexer::OpenParen.parser().cut()),
        separated_pair(parse_expr, Lexer::Comma.parser(), parse_expr),
        Lexer::CloseParen.parser(),
    );
    alt((
        map(new_pair, |(left, right)| {
            Expr::BinOp(Box::new(left), BinOp::Newpair, Box::new(right))
        }),
        map_res(str_delimited("\'"), |s| {
            if s.len() != 1 {
                Err(CharLiteralError)
            } else {
                Ok(Expr::Char(s.chars().next().unwrap_or_default()))
            }
        }),
        map(parse_int, Expr::Int),
        map(str_delimited("\""), Expr::String),
        map(
            pair(parse_ident, opt(parse_array_elem)),
            |(id, arr)| match arr {
                Some(arr) => Expr::ArrayElem(id, arr),
                None => Expr::Var(id),
            },
        ),
        map(parse_literal_keywords, |ASTWrapper(_, e)| e),
        map(
            delimited(
                Lexer::OpenParen.parser(),
                parse_expr.cut(),
                Lexer::CloseParen.parser().cut(),
            ),
            |e| e.1,
        ),
    ))(input)
}

/// Matches a [str] unary operator and returns [UnOp]
fn parse_unary(op: &str) -> UnOp {
    match op {
        "!" => UnOp::Neg,
        "-" => UnOp::Minus,
        "len" => UnOp::Len,
        "ord" => UnOp::Ord,
        "chr" => UnOp::Chr,
        _ => unreachable!(),
    }
}

/// Matches the binary operator string and combines the two input [Expr]s
/// into a [Expr::BinOp]. Also calculates the associatity-agnsostic span for the
/// resulting [Expr], allowing for nice error translation
fn parse_bin_op<'a>(
    tup: (String, ExprWrap<String, String>),
    expr1: ExprWrap<String, String>,
    input: &'a str,
) -> ExprWrap<String, String> {
    let (op, expr2) = tup;

    // let min = expr1.0.as_ptr().min(expr2.0.as_ptr()) as usize - input.as_ptr() as usize;
    // let max = (expr2.0.as_ptr() as usize + expr2.0.len())
    //     .max(expr1.0.as_ptr() as usize + expr1.0.len())
    //     - input.as_ptr() as usize;
    // let s = input[min..max].to_owned();

    ASTWrapper(
        "".to_owned(),
        Expr::BinOp(
            Box::new(expr1),
            match op.as_str() {
                "+" => BinOp::Add,
                "-" => BinOp::Sub,
                "*" => BinOp::Mul,
                "/" => BinOp::Div,
                "%" => BinOp::Mod,
                ">" => BinOp::Gt,
                ">=" => BinOp::Gte,
                "<" => BinOp::Lt,
                "<=" => BinOp::Lte,
                "==" => BinOp::Eq,
                "!=" => BinOp::Ne,
                "&&" => BinOp::And,
                "||" => BinOp::Or,
                _ => unreachable!(),
            },
            Box::new(expr2),
        ),
    )
}
