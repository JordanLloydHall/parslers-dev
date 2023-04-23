#![feature(assert_matches)]
#![feature(box_patterns)]
extern crate proc_macro;

use std::f32::consts::E;

// use proc_macro::{TokenStream, TokenTree};
use proc_macro2::{TokenStream, TokenTree};
use quote::__private::ext::RepToTokensExt;

// # Grammar
// ```
// <spec> ::= <statement>
//             | <statement> <program>
// <statement> ::= ( 'pub' ) ? 'let' <ident> '=' <parser> ';'
// <parser> ::= <ident>
//             | 'pure' '(' <pure_val> ')
//             | 'satisfy' '(' <ident> ')'
//             | 'try' '(' <parser> ')'
//             | 'look' '(' <parser> ')'
//             | 'neg_look' '(' <parser> ')'
//             | 'ap' '(' <parser> ',' <parser> ')'
//             | 'then' '(' <parser> ( ',' <parser> ) + ')'
//             | 'before' '(' <parser> ( ',' <parser> ) + ')'
//             | 'or' '(' <parser> ( ',' <parser> ) + ')'
//             | 'empty'
//             | 'branch' '(' <parser>, <parser>, <parser> ')'
// <ident> ::= [a-zA-Z_][a-zA-Z0-9_]*
// <pure_val> ::= 'func' '(' <ident> ')'
//             | 'val' '(' <string> ')'
// <string> ::= [^"]*
// ```

#[derive(Debug, PartialEq, Clone)]
struct Spec {
    statements: Vec<Statement>,
}

#[derive(Debug, PartialEq, Clone)]
struct Statement {
    public: bool,
    ident: String,
    parser: Parser,
}

#[derive(Debug, PartialEq, Clone)]
enum Parser {
    Ident(String),
    Pure(PureVal),
    Satisfy(Func),
    Try(Box<Parser>),
    Look(Box<Parser>),
    NegLook(Box<Parser>),
    Ap(Box<Parser>, Box<Parser>),
    Then(Vec<Parser>),
    Before(Vec<Parser>),
    Or(Vec<Parser>),
    Empty,
    Branch(Box<Parser>, Box<Parser>, Box<Parser>),
}

#[derive(Debug, PartialEq, Clone)]
enum PureVal {
    Val(String),
    Func(Func),
}

#[derive(Debug, PartialEq, Clone)]
struct Func {
    ident: String,
}

#[proc_macro]
pub fn parser(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let spec = parser::parse_spec(input.into());

    // Output the parser spec
    println!("{:#?}", spec);

    "".parse().unwrap()
}

mod parser {
    use super::*;
        pub fn parse_spec(input: TokenStream) -> Spec {
        let mut statements = Vec::new();
        let mut iter = input.into_iter();
        while let Some(statement) = parse_statement(&mut iter) {
            statements.push(statement);
        }
        Spec { statements }
    }

    fn parse_statement(iter: &mut proc_macro2::token_stream::IntoIter) -> Option<Statement> {
        let mut public = false;
        if let Some(TokenTree::Ident(ident)) = iter.next() {
            if ident.to_string() == "pub" {
                public = true;
            } else {
                return None;
            }
        }
        if let Some(TokenTree::Ident(ident)) = iter.next() {
            if ident.to_string() == "let" {
                if let Some(TokenTree::Ident(ident)) = iter.next() {
                    if let Some(TokenTree::Punct(punct)) = iter.next() {
                        if punct.as_char() == '=' {
                            if let Some(parser) = parse_parser(iter) {
                                if let Some(TokenTree::Punct(punct)) = iter.next() {
                                    if punct.as_char() == ';' {
                                        return Some(Statement {
                                            public,
                                            ident: ident.to_string(),
                                            parser,
                                        });
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        None
    }

    macro_rules! compose {
        ( $last:expr ) => { $last };
        ( $head:expr, $($tail:expr), +) => {
            compose_two($head, compose!($($tail),+))
        };
    }

    fn compose_two<A, B, C, G, F>(f: F, g: G) -> impl Fn(A) -> C
    where
        F: Fn(A) -> B,
        G: Fn(B) -> C,
    {
        move |x| g(f(x))
    }

    fn parse_parser(iter: &mut proc_macro2::token_stream::IntoIter) -> Option<Parser> {


        if let Some(TokenTree::Ident(ident)) = iter.next() {
            match ident.to_string().as_str() {
                "pure" => {
                    if let Some(TokenTree::Group(group)) = iter.next() {
                        parse_pure_val(&mut group.stream().into_iter()).map(Parser::Pure)
                    } else {
                        None
                    }
                }
                "satisfy" => {
                    if let Some(TokenTree::Group(group)) = iter.next() {
                        if let Some(TokenTree::Ident(ident)) = group.stream().into_iter().next() {
                            Some(Parser::Satisfy(Func {
                                ident: ident.to_string(),
                            }))
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                }
                "try" => {
                    if let Some(TokenTree::Group(group)) = iter.next() {
                        parse_parser(&mut group.stream().into_iter())
                            .map(compose!(Box::new, Parser::Try))
                    } else {
                        None}
                }
                "look" => {
                    if let Some(TokenTree::Group(group)) = iter.next() {
                        parse_parser(&mut group.stream().into_iter())
                            .map(compose!(Box::new, Parser::Look))
                    } else {
                        None
                    }
                }
                "neg_look" => {
                    if let Some(TokenTree::Group(group)) = iter.next() {
                        parse_parser(&mut group.stream().into_iter())
                            .map(compose!(Box::new, Parser::NegLook))
                    } else {None }
                }
                "ap" => {
                    if let Some(TokenTree::Group(group)) = iter.next() {
                        let mut iter = group.stream().into_iter();
                        let parser1 = parse_parser(&mut iter)?;
                        if let Some(TokenTree::Punct(punct)) = iter.next() {
                            if punct.as_char() != ',' {
                                return None;
                            }
                        } else {
                            return None;
                        }
                        let parser2 = parse_parser(&mut iter)?;

                        Some(Parser::Ap(Box::new(parser1), Box::new(parser2)))
                    } else {
                        None
                    }
                }
                "or" => {
                    if let Some(TokenTree::Group(group)) = iter.next() {
                        let mut iter = group.stream().into_iter();
                        let mut or_parsers = Vec::new();
                        while let Some(parser) = parse_parser(&mut iter) {
                            or_parsers.push(parser);
                            if let Some(TokenTree::Punct(punct)) = iter.next() {
                                if punct.as_char() != ',' {
                                    return None;
                                }
                            } else {
                                break;
                            }
                        }

                        Some(Parser::Or(or_parsers))
                    } else {
                        None
                    }
                }
                "then" => {
                    if let Some(TokenTree::Group(group)) = iter.next() {
                        let mut iter = group.stream().into_iter();
                        let mut or_parsers = Vec::new();
                        while let Some(parser) = parse_parser(&mut iter) {
                            or_parsers.push(parser);
                            if let Some(TokenTree::Punct(punct)) = iter.next() {
                                if punct.as_char() != ',' {
                                    return None;
                                }
                            } else {
                                break;
                            }
                        }

                        Some(Parser::Then(or_parsers))
                    } else {
                        None
                    }
                }
                "before" => {
                    if let Some(TokenTree::Group(group)) = iter.next() {
                        let mut iter = group.stream().into_iter();
                        let mut or_parsers = Vec::new();
                        while let Some(parser) = parse_parser(&mut iter) {
                            or_parsers.push(parser);
                            if let Some(TokenTree::Punct(punct)) = iter.next() {
                                if punct.as_char() != ',' {
                                    return None;
                                }
                            } else {
                                break;
                            }
                        }

                        Some(Parser::Before(or_parsers))
                    } else {
                        None
                    }
                }
                "branch" => {
                    if let Some(TokenTree::Group(group)) = iter.next() {
                        let mut iter = group.stream().into_iter();
                        let parser1 = parse_parser(&mut iter)?;
                        if let Some(TokenTree::Punct(punct)) = iter.next() {
                            if punct.as_char() != ',' {
                                return None;
                            }
                        } else {
                            return None;
                        }
                        let parser2 = parse_parser(&mut iter)?;
                        if let Some(TokenTree::Punct(punct)) = iter.next() {
                            if punct.as_char() != ',' {
                                return None;
                            }
                        } else {
                            return None;
                        }
                        let parser3 = parse_parser(&mut iter)?;

                        Some(Parser::Branch(Box::new(parser1), Box::new(parser2), Box::new(parser3)))
                    } else {
                        None
                    }
                }
                "empty" => Some(Parser::Empty),
                _ => Some(Parser::Ident(ident.to_string())),
            }
        } else {
            None
        }
    }

    fn parse_pure_val(iter: &mut proc_macro2::token_stream::IntoIter) -> Option<PureVal> {
        if let Some(TokenTree::Ident(ident)) = iter.next() {
            match ident.to_string().as_str() {
                "func" => {
                    if let Some(TokenTree::Group(group)) = iter.next() {
                        return group
                            .stream()
                            .into_iter()
                            .next()
                            .map(|tt| PureVal::Func(Func {
                                ident: tt.to_string()
                            }
                            ));
                    }
                }
                "val" => {
                    if let Some(TokenTree::Group(group)) = iter.next() {
                        return group
                            .stream()
                            .into_iter()
                            .next()
                            .map(|tt| PureVal::Val(tt.to_string()));
                    }
                }
                _ => {}
            }
        }
        None
    }

    #[test]
    fn parser_parses_pure_val_correctly() {
        use quote::quote;

        let input: proc_macro2::TokenStream = quote! {
            pure(val("hello"))
        }
        .into();

        let mut iter = input.into_iter();

        let parser = parse_parser(&mut iter).unwrap();

        assert_eq!(parser, Parser::Pure(PureVal::Val("\"hello\"".to_string())));
    }

    #[test]
    fn parser_parses_satisfy_correctly() {
        use quote::quote;

        let input: proc_macro2::TokenStream = quote! {
            satisfy(hello)
        }
        .into();

        let mut iter = input.into_iter();

        let parser = parse_parser(&mut iter).unwrap();

        assert_eq!(
            parser,
            Parser::Satisfy(Func {
                ident: "hello".to_string()
            })
        );
    }

    #[test]
    fn parser_parses_try_correctly() {
        use quote::quote;

        let input: proc_macro2::TokenStream = quote! {
            try(satisfy(hello))
        }
        .into();

        let mut iter = input.into_iter();

        let parser = parse_parser(&mut iter).unwrap();

        assert_eq!(
            parser,
            Parser::Try(Box::new(Parser::Satisfy(Func {
                ident: "hello".to_string()
            })))
        );
    }

    #[test]
    fn parser_parses_ap_correctly() {
        use quote::quote;
        use std::assert_matches::assert_matches;

        let input: proc_macro2::TokenStream = quote! {
            ap(pure(func(hello)), pure(val("world")))
        }
        .into();

        let mut iter = input.into_iter();

        let parser = parse_parser(&mut iter);

        assert_matches!(
            parser,
            Some(
                Parser::Ap(
                    box Parser::Pure(
                        PureVal::Func(
                            Func {
                                ident: x
                            }
                        )
                    ),
                    box Parser::Pure(
                        PureVal::Val(y)
                    )
                )
            ) if x == "hello" && y == "\"world\""
        );
    }

    #[test]
    fn parser_parses_or_correctly() {
        use quote::quote;
        use std::assert_matches::assert_matches;

        let input: proc_macro2::TokenStream = quote! {
            or(pure(func(hello)), pure(val("world")), pure(val("hello")))
        }
        .into();

        let mut iter = input.into_iter();

        let parser = parse_parser(&mut iter);

        assert_matches!(
            parser,
            Some(
                Parser::Or(
                    v
                )
                ) if v == vec![Parser::Pure(
                    PureVal::Func(
                        Func {
                            ident: "hello".to_string()
                        }
                    )
                ),
                Parser::Pure(
                    PureVal::Val("\"world\"".to_string())
                ),
                Parser::Pure(
                    PureVal::Val("\"hello\"".to_string())
                )]
        );
    }

    #[test]
    fn parser_parses_branch_correctly() {
        use quote::quote;
        use std::assert_matches::assert_matches;

        let input: proc_macro2::TokenStream = quote! {
            branch(pure(func(hello)), pure(val("world")), pure(val("hello")))
        }
        .into();

        let mut iter = input.into_iter();

        let parser = parse_parser(&mut iter);

        assert_matches!(
            parser,
            Some(
                Parser::Branch(
                    box Parser::Pure(
                        PureVal::Func(
                            Func {
                                ident: x
                            }
                        )
                    ),
                    box Parser::Pure(
                        PureVal::Val(y)
                    ),
                    box Parser::Pure(
                        PureVal::Val(z)
                    )
                )
            ) if x == "hello" && y == "\"world\"" && z == "\"hello\""
        );
    }}