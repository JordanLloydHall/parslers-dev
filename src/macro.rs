
// extern crate proc_macro;
// use proc_macro::{TokenStream, TokenTree};

// #[proc_macro]
// pub fn parsler(item: TokenStream) -> TokenStream {

//     let tokens = item.into_iter().next().expect("Expected one token");
//     let combinator = parse_or(tokens);

//     println!("{:#?}", combinator);

//     let optimised = combinator.optimise();

//     println!("{:#?}", optimised);

//     let code = format!("
    
//     fn parsler(input: &str) -> Result<((char, char), &str), String> {{
//         fn or<I, I2, O, F1, F2, E>(f1: F1, f2: F2) -> impl Fn(I) -> Result<(O, I2), E>
//         where
//             I: Clone,
//             F1: Fn(I) -> Result<(O, I2), E>,
//             F2: Fn(I) -> Result<(O, I2), E>,
//         {{
//             move |i: I| f1(i.clone()).or_else(|_| f2(i))
//         }}
//         fn and<I, O1, O2, F1, F2, E>(f1: F1, f2: F2) -> impl Fn(I) -> Result<((O1, O2), I), E>
//         where
//             F1: Fn(I) -> Result<(O1, I), E>,
//             F2: Fn(I) -> Result<(O2, I), E>,
//         {{
//             move |i: I| f1(i).and_then(|(i1, i2)| f2(i2).map(|(i3, i4)| ((i1, i3), i4)))
//         }}

//         fn char(c: char) -> impl Fn(&str) -> Result<(char, &str), String> {{
//             move |i: &str| {{
//                 let mut chars = i.chars();
//                 if chars.next() == Some(c) {{
//                     Ok((c, chars.as_str()))
//                 }} else {{
//                     Err(format!(\"Could not find char \"))
//                 }}
//             }}
//         }}
//         {}(input)
//     }}
    
//     ", optimised.to_rust());

//     println!("{}", code);

//     code.parse().unwrap()
// }

// ///
// /// <parser> ::= 'let' <identifier> '=' <combinator> ';'
// /// <combinator> ::= <parser> | 
// ///                 | <parser> '|' <parser> 
// ///                 | <parser> '*' <parser> 

// fn parser_parser(input: TokenTree) -> Combinator {
//     match input {
//         TokenTree::I
//         _ => panic!("Expected group"),
//     }
// }

// fn parse_or(input: TokenTree) -> Combinator {
//     match input {
//         TokenTree::Group(group) => {
//             let mut tokens = group.stream().into_iter();
//             let left_combinator = parse_and(tokens.next().unwrap());
//             match tokens.next() {
//                 Some(TokenTree::Punct(punct)) => {
//                     assert_eq!(punct.as_char(), '|');
//                 }
//                 _ => panic!("Expected |"),
//             }
//             let right_combinator = parse_and(tokens.next().unwrap());
//             Combinator::Or(Box::new(left_combinator), Box::new(right_combinator))
//         }
//         _ => panic!("Expected group"),
//     }
// }

// fn parse_and(input: TokenTree) -> Combinator {
//     match input {
//         TokenTree::Group(group) => {
//             let mut tokens = group.stream().into_iter();
//             let left_combinator = parse_char(tokens.next().unwrap());
//             let right_combinator = parse_char(tokens.next().unwrap());
//             Combinator::And(box left_combinator, box right_combinator)
//         }
//         _ => panic!("Expected group"),
//     }
// }

// fn parse_char(input: TokenTree) -> Combinator {
//     match input {
//         TokenTree::Literal(literal) => {
//             let string = literal.to_string();
//             let mut chars = string.chars();
//             assert_eq!(chars.next(), Some('\''));
//             let first = chars.next().unwrap();
//             assert_eq!(chars.next(), Some('\''));
//             assert!(chars.next().is_none());
//             Combinator::Char(first)
//         }
//         _ => panic!("Expected literal"),
//     }
// }

extern crate proc_macro;
use proc_macro::{TokenStream, TokenTree};

// # Grammar
// ```
// <spec> ::= <statement> 
//             | <statement> <program>
// <statement> ::= 'let' <ident> '=' <parser> ';'
// <parser> ::= ( 'pub' ) ? <ident> 
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

struct Spec {
    statements: Vec<Statement>,
}

struct Statement {
    public: bool,
    ident: String,
    parser: Parser,
}

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

enum PureVal {
    Val(String),
    Func(Func),
}

struct Func {
    ident: String
}

// #[proc_macro]
// pub fn parser(input: TokenStream) -> TokenStream {
//     let spec = parse_spec(input);
//     "".parse().unwrap()
// }