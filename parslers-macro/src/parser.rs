// use core::num::dec2flt::parse;

use parslers_lib::ast::*;
use proc_macro2::{Literal, TokenStream, TokenTree};

pub fn parse_spec(input: TokenStream) -> Spec {
    let mut statements = Vec::new();
    let mut iter = input.into_iter();
    while let Some(statement) = parse_statement(&mut iter) {
        statements.push(statement);
    }
    Spec { statements }
}

pub(crate) fn parse_statement(iter: &mut proc_macro2::token_stream::IntoIter) -> Option<Statement> {
    let mut public = false;
    if let Some(TokenTree::Ident(ident)) = iter.clone().next() {
        if ident.to_string() == "pub" {
            public = true;
            iter.next();
        }
    }
    if let Some(TokenTree::Ident(ident)) = iter.next() {
        if ident.to_string() == "let" {
            if let Some(TokenTree::Ident(ident)) = iter.next() {
                if let Some(TokenTree::Punct(punct)) = iter.next() {
                    if punct.as_char() == ':' {
                        let type_ = syn::parse2::<syn::Type>(TokenStream::from_iter(
                            iter.by_ref().take_while(|token| {
                                if let TokenTree::Punct(punct) = token {
                                    punct.as_char() != '='
                                } else {
                                    true
                                }
                            }),
                        ))
                        .ok()?;
                        if let Some(parser) = parse_parser(iter) {
                            if let Some(TokenTree::Punct(punct)) = iter.next() {
                                if punct.as_char() == ';' {
                                    return Some(Statement {
                                        public,
                                        ident: ident.to_string(),
                                        type_,
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
                    Some(Parser::Satisfy(Func {
                        ident: group.to_string(),
                    }))
                } else {
                    None
                }
            }
            "try" => {
                if let Some(TokenTree::Group(group)) = iter.next() {
                    parse_parser(&mut group.stream().into_iter())
                        .map(compose!(Box::new, Parser::Try))
                } else {
                    None
                }
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
                } else {
                    None
                }
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
                // if let Some(TokenTree::Group(group)) = iter.next() {
                //     // let mut iter = group.stream().into_iter();
                //     // let mut or_parsers = Vec::new();
                //     // while let Some(parser) = parse_parser(&mut iter) {
                //     //     or_parsers.push(parser);
                //     //     if let Some(TokenTree::Punct(punct)) = iter.next() {
                //     //         if punct.as_char() != ',' {
                //     //             return None;
                //     //         }
                //     //     } else {
                //     //         break;
                //     //     }
                //     // }

                //     // Some(Parser::Or(or_parsers))
                // } else {
                //     None
                // }
                None
            }
            "then" => {
                // if let Some(TokenTree::Group(group)) = iter.next() {
                //     let mut iter = group.stream().into_iter();
                //     let mut or_parsers = Vec::new();
                //     while let Some(parser) = parse_parser(&mut iter) {
                //         or_parsers.push(parser);
                //         if let Some(TokenTree::Punct(punct)) = iter.next() {
                //             if punct.as_char() != ',' {
                //                 return None;
                //             }
                //         } else {
                //             break;
                //         }
                //     }

                //     Some(Parser::Then(or_parsers))
                // } else {
                //     None
                // }
                None
            }
            "before" => {
                // if let Some(TokenTree::Group(group)) = iter.next() {
                //     let mut iter = group.stream().into_iter();
                //     let mut or_parsers = Vec::new();
                //     while let Some(parser) = parse_parser(&mut iter) {
                //         or_parsers.push(parser);
                //         if let Some(TokenTree::Punct(punct)) = iter.next() {
                //             if punct.as_char() != ',' {
                //                 return None;
                //             }
                //         } else {
                //             break;
                //         }
                //     }

                //     Some(Parser::Before(or_parsers))
                // } else {
                //     None
                // }
                None
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

                    Some(Parser::Branch(
                        Box::new(parser1),
                        Box::new(parser2),
                        Box::new(parser3),
                    ))
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
                    return group.stream().into_iter().next().map(|tt| {
                        PureVal::Func(Func {
                            ident: tt.to_string(),
                        })
                    });
                }
            }
            "val" => {
                if let Some(TokenTree::Group(group)) = iter.next() {
                    return Some(PureVal::Val(group.stream().to_string()));
                }
            }
            _ => {}
        }
    }
    None
}

fn parse_func(func: &str) -> syn::Expr {
    syn::parse_str::<syn::Expr>(func).unwrap()
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
            ident: "hello".to_owned()
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
            ident: "hello".to_owned()
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
        or(pure(func(hello)), pure(val("world")))
    }
    .into();

    let mut iter = input.into_iter();

    let parser = parse_parser(&mut iter);

    assert_matches!(
        parser,
        Some(
            Parser::Or(
                v1, v2
            )
            ) if v1.as_ref() == &Parser::Pure(
                PureVal::Func(
                    Func {
                        ident: "hello".to_owned()
                    }
                )
            ) && v2.as_ref() ==
            &Parser::Pure(
                PureVal::Val("\"world\"".to_string())
            )
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
}

#[test]
fn parser_parses_pub_statement_correctly() {
    use quote::quote;
    use std::assert_matches::assert_matches;

    let input: proc_macro2::TokenStream = quote! {
        pub let hello: &str = pure(val("world"));
    }
    .into();

    let mut iter = input.into_iter();

    let parser = parse_statement(&mut iter);

    assert_matches!(
        parser,
        Some(
            Statement {
                public: true,
                ident: x,
                type_: syn::Type::Reference(_),
                parser: Parser::Pure(
                    PureVal::Val(y)
                )
            }
        ) if x == "hello" && y == "\"world\""
    );
}

#[test]
fn parser_parses_statement_correctly() {
    use quote::quote;
    use std::assert_matches::assert_matches;

    let input: proc_macro2::TokenStream = quote! {
        let hello: &str = pure(val("world"));
    }
    .into();

    let mut iter = input.into_iter();

    let parser = parse_statement(&mut iter);

    assert_matches!(
        parser,
        Some(
            Statement {
                public: false,
                ident: x,
                type_: syn::Type::Reference(_),
                parser: Parser::Pure(
                    PureVal::Val(y)
                )
            }
        ) if x == "hello" && y == "\"world\""
    );
}
