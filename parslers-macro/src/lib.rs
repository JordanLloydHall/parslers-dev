#![feature(assert_matches)]
#![feature(box_patterns)]
extern crate proc_macro;

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
mod parser;

#[proc_macro]
pub fn parser(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let spec = parser::parse_spec(input.into());
    let spec = codegen::gen_spec(spec);

    spec.into()
}

mod codegen {
    use parslers_lib::ast;
    use proc_macro2::TokenStream;
    use quote::quote;

    pub fn gen_spec(spec: ast::Spec) -> TokenStream {
        let statements = spec.statements.into_iter().map(gen_statement);
        quote! {
            #(#statements)*
        }
    }

    fn gen_statement(statement: ast::Statement) -> TokenStream {
        // let public = statement.public;
        let ident = syn::Ident::new(&statement.ident, proc_macro2::Span::call_site());
        let type_ = statement.type_;
        let parser = gen_parser(statement.parser);
        quote! {
            pub fn #ident(input: &str) -> Result<(#type_, &str), ()> {
                #parser
            }
        }
    }

    fn gen_parser(parser: ast::Parser) -> TokenStream {
        match parser {
            ast::Parser::Pure(pure_val) => gen_pure_val(pure_val),
            ast::Parser::Satisfy(ast::Func { ident }) => gen_satisfy(ident),
            ast::Parser::Then(parsers) => gen_then(parsers),
            _ => unimplemented!(),
        }
    }

    fn gen_pure_val(pure_val: ast::PureVal) -> TokenStream {
        match pure_val {
            ast::PureVal::Val(val) => {
                let val = syn::parse_str::<syn::Expr>(&val).unwrap();
                quote! { Ok((#val, input)) }
            }
            ast::PureVal::Func(ast::Func { ident }) => {
                let val = syn::parse_str::<syn::Expr>(&ident).unwrap();
                quote! { Ok((#val, input)) }
            }
        }
    }

    fn gen_satisfy(ident: String) -> TokenStream {
        let ident = syn::Ident::new(&ident, proc_macro2::Span::call_site());
        quote! {
            
                let mut iter = input.chars();
                let c = iter.next().ok_or(())?;
                if #ident(c) {
                    Ok((c, iter.as_str()))
                } else {
                    Err(())
                }
            
        }
    }

    fn gen_then<I>(parsers: I) -> TokenStream
        where
            I: IntoIterator<Item = ast::Parser>,
            <I as IntoIterator>::IntoIter: DoubleEndedIterator {

        let mut parsers = parsers.into_iter().rev().map(gen_parser);
        let last = parsers.next().unwrap();
        quote! {
            
                #(let (_, input) = {#parsers}?;)*
                #last
            
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;
        use crate::parser;
        use quote::quote;

        #[test]
        fn test_gen_statement() {
            let input: proc_macro2::TokenStream = quote! {
                pub let a: &str = pure(val("a"));
            }
            .into();

            let statement = parser::parse_statement(&mut input.into_iter()).unwrap();
            let expected = quote! {
                pub fn a(input: &str) -> Result<(&str , &str), ()> {
                    Ok(("a", input))
                }
            }
            .to_string();
            assert_eq!(gen_statement(statement).to_string(), expected);
        }
    }
}
