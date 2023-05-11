use crate::ast;
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
        ast::Parser::Satisfy(ast::Func { ident }) => gen_satisfy(&ident),
        ast::Parser::Then(first, second) => gen_then(*first, *second),
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
            quote! { Ok((#ident, input)) }
        }
    }
}

fn gen_satisfy(ident: &str) -> TokenStream {
    let ident = syn::Ident::new(ident, proc_macro2::Span::call_site());
    quote! {

            let mut iter = input.chars();
            let c = iter.next().ok_or(())?;
            if (#ident)(c) {
                Ok((c, iter.as_str()))
            } else {
                Err(())
            }

    }
}

fn gen_then(first: ast::Parser, second: ast::Parser) -> TokenStream {
    let first = gen_parser(first);
    let second = gen_parser(second);
    // let last = parsers.next().unwrap();
    quote! {

            let (_, input) = {#first}?;
            #second

    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use quote::quote;

    #[test]
    fn test_gen_statement() {
        let statement = ast::Statement {
            public: false,
            ident: "a".to_string(),
            type_: syn::parse_str("Result<(&str, &str), ()>").unwrap(),
            parser: ast::Parser::Pure(ast::PureVal::Val("\"a\"".to_string())),
        };
        let expected = quote! {
            pub fn a(input: &str) -> Result<(&str , &str), ()> {
                Ok(("a", input))
            }
        }
        .to_string();
        assert_eq!(gen_statement(statement).to_string(), expected);
    }
}
