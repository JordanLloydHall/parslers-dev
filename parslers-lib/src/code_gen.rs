use crate::ast;
use proc_macro2::TokenStream;
use quote::quote;
use syn::ExprClosure;

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
            quote! { Ok((#ident, input)) }
        }
    }
}

fn gen_satisfy(ident: syn::Expr) -> TokenStream {
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

fn gen_then<I>(parsers: I) -> TokenStream
where
    I: IntoIterator<Item = ast::Parser>,
    <I as IntoIterator>::IntoIter: DoubleEndedIterator,
{
    let mut parsers = parsers.into_iter().rev().map(gen_parser);
    let last = parsers.next().unwrap();
    quote! {

            #(let (_, input) = {#parsers}?;)*
            #last

    }
}
