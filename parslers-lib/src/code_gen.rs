use std::collections::{HashMap, HashSet};

use crate::{
    ast::{self, Parser, Spec, Statement},
    parsler::Parsler,
    reflect::Reflect,
};
use proc_macro2::TokenStream;
use quote::quote;

pub struct CompileContext {
    functions: HashMap<String, String>,
    named_parsers: HashMap<String, Option<(ast::Parser, syn::Type)>>,
}

impl CompileContext {
    pub fn new() -> Self {
        Self {
            functions: HashMap::new(),
            named_parsers: HashMap::new(),
        }
    }

    pub fn add_function(&mut self, func: &impl Reflect) -> String {
        let num_functions = self.functions.len();
        self.functions
            .entry(func.reflect())
            .or_insert(format!("f{}", num_functions))
            .clone()
    }

    pub fn register_parser(&mut self, name: &str) -> Option<Option<ast::Parser>> {
        let res = self.named_parsers.get(name).cloned();
        if let None = res {
            self.named_parsers.insert(name.to_owned(), None);
        }
        res.map(|op| op.map(|(p, t)| p))
    }

    pub fn insert_parser<P: Parsler>(&mut self, name: &str, parser: ast::Parser) {
        let p = self.named_parsers.get_mut(name);

        let type_ = std::any::type_name::<P::Output>();
        let type_ = syn::parse_str(type_).unwrap();

        match p {
            Some(Some(_)) => panic!("Parser with name '{}' already exists", name),
            None => panic!("Parser with name '{}' was never registered", name),
            Some(p) => *p = Some((parser, type_)),
        }
    }
}

pub fn compile<P: Parsler>(name: &str, parser: P, context: &mut CompileContext) -> ast::Statement {
    let type_ = std::any::type_name::<P::Output>();
    let type_ = syn::parse_str(type_).unwrap();

    let parser = parser.compile(context);
    Statement {
        public: true,
        ident: name.to_owned(),
        type_,
        parser,
    }
}

// pub fn gen_spec(spec: ast::Spec) -> TokenStream {
//     let statements = spec.statements.into_iter().map(gen_statement);
//     quote! {
//         #(#statements)*
//     }
// }

pub fn gen_statement(statement: ast::Statement, context: &CompileContext) -> TokenStream {
    // let public = statement.public;
    let ident = syn::Ident::new(&statement.ident, proc_macro2::Span::call_site());
    let type_ = statement.type_;
    let (parser, _) = gen_parser(statement.parser);

    let functions = context
        .functions
        .iter()
        .map(|(body, name)| {
            let mut body = syn::parse_str::<syn::ItemFn>(body).unwrap();

            body.sig.ident = syn::Ident::new(name, proc_macro2::Span::call_site());

            body
        })
        .collect::<Vec<_>>();
    let aux_parsers = context
        .named_parsers
        .iter()
        .map(|(name, parser)| {
            let (ts, ty) = match parser {
                None => panic!("Parser with name '{}' was never finished", name),
                Some((p, t)) => (gen_parser(p.clone()).0, t.clone()),
            };

            let ident = syn::Ident::new(name, proc_macro2::Span::call_site());
            quote! {
                fn #ident(input: &mut std::str::Chars) -> Result<#ty, &'static str> {
                    #ts
                }
            }
        })
        .collect::<Vec<_>>();
    quote! {
        extern crate alloc;
        #(#functions)*
        #(#aux_parsers)*
        pub fn #ident(input: &mut std::str::Chars) -> Result<#type_, &'static str> {
            #parser
        }
    }
}

fn gen_parser(parser: ast::Parser) -> (TokenStream, bool) {
    match parser {
        ast::Parser::Pure(pure_val) => gen_pure_val(pure_val),
        ast::Parser::Satisfy(ast::Func { name }) => gen_satisfy(&name),
        ast::Parser::Then(first, second) => gen_then(*first, *second),
        ast::Parser::Or(first, second) => gen_or(*first, *second),
        ast::Parser::Try(parser) => gen_try(*parser),
        ast::Parser::Empty => (quote! { Err("Expected empty") }, false),
        ast::Parser::Branch(first, second, third) => gen_branch(*first, *second, *third),
        ast::Parser::Look(parser) => gen_look(*parser),
        ast::Parser::NegLook(parser) => gen_neg_look(*parser),
        ast::Parser::Ap(first, second) => gen_ap(*first, *second),
        ast::Parser::Ident(name) => gen_ident(&name),
        ast::Parser::Before(first, second) => gen_before(*first, *second),
    }
}

fn gen_ident(name: &str) -> (TokenStream, bool) {
    let ident = syn::parse_str::<syn::Expr>(name).unwrap();
    (
        quote! {
            #ident(input)
        },
        false,
    )
}

fn gen_then(first: ast::Parser, second: ast::Parser) -> (TokenStream, bool) {
    let (first, _) = gen_parser(first);
    let (second, returns_func) = gen_parser(second);
    // let last = parsers.next().unwrap();
    (
        quote! {
            {#first}.and_then(|_| {#second})
        },
        returns_func,
    )
}

fn gen_before(first: ast::Parser, second: ast::Parser) -> (TokenStream, bool) {
    let (first, returns_func) = gen_parser(first);
    let (second, _) = gen_parser(second);
    (
        quote! {
            {#first}.and_then(|res| {#second}.map(|_| res))
        },
        returns_func,
    )
}

fn gen_ap(first: ast::Parser, second: ast::Parser) -> (TokenStream, bool) {
    let (first, _) = gen_parser(first);
    let (second, _) = gen_parser(second);
    (
        quote! {
            {#second}.and_then(|f| {#first}.map(|x| f(x)))
        },
        false,
    )
}

fn gen_neg_look(parser: ast::Parser) -> (TokenStream, bool) {
    let (parser, _) = gen_parser(parser);
    (
        quote! {
            let copied_input = input.clone();
            let result = {#parser};
            *input = copied_input;
            if result.is_ok() {
                Err("Expected negative look".to_owned())
            } else {
                Ok(())
            }
        },
        false,
    )
}

fn gen_look(parser: ast::Parser) -> (TokenStream, bool) {
    let (parser, returns_func) = gen_parser(parser);
    (
        quote! {
            let copied_input = input.clone();
            let result = {#parser};
            *input = copied_input;
            result
        },
        returns_func,
    )
}

fn gen_branch(b: ast::Parser, l: ast::Parser, r: ast::Parser) -> (TokenStream, bool) {
    let (b, _) = gen_parser(b);
    let (l, returns_funcl) = gen_parser(l);
    let (r, returns_funcr) = gen_parser(r);

    let l: TokenStream = if returns_funcl {
        quote! { {#l}.map(|f| f(l)) }
    } else {
        quote! { {#l} }
    };

    let r = if returns_funcr {
        quote! { {#r}.map(|f| f(r)) }
    } else {
        quote! { {#r} }
    };

    (
        quote! {
            {#b}.and_then(|b| match b {
                either::Either::Left(l) => #l,
                either::Either::Right(r) => #r,
            })
        },
        false,
    )
}

fn gen_try(parser: ast::Parser) -> (TokenStream, bool) {
    let (parser, returns_func) = gen_parser(parser);
    (
        quote! {
            let copied_input = input.clone();
            let result = {#parser};
            if result.is_err() {
                *input = copied_input;
            }
            result
        },
        returns_func,
    )
}

fn gen_pure_val(pure_val: ast::PureVal) -> (TokenStream, bool) {
    match pure_val {
        ast::PureVal::Val(val) => {
            let val = syn::parse_str::<syn::Expr>(&val).unwrap();
            (quote! { Ok(#val) }, false)
        }
        ast::PureVal::Func(ast::Func { name }) => {
            let ident = syn::parse_str::<syn::Ident>(&name).unwrap();
            (quote! { Ok(#ident) }, true)
        }
    }
}

fn gen_satisfy(ident: &str) -> (TokenStream, bool) {
    let ident = syn::parse_str::<syn::Expr>(ident).unwrap();
    (
        quote! {

            input.next().ok_or("Found EOF when character was expected").and_then(|c| if (#ident)(c) {
                Ok(c)
            } else {
                println!("satisfy: {}, {}", c, stringify!(#ident));
                Err("expected a specific character")
            })
        },
        false,
    )
}

fn gen_or(first: ast::Parser, second: ast::Parser) -> (TokenStream, bool) {
    let (first, returns_funcl) = gen_parser(first);
    let (second, returns_funcr) = gen_parser(second);
    (
        quote! {
            let result = {#first};
            if result.is_ok() {
                result
            } else {
                {#second}
            }
        },
        returns_funcl || returns_funcr,
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use quote::quote;

    #[test]
    fn test_gen_statement() {
        let statement = ast::Statement {
            public: false,
            ident: "a".to_owned(),
            type_: syn::parse_str("Result<(&str, &str), ()>").unwrap(),
            parser: ast::Parser::Pure(ast::PureVal::Val("\"a\"".to_owned())),
        };
        let expected = quote! {
            pub fn a(input: &str) -> Result<(&str , &str), ()> {
                Ok(("a", input))
            }
        }
        .to_string();
        // assert_eq!(gen_statement(statement).to_string(), expected);
    }
}
