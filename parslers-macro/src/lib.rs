#![feature(assert_matches)]
#![feature(box_patterns)]

extern crate proc_macro;

use quote::{quote, ToTokens};
use syn::{
    punctuated::Punctuated, Expr, ExprCall, ExprPath, Path, PathSegment, Stmt, Type, TypePath,
    TypeTraitObject,
};
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

// #[proc_macro]
// pub fn parser(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
//     let spec = parser::parse_spec(input.into());
//     let spec = codegen::gen_spec(spec);

//     spec.into()
// }

#[proc_macro_attribute]
pub fn reflect(
    arg: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    // let function = syn::parse::<syn::ItemFn>(item).unwrap();
    let function = syn::parse_macro_input!(item as syn::ItemFn);
    let function_name = function.sig.ident.clone();
    let function_arg_names = function
        .sig
        .inputs
        .clone()
        .into_iter()
        .map(|item| {
            if let syn::FnArg::Typed(pat_type) = item {
                *pat_type.pat
            } else {
                panic!("Expected typed argument")
            }
        })
        .collect::<Vec<_>>();
    let function_arg_types: Vec<_> = function
        .sig
        .inputs
        .clone()
        .into_iter()
        .map(|item| {
            if let syn::FnArg::Typed(pat_type) = item {
                *pat_type.ty
            } else {
                panic!("Expected typed argument")
            }
        })
        .collect();
    let function_arg_type_idents = function_arg_types
        .iter()
        .map(|t| t.to_token_stream().to_string())
        .collect::<Vec<_>>();
    let all_function_generics = function.sig.generics.clone();
    let _function_generics = function
        .sig
        .generics
        .clone()
        .params
        .into_iter()
        .filter_map(|x| match &x {
            syn::GenericParam::Type(t) => {
                if function_arg_type_idents.contains(&t.ident.to_string()) {
                    Some(x)
                } else {
                    None
                }
            }
            _ => None,
        })
        .collect::<Vec<_>>();
    let mut function_body = function.block.clone().stmts;
    let mut impl_ty = None;
    let function_output = match function.sig.output.clone() {
        syn::ReturnType::Default => quote! {()},
        syn::ReturnType::Type(_, ty) => {
            if let Type::Path(TypePath {
                qself: None,
                path:
                    Path {
                        leading_colon: None,
                        segments,
                    },
            }) = ty.clone().as_ref()
            {
                if let Some(PathSegment {
                    ident,
                    arguments: syn::PathArguments::AngleBracketed(args),
                }) = segments.first()
                {
                    if ident == "Box" {
                        if let Some(syn::GenericArgument::Type(Type::TraitObject(
                            TypeTraitObject {
                                dyn_token: Some(Dyn),
                                bounds,
                            },
                        ))) = args.args.first()
                        {
                            impl_ty = Some(quote! {
                                impl #bounds
                            })
                        }
                    }
                }
            }
            quote! {
                #ty
            }
        }
    };

    // [PathSegment {
    //     ident:
    //         Ident {
    //             ident: "Box",
    //             span: _,
    //         },
    //     arguments:
    //         PathArguments::AngleBracketed {
    //             colon2_token: None,
    //             lt_token: Lt,
    //             args:
    //                 [GenericArgument::Type(Type::TraitObject {
    //                     dyn_token: Some(Dyn),
    //                     bounds,
    //                 })],
    //             gt_token: Gt,
    //         },
    // }]

    let mut new_function_body = function_body.clone();

    if impl_ty.is_some() {
        if let Stmt::Expr(
            Expr::Call(ExprCall {
                attrs: _,
                func:
                    box Expr::Path(ExprPath {
                        attrs: _,
                        qself: None,
                        path:
                            Path {
                                leading_colon: None,
                                segments,
                            },
                    }),
                paren_token: Paren,
                args,
            }),
            None,
        ) = function_body.last().cloned().unwrap()
        {
            let mut iter = segments.iter();
            if let Some(PathSegment {
                ident,
                arguments: _,
            }) = iter.next()
            {
                if ident == "Box" && iter.next().unwrap().ident == "new" {
                    *new_function_body.last_mut().unwrap() =
                        Stmt::Expr(args.first().unwrap().clone(), None);
                }
            }
        }
    }

    let impl_ty = impl_ty.unwrap_or_else(|| quote! {#function_output});

    let new_function = if arg.to_string() == "unbox" {
        quote! {
            fn #function_name #all_function_generics(#(#function_arg_names: #function_arg_types,)*) -> #impl_ty {
                #(#new_function_body)*
            }
        }
    } else {
        quote! {#function}
    };

    quote! {
        #[allow(non_camel_case_types)]
        #[allow(incorrect_ident_case)]
        #[derive(Clone, Copy, Debug, Hash)]
        pub struct #function_name;

        impl #all_function_generics FnOnce<(#(#function_arg_types,)*)> for #function_name
        {
            type Output = #function_output;
            // Required method
            extern "rust-call" fn call_once(self, (#(#function_arg_names,)*): (#(#function_arg_types,)*)) -> Self::Output {
                #(#function_body)*
            }
        }

        // impl < #(#function_generics, )*> FnMut<(#(#function_arg_types,)*)> for #function_name
        // {
        //     // type Output = #function_output;
        //     // Required method
        //     extern "rust-call" fn call_mut(&mut self, (#(#function_arg_names,)*): (#(#function_arg_types,)*)) -> <Self as FnOnce<(#(#function_arg_types,)*)>>::Output {
        //         #(#function_body)*
        //     }
        // } 

        // impl < #(#function_generics, )*> Fn<(#(#function_arg_types,)*)> for #function_name
        // {
        //     // type Output = #function_output;
        //     // Required method
        //     extern "rust-call" fn call(&self, (#(#function_arg_names,)*): (#(#function_arg_types,)*)) -> <Self as FnOnce<(#(#function_arg_types,)*)>>::Output {
        //         #(#function_body)*
        //     }
        // }

        impl Reflect for #function_name {
            fn name(&self) -> &'static str {
                stringify!(#function_name)
            }
            fn reflect(&self) -> String {
                stringify!(#new_function).to_owned()
            }
        }
    }
    .into()
}

// mod combinators {
//     include!(concat!(env!("OUT_DIR"), "/combinators.rs"));
// }

// mod codegen {
//     use parslers_lib::ast;
//     use proc_macro2::TokenStream;
//     use quote::quote;
//     use syn::ExprClosure;

//     pub fn gen_spec(spec: ast::Spec) -> TokenStream {
//         let statements = spec.statements.into_iter().map(gen_statement);
//         quote! {
//             #(#statements)*
//         }
//     }

//     fn gen_statement(statement: ast::Statement) -> TokenStream {
//         // let public = statement.public;
//         let ident = syn::Ident::new(&statement.ident, proc_macro2::Span::call_site());
//         let type_ = statement.type_;
//         let parser = gen_parser(statement.parser);
//         quote! {
//             pub fn #ident(input: &str) -> Result<(#type_, &str), ()> {
//                 #parser
//             }
//         }
//     }

//     fn gen_parser(parser: ast::Parser) -> TokenStream {
//         match parser {
//             ast::Parser::Pure(pure_val) => gen_pure_val(pure_val),
//             ast::Parser::Satisfy(ast::Func { ident }) => gen_satisfy(&ident),
//             ast::Parser::Then(p1, p2) => gen_then(*p1, *p2),
//             _ => unimplemented!(),
//         }
//     }

//     fn gen_pure_val(pure_val: ast::PureVal) -> TokenStream {
//         match pure_val {
//             ast::PureVal::Val(val) => {
//                 let val = syn::parse_str::<syn::Expr>(&val).unwrap();
//                 quote! { Ok((#val, input)) }
//             }
//             ast::PureVal::Func(ast::Func { ident }) => {
//                 quote! { Ok((#ident, input)) }
//             }
//         }
//     }

//     fn gen_satisfy(ident: &str) -> TokenStream {
//         let ident = syn::Ident::new(ident, proc_macro2::Span::call_site());
//         quote! {

//                 let mut iter = input.chars();
//                 let c = iter.next().ok_or(())?;
//                 if (#ident)(c) {
//                     Ok((c, iter.as_str()))
//                 } else {
//                     Err(())
//                 }

//         }
//     }

//     fn gen_then(p1: ast::Parser, p2: ast::Parser) -> TokenStream {
//         // let mut parsers = parsers.into_iter().rev().map(gen_parser);
//         // let last = parsers.next().unwrap();
//         let p1 = gen_parser(p1);
//         let p2 = gen_parser(p2);
//         quote! {

//                 let (_, input) = {#p1}?;
//                 #p2

//         }
//     }
// }
