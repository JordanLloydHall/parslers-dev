#![feature(assert_matches)]
#![feature(box_patterns)]

extern crate proc_macro;

use quote::{quote, ToTokens};
use syn::{Expr, ExprCall, ExprPath, Path, PathSegment, Stmt, Type, TypePath, TypeTraitObject};

#[proc_macro_attribute]
pub fn reflect(
    arg: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
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
    let function_body = function.block.clone().stmts;
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
                                dyn_token: Some(_),
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
                paren_token: _,
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
