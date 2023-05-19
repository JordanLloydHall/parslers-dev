use std::collections::HashSet;

use proc_macro2::TokenStream;

use crate::code_gen::CompileContext;
use quote::quote;

#[derive(Debug, PartialEq, Clone)]
pub struct Spec {
    pub statements: Vec<Statement>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Statement {
    pub public: bool,
    pub ident: String,
    pub type_: syn::Type,
    pub parser: AnalysedParser,
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct AnalysedParser {
    pub output_used: bool,
    pub returns_func: bool,
    pub parser: Parser,
}

impl AnalysedParser {
    pub fn new(parser: Parser) -> Self {
        AnalysedParser {
            output_used: true,
            returns_func: true,
            parser,
        }
    }

    pub fn size(&self) -> usize {
        self.parser.size()
    }

    pub fn reduce(self) -> Self {
        AnalysedParser {
            output_used: self.output_used,
            returns_func: self.returns_func,
            parser: self.parser.reduce(),
        }
    }

    pub fn returns_func_analysis(&mut self) -> bool {
        let returns_func = self.parser.returns_func_analysis();

        self.returns_func = returns_func;

        returns_func
    }

    pub fn output_used_analysis(&mut self, used: bool, ctx: &mut HashSet<String>) {
        self.parser.output_used_analysis(used, ctx);

        self.output_used = used;
    }

    pub fn compile(&self) -> TokenStream {
        if self.output_used {
            self.parser.compile_used(self.returns_func)
        } else {
            self.parser.compile_unused(self.returns_func)
        }
    }
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Parser {
    Ident(String),
    Pure(PureVal),
    Satisfy(Func),
    Try(Box<AnalysedParser>),
    Look(Box<AnalysedParser>),
    NegLook(Box<AnalysedParser>),
    Ap(Box<AnalysedParser>, Box<AnalysedParser>),
    Then(Box<AnalysedParser>, Box<AnalysedParser>),
    Before(Box<AnalysedParser>, Box<AnalysedParser>),
    Or(Box<AnalysedParser>, Box<AnalysedParser>),
    Recognise(Box<AnalysedParser>),
    Empty,
    Branch(
        Box<AnalysedParser>,
        Box<AnalysedParser>,
        Box<AnalysedParser>,
    ),
    Loop(Box<AnalysedParser>, Box<AnalysedParser>),
}

impl Parser {
    pub fn size(&self) -> usize {
        match self {
            Parser::Pure(_) => 1,
            Parser::Satisfy(_) => 1,
            Parser::Try(p) => 1 + p.size(),
            Parser::Look(p) => 1 + p.size(),
            Parser::NegLook(p) => 1 + p.size(),
            Parser::Ap(p, q) => 1 + p.size() + q.size(),
            Parser::Then(p, q) => 1 + p.size() + q.size(),
            Parser::Before(p, q) => 1 + p.size() + q.size(),
            Parser::Or(p, q) => 1 + p.size() + q.size(),
            Parser::Empty => 1,
            Parser::Branch(p, q, r) => 1 + p.size() + q.size() + r.size(),
            Parser::Ident(_) => 1,
            Parser::Recognise(p) => 1 + p.size(),
            Parser::Loop(p0, pn) => 1 + p0.size() + pn.size(),
        }
    }

    pub fn reduce(self) -> Self {
        let p = match self {
            Parser::Pure(p) => Parser::Pure(p),
            Parser::Satisfy(s) => Parser::Satisfy(s),
            Parser::Try(box p) => Parser::Try(Box::new(p.reduce())),
            Parser::Look(box p) => Parser::Look(Box::new(p.reduce())),
            Parser::NegLook(box p) => Parser::NegLook(Box::new(p.reduce())),
            Parser::Ap(box p, box q) => Parser::Ap(Box::new(p.reduce()), Box::new(q.reduce())),
            Parser::Then(box p, box q) => Parser::Then(Box::new(p.reduce()), Box::new(q.reduce())),
            Parser::Before(box p, box q) => {
                Parser::Before(Box::new(p.reduce()), Box::new(q.reduce()))
            }
            Parser::Or(box p, box q) => Parser::Or(Box::new(p.reduce()), Box::new(q.reduce())),
            Parser::Empty => Parser::Empty,
            Parser::Branch(box p, box q, box r) => Parser::Branch(
                Box::new(p.reduce()),
                Box::new(q.reduce()),
                Box::new(r.reduce()),
            ),
            Parser::Ident(id) => Parser::Ident(id),
            Parser::Recognise(box p) => Parser::Recognise(Box::new(p.reduce())),
            Parser::Loop(box p0, box pn) => {
                Parser::Loop(Box::new(p0.reduce()), Box::new(pn.reduce()))
            }
        };

        // Special cases
        match p {
            Parser::Ap(
                box AnalysedParser {
                    parser: Parser::Pure(PureVal::Val(val)),
                    ..
                },
                box AnalysedParser {
                    parser: Parser::Pure(PureVal::Func(f)),
                    ..
                },
            ) => Parser::Pure(PureVal::Val(format!("{}({})", f.name, val))),
            Parser::Ap(
                box AnalysedParser {
                    parser: Parser::Pure(PureVal::Val(val)),
                    ..
                },
                p,
            ) => Parser::Ap(
                p,
                Box::new(AnalysedParser {
                    parser: Parser::Pure(PureVal::Val(format!("|f| f({})", val))),
                    output_used: true,
                    returns_func: true,
                }),
            ),
            Parser::Or(
                box AnalysedParser {
                    parser: Parser::Or(box p, box q),
                    ..
                },
                box r,
            ) => Parser::Or(
                Box::new(p.reduce()),
                Box::new(AnalysedParser {
                    parser: Parser::Or(Box::new(q.reduce()), Box::new(r.reduce())),
                    output_used: true,
                    returns_func: true,
                }),
            )
            .reduce(),
            Parser::Or(
                box AnalysedParser {
                    parser: Parser::Empty,
                    ..
                },
                box p,
            )
            | Parser::Or(
                box p,
                box AnalysedParser {
                    parser: Parser::Empty,
                    ..
                },
            ) => p.parser.reduce(),
            Parser::Ap(
                _,
                box AnalysedParser {
                    parser: Parser::Empty,
                    ..
                },
            ) => Parser::Empty,
            Parser::Or(
                box AnalysedParser {
                    parser: Parser::Pure(x),
                    ..
                },
                _,
            ) => Parser::Pure(x),
            // Parser::Try(box Parser::Satisfy(f)) => Parser::Satisfy(f),
            Parser::Try(box AnalysedParser {
                parser: Parser::Pure(x),
                ..
            }) => Parser::Pure(x),
            Parser::Look(box AnalysedParser {
                parser: Parser::Empty,
                ..
            }) => Parser::Empty,
            // Parser::Ap(box Parser::Ap(w, v), u) => Parser::Ap(w, Box::new(Parser::Ap(v, Box::new(Parser::Ap(u, Box::new(Parser::Pure()))))))
            _ => p,
        }
    }

    pub fn returns_func_analysis(&mut self) -> bool {
        match self {
            Parser::Ident(f) => true,
            Parser::Pure(PureVal::Func(_)) => true,
            Parser::Pure(PureVal::Val(_)) => false,
            Parser::Satisfy(_) => false,
            Parser::Try(p) => p.returns_func_analysis(),
            Parser::Look(p) => p.returns_func_analysis(),
            Parser::NegLook(p) => {
                p.returns_func_analysis();
                false
            }
            Parser::Ap(p, q) => {
                p.returns_func_analysis();
                q.returns_func_analysis();
                true
            }
            Parser::Then(q, p) => {
                q.returns_func_analysis();
                p.returns_func_analysis()
            }
            Parser::Before(p, q) => {
                q.returns_func_analysis();
                p.returns_func_analysis()
            }
            Parser::Or(p, q) => q.returns_func_analysis() & p.returns_func_analysis(),
            Parser::Recognise(p) => {
                p.returns_func_analysis();
                false
            }
            Parser::Empty => false,
            Parser::Branch(b, l, r) => {
                b.returns_func_analysis();
                l.returns_func_analysis() & r.returns_func_analysis()
            }
            Parser::Loop(p0, pn) => {
                p0.returns_func_analysis();
                pn.returns_func_analysis();
                false
            }
        }
    }

    fn output_used_analysis(&mut self, used: bool, ctx: &mut HashSet<String>) {
        match self {
            // Parser::Ident(_) => todo!(),
            // Parser::Pure(_) => todo!(),
            // Parser::Satisfy(_) => todo!(),
            Parser::Try(p) => p.output_used_analysis(used, ctx),
            Parser::Look(p) => p.output_used_analysis(used, ctx),
            Parser::NegLook(p) => p.output_used_analysis(used, ctx),
            Parser::Ap(p, q) => {
                p.output_used_analysis(used, ctx);
                q.output_used_analysis(used, ctx);
            }
            Parser::Then(p, q) => {
                p.output_used_analysis(false, ctx);
                q.output_used_analysis(used, ctx);
            }
            Parser::Before(p, q) => {
                p.output_used_analysis(used, ctx);
                q.output_used_analysis(false, ctx);
            }
            Parser::Or(p, q) => {
                p.output_used_analysis(used, ctx);
                q.output_used_analysis(used, ctx);
            }
            Parser::Recognise(p) => {
                p.output_used_analysis(false, ctx);
            }
            // Parser::Empty => todo!(),
            Parser::Branch(b, l, r) => {
                b.output_used_analysis(true, ctx);
                l.output_used_analysis(used, ctx);
                r.output_used_analysis(used, ctx);
            }
            Parser::Ident(id) => {
                if !used {
                    if !ctx.contains(id.strip_suffix("_unused").unwrap_or(id)) {
                        let new_name = format!("{}_unused", id);
                        ctx.insert(id.clone());
                        *id = new_name;
                    }
                }
            }
            Parser::Loop(p0, pn) => {
                p0.output_used_analysis(used, ctx);
                pn.output_used_analysis(used, ctx);
            }
            _ => {}
        }
    }

    pub fn compile_used(&self, returns_func: bool) -> TokenStream {
        match self {
            Parser::Ident(s) => {
                let ident = syn::parse_str::<syn::Expr>(s).unwrap();
                quote! {
                    #ident(input)
                }
            }
            Parser::Pure(pure_val) => match pure_val {
                PureVal::Val(val) => {
                    let val = syn::parse_str::<syn::Expr>(&val).unwrap();
                    quote! { Ok(#val) }
                }
                PureVal::Func(Func { name }) => {
                    let ident = syn::parse_str::<syn::Ident>(&name).unwrap();
                    quote! { Ok(#ident) }
                }
            },
            Parser::Satisfy(ident) => {
                let ident = syn::parse_str::<syn::Expr>(&ident.name).unwrap();
                quote! {

                    input.next().ok_or("Found EOF when character was expected").and_then(|c| if (#ident)(c) {
                        Ok(c)
                    } else {
                        Err("expected a specific character")
                    })
                }
            }
            Parser::Try(p) => {
                let parser = p.compile();
                quote! {
                    let copied_input = input.clone();
                    let result = {#parser};
                    if result.is_err() {
                        *input = copied_input;
                    }
                    result
                }
            }
            Parser::Look(p) => {
                let parser = p.compile();
                quote! {
                    let copied_input = input.clone();
                    let result = {#parser};
                    *input = copied_input;
                    result
                }
            }
            Parser::NegLook(p) => {
                let p = p.compile();
                quote! {
                    let copied_input = input.clone();
                    let result = {#p};
                    *input = copied_input;
                    if result.is_ok() {
                        Err("Expected negative look".to_owned())
                    } else {
                        Ok(())
                    }
                }
            }
            Parser::Ap(p, q) => {
                let p = p.compile();
                let q = q.compile();

                quote! {
                    {#q}.and_then(|f| {#p}.map(|x| f(x)))
                }
            }
            Parser::Then(p, q) => {
                let p = p.compile();
                let q = q.compile();

                quote! {
                    {#p}.and_then(|_| {#q})
                }
            }
            Parser::Before(p, q) => {
                let p = p.compile();
                let q = q.compile();

                quote! {
                    {#p}.and_then(|res| {#q}.map(|_| res))
                }
            }
            Parser::Or(p, q) => {
                let p = p.compile();
                let q = q.compile();

                quote! {
                    {#p}.or_else(|_| {#q})
                }
            }
            Parser::Recognise(p) => {
                let parser = p.compile();

                quote! {
                    let start = input.clone().as_str();
                    {#parser}.map(|_| start[..start.len() - input.as_str().len()].to_owned())
                }
            }
            Parser::Empty => quote! { Err("Expected empty") },
            Parser::Branch(b, l, r) => {
                let (b1) = b.compile();
                let (l1) = l.compile();
                let (r1) = r.compile();

                let l: TokenStream = if l.returns_func {
                    quote! { {#l1}.map(|f| f(l)) }
                } else {
                    quote! { {#l1} }
                };

                let r = if r.returns_func {
                    quote! { {#r1}.map(|f| f(r1)) }
                } else {
                    quote! { {#r1} }
                };

                quote! {
                    {#b1}.and_then(|b| match b {
                        either::Either::Left(l) => #l,
                        either::Either::Right(r) => #r,
                    })
                }
            }
            Parser::Loop(p0, pn) => {
                let p0 = p0.compile();
                let pn = pn.compile();
                quote! {
                    {#p0}.map(|mut v| {
                        while let Ok(x) = {#pn} {
                            v.push(x);
                        }
                        v
                    })
                }
            }
        }
    }

    pub fn compile_unused(&self, returns_func: bool) -> TokenStream {
        match self {
            Parser::Ident(s) => {
                let ident = syn::parse_str::<syn::Expr>(s).unwrap();
                quote! {
                    #ident(input)
                }
            }
            Parser::Pure(pure_val) => match pure_val {
                PureVal::Val(val) => {
                    quote! { Ok(()) }
                }
                PureVal::Func(Func { name }) => {
                    quote! { Ok(()) }
                }
            },
            Parser::Satisfy(ident) => {
                let ident = syn::parse_str::<syn::Expr>(&ident.name).unwrap();
                quote! {

                    input.next().ok_or("Found EOF when character was expected").and_then(|c| if (#ident)(c) {
                        Ok(())
                    } else {
                        Err("expected a specific character")
                    })
                }
            }
            Parser::Try(p) => {
                let parser = p.compile();
                quote! {
                    let copied_input = input.clone();
                    let result = {#parser};
                    if result.is_err() {
                        *input = copied_input;
                    }
                    result
                }
            }
            Parser::Look(p) => {
                let parser = p.compile();
                quote! {
                    let copied_input = input.clone();
                    let result = {#parser};
                    *input = copied_input;
                    result
                }
            }
            Parser::NegLook(p) => {
                let p = p.compile();
                quote! {
                    let copied_input = input.clone();
                    let result = {#p};
                    *input = copied_input;
                    if result.is_ok() {
                        Err("Expected negative look".to_owned())
                    } else {
                        Ok(())
                    }
                }
            }
            Parser::Ap(p, q) => {
                let p = p.compile();
                let q = q.compile();

                quote! {
                    {#q}.and_then(|_| {#p})
                }
            }
            Parser::Then(p, q) => {
                let p = p.compile();
                let q = q.compile();

                quote! {
                    {#p}.and_then(|_| {#q})
                }
            }
            Parser::Before(p, q) => {
                let p = p.compile();
                let q = q.compile();

                quote! {
                    {#p}.and_then(|_| {#q})
                }
            }
            Parser::Or(p, q) => {
                let p = p.compile();
                let q = q.compile();

                quote! {
                    {#p}.or_else(|_| {#q})
                }
            }
            Parser::Recognise(p) => {
                let parser = p.compile();

                quote! {
                    {#parser}
                }
            }
            Parser::Empty => quote! { Err("Expected empty") },
            Parser::Branch(b, l, r) => {
                let (b1) = b.compile();
                let (l1) = l.compile();
                let (r1) = r.compile();

                quote! {
                    {#b1}.and_then(|b| match b {
                        either::Either::Left(l) => {#l1},
                        either::Either::Right(r) => {#r1},
                    })
                }
            }
            Parser::Loop(p0, pn) => {
                let p0 = p0.compile();
                let pn = pn.compile();
                quote! {
                    {#p0}.map(|v| {
                        while let Ok(x) = {#pn} {}
                        ()
                    })
                }
            }
        }
    }
}

// pub struct AnalysedParser {
//     pub output_used: bool,
//     pub parser: Box<AnalysedParser>,
// }

// fn usage_optimisation(p: Parser, used: bool) -> AnalysedParser {
//     match p {
//         // Parser::Ident(_) => todo!(),
//         // Parser::Pure(_) => todo!(),
//         // Parser::Satisfy(_) => todo!(),
//         Parser::Try(_) => todo!(),
//         Parser::Look(_) => todo!(),
//         Parser::NegLook(_) => todo!(),
//         Parser::Ap(_, _) => todo!(),
//         Parser::Then(box p, box p) => AnalysedParser { output_used: used, parser: Box::new(Parser::Then((), ())) }
//         Parser::Before(_, _) => todo!(),
//         Parser::Or(_, _) => todo!(),
//         Parser::Recognise(p) => todo!(),
//         // Parser::Empty => todo!(),
//         // Parser::Branch(_, _, _) => todo!(),
//     }
// }

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum PureVal {
    Val(String),
    Func(Func),
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct Func {
    pub name: String,
}
