use std::{any::Any, collections::HashMap, hash::Hasher, marker::PhantomData, rc::Rc, str::Chars};

use std::hash::Hash;

use either::Either;
use parslers_macro::reflect;

use crate::{
    ast::{self, Spec},
    code_gen::CompileContext,
    reflect::Reflect,
};

impl std::hash::Hash for dyn DynHashable {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        // let unboxed_ref = &(*self) as &dyn Parsler<Output = T>;
        self.dyn_hash(state)
    }
}

trait DynHashable {
    fn dyn_hash(&self, hasher: &mut dyn Hasher);
}

impl<H> DynHashable for H
where
    H: Hash,
{
    fn dyn_hash(&self, hasher: &mut dyn Hasher) {
        self.hash(hasher);
    }
}

pub trait Parsler: DynHashable {
    type Output;

    fn parse(&self, input: &mut Chars) -> Result<Self::Output, String>;
    fn compile(&self, info: &mut CompileContext) -> ast::Parser;

    fn ap<B, F, P>(self, p: P) -> Ap<B, F, Self, P>
    where
        Self: Sized,
        P: Parsler<Output = F>,
        F: FnOnce(Self::Output) -> B,
    {
        Ap(self, p)
    }

    fn map<B, F>(self, f: F) -> Map<B, Self, F>
    where
        Self: Sized,
        F: FnOnce(Self::Output) -> B + Clone + Reflect,
    {
        Map(self, f)
    }

    fn then<P>(self, p: P) -> Then<Self, P>
    where
        Self: Sized,
        P: Parsler,
    {
        Then(self, p)
    }

    fn or<P>(self, p: P) -> Or<Self, P>
    where
        Self: Sized,
        P: Parsler<Output = Self::Output>,
    {
        Or(self, p)
    }

    fn before<P>(self, p: P) -> Before<Self, P>
    where
        Self: Sized,
        P: Parsler,
    {
        Before(self, p)
    }

    fn attempt(self) -> Attempt<Self>
    where
        Self: Sized,
    {
        Attempt(self)
    }

    fn look(self) -> Look<Self>
    where
        Self: Sized,
    {
        Look(self)
    }

    fn neg_look(self) -> NegLook<Self>
    where
        Self: Sized,
    {
        NegLook(self)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct LazyParser<P: Parsler, F: FnOnce() -> P + Clone>(std::cell::OnceCell<P>, F);

impl<P: Parsler, F: FnOnce() -> P + Clone> LazyParser<P, F> {
    pub fn new(f: F) -> Self {
        Self(std::cell::OnceCell::new(), f)
    }
}

impl<P: Parsler + Hash, F: FnOnce() -> P + Clone + Hash> Parsler for LazyParser<P, F> {
    type Output = P::Output;

    fn parse(&self, input: &mut Chars) -> Result<Self::Output, String> {
        // println!("Here we are! {}", input.as_str());
        self.0.get_or_init(self.1.clone()).parse(input)
    }

    fn compile(&self, info: &mut CompileContext) -> ast::Parser {
        self.0.get_or_init(self.1.clone()).compile(info)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Satisfy<F: FnOnce(char) -> bool + Reflect + Clone>(pub F);

impl<F: FnOnce(char) -> bool + Reflect + Clone + Hash> Parsler for Satisfy<F> {
    type Output = char;

    fn parse(&self, input: &mut Chars) -> Result<Self::Output, String> {
        if let Some(c) = input.next() {
            if self.0.clone()(c) {
                return Ok(c);
            }
        }
        Err(format!("Expected satisfy {:?}", self.0.clone()))
    }

    fn compile(&self, info: &mut CompileContext) -> ast::Parser {
        let name = info.add_function(&self.0);

        ast::Parser::Satisfy(ast::Func { name })
    }
}

#[derive(Debug)]
pub struct Ap<B, F: FnOnce(P1::Output) -> B, P1: Parsler, P2: Parsler<Output = F>>(pub P1, pub P2);

impl<B, F: FnOnce(P1::Output) -> B, P1: Parsler + Clone, P2: Parsler<Output = F> + Clone> Clone
    for Ap<B, F, P1, P2>
{
    fn clone(&self) -> Self {
        Self(self.0.clone(), self.1.clone())
    }
}

impl<B, F: FnOnce(P1::Output) -> B, P1: Parsler, P2: Parsler<Output = F>> Hash
    for Ap<B, F, P1, P2>
{
}

impl<B, F: FnOnce(P1::Output) -> B, P1: Parsler, P2: Parsler<Output = F>> Parsler
    for Ap<B, F, P1, P2>
{
    type Output = B;
    fn parse(&self, input: &mut Chars) -> Result<Self::Output, String> {
        let f = self.1.parse(input)?;
        let a = self.0.parse(input)?;
        Ok(f(a))
    }

    fn compile(&self, info: &mut CompileContext) -> ast::Parser {
        let left = self.0.compile(info);
        let right = self.1.compile(info);

        ast::Parser::Ap(Box::new(left), Box::new(right))
    }
}

#[derive(Debug)]
pub struct Map<B, P1: Parsler, F: FnOnce(P1::Output) -> B + Clone + Reflect>(pub P1, pub F);

impl<B, P1: Parsler + Clone, F: FnOnce(P1::Output) -> B + Clone + Reflect> Clone for Map<B, P1, F> {
    fn clone(&self) -> Self {
        Self(self.0.clone(), self.1.clone())
    }
}

impl<B, P1: Parsler, F: FnOnce(P1::Output) -> B + Clone + Reflect> Parsler for Map<B, P1, F> {
    type Output = B;
    fn parse(&self, input: &mut Chars) -> Result<Self::Output, String> {
        let a = self.0.parse(input)?;
        Ok(self.1.clone()(a))
    }

    fn compile(&self, info: &mut CompileContext) -> ast::Parser {
        let name = info.add_function(&self.1);
        let p = self.0.compile(info);

        ast::Parser::Ap(
            Box::new(p),
            Box::new(ast::Parser::Pure(ast::PureVal::Func(ast::Func { name }))),
        )
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Then<P1: Parsler, P2: Parsler>(pub P1, pub P2);

impl<P1: Parsler, P2: Parsler> Parsler for Then<P1, P2> {
    type Output = P2::Output;
    fn parse(&self, input: &mut Chars) -> Result<Self::Output, String> {
        let _ = self.0.parse(input)?;
        let b = self.1.parse(input)?;
        Ok(b)
    }

    fn compile(&self, info: &mut CompileContext) -> ast::Parser {
        let left = self.0.compile(info);
        let right = self.1.compile(info);

        ast::Parser::Then(Box::new(left), Box::new(right))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Before<P1: Parsler, P2: Parsler>(pub P1, pub P2);

impl<P1: Parsler, P2: Parsler> Parsler for Before<P1, P2> {
    type Output = P1::Output;
    fn parse(&self, input: &mut Chars) -> Result<Self::Output, String> {
        let a = self.0.parse(input)?;
        let _ = self.1.parse(input)?;
        Ok(a)
    }

    fn compile(&self, info: &mut CompileContext) -> ast::Parser {
        let left = self.0.compile(info);
        let right = self.1.compile(info);

        ast::Parser::Before(Box::new(left), Box::new(right))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Or<P1: Parsler, P2: Parsler<Output = P1::Output>>(pub P1, pub P2);

impl<P1: Parsler, P2: Parsler<Output = P1::Output>> Parsler for Or<P1, P2> {
    type Output = P1::Output;
    fn parse(&self, input: &mut Chars) -> Result<Self::Output, String> {
        match self.0.parse(input) {
            Err(_) => self.1.parse(input),
            other => other,
        }
    }

    fn compile(&self, info: &mut CompileContext) -> ast::Parser {
        let left = self.0.compile(info);
        let right = self.1.compile(info);

        ast::Parser::Or(Box::new(left), Box::new(right))
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Branch<
    L,
    R,
    O,
    F1: FnOnce(L) -> O + Reflect,
    F2: FnOnce(R) -> O + Reflect,
    P1: Parsler<Output = Either<L, R>>,
    P2: Parsler<Output = F1>,
    P3: Parsler<Output = F2>,
>(pub P1, pub P2, pub P3);

impl<
        L,
        R,
        O,
        F1: FnOnce(L) -> O + Reflect,
        F2: FnOnce(R) -> O + Reflect,
        P1: Parsler<Output = Either<L, R>> + Clone,
        P2: Parsler<Output = F1> + Clone,
        P3: Parsler<Output = F2> + Clone,
    > Clone for Branch<L, R, O, F1, F2, P1, P2, P3>
{
    fn clone(&self) -> Self {
        Branch(self.0.clone(), self.1.clone(), self.2.clone())
    }
}

impl<
        L,
        R,
        O,
        F1: FnOnce(L) -> O + Reflect,
        F2: FnOnce(R) -> O + Reflect,
        P1: Parsler<Output = Either<L, R>>,
        P2: Parsler<Output = F1>,
        P3: Parsler<Output = F2>,
    > Parsler for Branch<L, R, O, F1, F2, P1, P2, P3>
{
    type Output = O;
    fn parse(&self, input: &mut Chars) -> Result<Self::Output, String> {
        match self.0.parse(input)? {
            Either::Left(l) => self.1.parse(input).map(|f| f(l)),
            Either::Right(r) => self.2.parse(input).map(|f| f(r)),
        }
    }

    fn compile(&self, info: &mut CompileContext) -> ast::Parser {
        let branch = self.0.compile(info);
        let left = self.1.compile(info);
        let right = self.2.compile(info);

        ast::Parser::Branch(Box::new(branch), Box::new(left), Box::new(right))
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Empty<O>(pub PhantomData<O>);

impl<O> Clone for Empty<O> {
    fn clone(&self) -> Self {
        Empty(PhantomData)
    }
}

impl<O> Default for Empty<O> {
    fn default() -> Self {
        Empty(PhantomData)
    }
}

impl<O> Parsler for Empty<O> {
    type Output = O;
    fn parse(&self, _input: &mut Chars) -> Result<Self::Output, String> {
        Err("Expected empty".to_owned())
    }

    fn compile(&self, _info: &mut CompileContext) -> ast::Parser {
        ast::Parser::Empty
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Attempt<P1: Parsler>(pub P1);

impl<P1: Parsler> Parsler for Attempt<P1> {
    type Output = P1::Output;
    fn parse(&self, input: &mut Chars) -> Result<Self::Output, String> {
        let copied_input = input.clone();
        let a = self.0.parse(input);
        if a.is_err() {
            *input = copied_input;
        }
        a
    }

    fn compile(&self, info: &mut CompileContext) -> ast::Parser {
        let p = self.0.compile(info);
        ast::Parser::Try(Box::new(p))
    }
}

pub struct Look<P1: Parsler>(pub P1);

impl<P1: Parsler> Parsler for Look<P1> {
    type Output = P1::Output;
    fn parse(&self, input: &mut Chars) -> Result<Self::Output, String> {
        let a = self.0.parse(input)?;
        Ok(a)
    }

    fn compile(&self, info: &mut CompileContext) -> ast::Parser {
        let p = self.0.compile(info);
        ast::Parser::Look(Box::new(p))
    }
}

pub struct NegLook<P1: Parsler>(pub P1);

impl<P1: Parsler<Output = ()>> Parsler for NegLook<P1> {
    type Output = ();
    fn parse(&self, input: &mut Chars) -> Result<Self::Output, String> {
        match self.0.parse(input) {
            Ok(_) => Err("Expected neg look".to_owned()),
            Err(_) => Ok(()),
        }
    }

    fn compile(&self, info: &mut CompileContext) -> ast::Parser {
        let p = self.0.compile(info);
        ast::Parser::NegLook(Box::new(p))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Pure<A: Reflect>(pub A);

impl<A: Clone + Reflect> Parsler for Pure<A> {
    type Output = A;
    fn parse(&self, _input: &mut Chars) -> Result<Self::Output, String> {
        let val = self.0.clone();
        Ok(val)
    }

    fn compile(&self, info: &mut CompileContext) -> ast::Parser {
        if let Ok(_) = syn::parse_str::<syn::ItemFn>(&self.0.reflect()) {
            let name = info.add_function(&self.0);
            ast::Parser::Pure(ast::PureVal::Func(ast::Func { name }))
        } else {
            ast::Parser::Pure(ast::PureVal::Val(self.0.reflect()))
        }
    }
}

pub fn pure<A: Reflect>(a: A) -> Pure<A> {
    Pure(a)
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NamedParser<P: Parsler>(pub P, pub String);

impl<P: Parsler> Parsler for NamedParser<P> {
    type Output = P::Output;

    fn parse(&self, input: &mut Chars) -> Result<Self::Output, String> {
        self.0.parse(input)
    }

    fn compile(&self, info: &mut CompileContext) -> ast::Parser {
        match info.register_parser(&self.1) {
            Some(_) => ast::Parser::Ident(self.1.clone()),
            None => {
                let p = self.0.compile(info);
                info.insert_parser::<P>(&self.1, p.clone());
                p
            }
        }
    }
}

impl<P: Parsler + ?Sized> Parsler for Box<P> {
    type Output = P::Output;
    fn parse(&self, input: &mut Chars) -> Result<Self::Output, String> {
        self.as_ref().parse(input)
    }

    fn compile(&self, info: &mut CompileContext) -> ast::Parser {
        self.as_ref().compile(info)
    }
}

impl<P: Parsler + ?Sized> Parsler for Rc<P> {
    type Output = P::Output;
    fn parse(&self, input: &mut Chars) -> Result<Self::Output, String> {
        self.as_ref().parse(input)
    }

    fn compile(&self, info: &mut CompileContext) -> ast::Parser {
        self.as_ref().compile(info)
    }
}

#[reflect]
pub fn cons<A: 'static>(a: A) -> Box<dyn FnOnce(Vec<A>) -> Vec<A>> {
    Box::new(move |mut v| {
        v.push(a);
        v
    })
}

#[reflect]
fn append<A: 'static>(mut v: Vec<A>) -> Box<dyn FnOnce(A) -> Vec<A>> {
    Box::new(move |a| {
        v.push(a);
        v
    })
}

#[reflect]
fn singleton<A>(a: A) -> Vec<A> {
    vec![a]
}

#[reflect]
fn concat<A: 'static>(mut v: Vec<A>) -> Box<dyn FnOnce(Vec<A>) -> Vec<A>> {
    Box::new(move |mut a| {
        v.append(&mut a);
        v
    })
}

#[reflect]
fn reverse<A>(v: Vec<A>) -> Vec<A> {
    v.into_iter().rev().collect()
}

pub fn some_rev<P: Parsler + Clone + 'static>(p: P) -> impl Parsler<Output = Vec<P::Output>> + Clone
where
    P::Output: Clone + 'static + Reflect,
{
    let clone_p = p.clone();
    NamedParser(
        LazyParser::new(|| many_rev(p)).ap(clone_p.map(cons)),
        "some_rev".to_owned(),
    )
    // .map(reverse)
}

pub fn many_rev<P: Parsler + Clone + 'static>(p: P) -> impl Parsler<Output = Vec<P::Output>> + Clone
where
    P::Output: Clone + 'static + Reflect,
{
    NamedParser(
        LazyParser::new(|| Rc::new(some_rev(p)) as Rc<dyn Parsler<Output = Vec<P::Output>>>)
            .or(pure(vec![])),
        "many_rev".to_owned(),
    )
}

pub fn many<P: Parsler + Clone + 'static>(p: P) -> impl Parsler<Output = Vec<P::Output>> + Clone
where
    P::Output: Clone + 'static + Reflect,
{
    many_rev(p).map(reverse)
}

pub fn some<P: Parsler + Clone + 'static>(p: P) -> impl Parsler<Output = Vec<P::Output>> + Clone
where
    P::Output: Clone + 'static + Reflect,
{
    some_rev(p).map(reverse)
}

pub fn match_char(c: char) -> impl Parsler<Output = char> + Clone {
    #[derive(Copy, Clone, Debug)]
    struct CharMatch(char);
    impl Reflect for CharMatch {
        fn name(&self) -> &'static str {
            "match_char"
        }
        fn reflect(&self) -> String {
            format!("fn match_char(a: char) -> bool {{ a == '{}'}}", self.0)
        }
    }

    impl FnOnce<(char,)> for CharMatch {
        type Output = bool;

        extern "rust-call" fn call_once(self, (c,): (char,)) -> Self::Output {
            // println!("{} == {}", c, self.0);
            c == self.0
        }
    }

    Satisfy(CharMatch(c)).then(pure(c))
}

pub fn one_of(chars: impl IntoIterator<Item = char>) -> std::rc::Rc<dyn Parsler<Output = char>> {
    let mut iter = chars
        .into_iter()
        .map(|c| Rc::new(match_char(c).attempt()) as Rc<dyn Parsler<Output = char>>);
    let first = iter.next().unwrap();
    iter.fold(first, |a, b| Rc::new(a.or(b)))
}

#[reflect]
fn id<A>(a: A) -> A {
    a
}

struct emty<A>(PhantomData<A>);

impl<A> FnOnce<((),)> for emty<A> {
    type Output = A;
    extern "rust-call" fn call_once(self, _: ((),)) -> Self::Output {
        panic!("emty")
    }
}

impl<A> Reflect for emty<A> {
    fn name(&self) -> &'static str {
        "emty"
    }
    fn reflect(&self) -> String {
        format!(
            "fn emty(a: ()) -> {} {{ panic!(\"emty\") }}",
            std::any::type_name::<A>()
        )
    }
}

pub fn filtered_by<P: Parsler + Clone, F: FnOnce(&P::Output) -> bool + Reflect + Clone>(
    p: P,
    f: F,
) -> impl Parsler<Output = P::Output> + Clone {
    struct Cond<O, F>(F, PhantomData<O>);

    impl<O, F: Clone> Clone for Cond<O, F> {
        fn clone(&self) -> Self {
            Cond(self.0.clone(), PhantomData)
        }
    }

    impl<I, F: FnOnce(&I) -> bool + Reflect> Reflect for Cond<I, F> {
        fn reflect(&self) -> String {
            format!(
                "
                fn cond(x: {}) -> either::Either<(), {}> {{
                    {}
                    if {{{}}}(&x) {{
                        either::Either::Right(x)
                    }} else {{
                        either::Either::Left(())
                    }}
                }}
                ",
                std::any::type_name::<I>(),
                std::any::type_name::<I>(),
                self.0.reflect(),
                self.0.name().split("::").last().unwrap(),
            )
            .to_owned()
        }
    }

    impl<O, F: FnOnce(&O) -> bool + Reflect> FnOnce<(O,)> for Cond<O, F> {
        type Output = either::Either<(), O>;

        extern "rust-call" fn call_once(self, (a,): (O,)) -> Self::Output {
            if (self.0)(&a) {
                either::Either::Right(a)
            } else {
                either::Either::Left(())
            }
        }
    }

    Branch(
        Ap(p, pure(Cond(f, PhantomData))),
        Empty::<emty<P::Output>>::default(),
        pure(id),
    )
}

#[reflect]
pub fn collect_string(chars: Vec<char>) -> String {
    chars.into_iter().collect()
}

pub fn ident(
    is_not_keyword: impl FnOnce(&String) -> bool + Reflect + Clone,
) -> impl Parsler<Output = String> {
    filtered_by(some(one_of('a'..='z')).map(collect_string), is_not_keyword)
}

pub fn tag(s: &str) -> impl Parsler<Output = &str> + Clone {
    let mut iter = s
        .chars()
        .map(|c| Rc::new(match_char(c)) as Rc<dyn Parsler<Output = char>>);

    let first = iter.next().unwrap();
    iter.fold(first, |a, b| Rc::new(a.then(b))).then(pure(s))
}

pub fn not(c: char) -> impl Parsler<Output = char> + Clone {
    #[derive(Copy, Clone, Debug)]
    struct CharNot(char);
    impl Reflect for CharNot {
        fn name(&self) -> &'static str {
            "not"
        }
        fn reflect(&self) -> String {
            format!("fn not(a: char) -> bool {{ a != '{}'}}", self.0)
        }
    }

    impl FnOnce<(char,)> for CharNot {
        type Output = bool;

        extern "rust-call" fn call_once(self, (c,): (char,)) -> Self::Output {
            c != self.0
        }
    }

    Satisfy(CharNot(c))
}

pub fn ws() -> impl Parsler<Output = Vec<char>> + Clone {
    some(one_of(" \t\n\r".chars()))
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn ident_works_correctly() {
        #[reflect]
        fn is_not_keyword(s: &String) -> bool {
            s != "abc"
        }
        let mut input = "abcd".chars();
        let result = ident(is_not_keyword).parse(&mut input);
        assert_eq!(result, Ok("abcd".to_owned()));
    }
}
