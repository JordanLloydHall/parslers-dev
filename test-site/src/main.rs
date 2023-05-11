#![feature(unboxed_closures, fn_traits, type_name_of_val, lazy_cell)]
use std::{collections::HashMap, str::Chars, rc::Rc};

use either::Either;
use parslers_lib::ast::{self, Spec};
use parslers_macro::{combinator};

trait Reflect {
    fn name(&self) -> &'static str {
        std::any::type_name::<Self>()
    }

    fn to_string(&self) -> String;
}

#[combinator]
fn is_a(c: char) -> bool {
    c == 'a'
}

#[combinator]
fn ident<A>(a: A) -> A {
    a
}

#[combinator]
fn return_ident<A>(_a: A) -> ident {
    ident
}

struct ParlserInfo {
    parslers: Spec,
    functions: HashMap<&'static str, Box<dyn Reflect>>,
}

fn match_char(c: char) -> impl Parsler<Output = char> + Clone + std::fmt::Debug {
    #[derive(Copy, Clone, Debug)]
    struct CharMatch(char);
    impl Reflect for CharMatch {
        fn to_string(&self) -> String {
            format!("fn (a: char) {{ a == {}}}", self.0)
        }
    }

    impl FnOnce<(char,)> for CharMatch {
        type Output = bool;

        extern "rust-call" fn call_once(self, (c,): (char,)) -> Self::Output {
            c == self.0
        }
    }

    impl FnMut<(char,)> for CharMatch {
        extern "rust-call" fn call_mut(&mut self, (c,): (char,)) -> Self::Output {
            c == self.0
        }
    }

    impl Fn<(char,)> for CharMatch {
        extern "rust-call" fn call(&self, (c,): (char,)) -> Self::Output {
            c == self.0
        }
    }

    Satisfy(CharMatch(c)).then(pure(c))
}

trait Parsler {
    type Output;

    fn parse(&self, input: &mut Chars) -> Result<Self::Output, String>;

    fn ap<B, F: FnOnce(Self::Output) -> B, P1: Parsler<Output = F>>(self, p1: P1) -> Ap<B, F, Self, P1>
    where
        Self: Sized,
    {
        Ap(self, p1)
    }

    fn map<B, F: FnOnce(Self::Output) -> B + Clone + Reflect + std::fmt::Debug>(self, f: F) -> Map<B, Self, F>
    where
        Self: Sized,
    {
        Map(self, f)
    }

    fn then<P1: Parsler>(self, p1: P1) -> Then<Self, P1>
    where
        Self: Sized,
    {
        Then(self, p1)
    }

    fn or<P1: Parsler<Output = Self::Output>>(self, p1: P1) -> Or<Self, P1>
    where
        Self: Sized,
    {
        Or(self, p1)
    }

    fn compile(self, info: &mut ParlserInfo) -> ast::Parser;
}

struct LazyParser<P: Parsler, F: FnOnce() -> P + Clone>(std::cell::OnceCell<P>, F);

impl <P: Parsler, F: FnOnce() -> P + Clone> LazyParser<P, F> {
    fn new(f: F) -> Self {
        Self(std::cell::OnceCell::new(), f)
    }
}

impl <P: Parsler, F: FnOnce() -> P + Clone> Parsler for LazyParser<P, F> {
    type Output = P::Output;

    fn parse(&self, input: &mut Chars) -> Result<Self::Output, String> {
        println!("Here we are! {}", input.as_str());
        self.0.get_or_init(self.1.clone()).parse(input)
    }

    fn compile(self, info: &mut ParlserInfo) -> ast::Parser {
        self.0.into_inner().unwrap_or_else(|| self.1()).compile(info)
    }
}

#[derive(Clone, Debug)]
struct Satisfy<F: Fn(char) -> bool + Reflect>(F);

impl<F: Fn(char) -> bool + Reflect + std::fmt::Debug> Parsler for Satisfy<F> {
    type Output = char;

    fn parse(&self, input: &mut Chars) -> Result<Self::Output, String> {
        if let Some(c) = input.next() {
            if self.0(c) {
                return Ok(c);
            }
        }
        Err("Expected satisfy".to_string())
    }

    fn compile(self, _info: &mut ParlserInfo) -> ast::Parser {
        todo!()
    }
}

#[derive(Clone, Debug)]
struct Ap<B, F: FnOnce(P1::Output) -> B, P1: Parsler, P2: Parsler<Output = F>>(P1, P2);

impl<B, F: FnOnce(P1::Output) -> B, P1: Parsler, P2: Parsler<Output = F>> Parsler for Ap<B, F, P1, P2> {
    type Output = B;
    fn parse(&self, input: &mut Chars) -> Result<Self::Output, String> {
        let a = self.0.parse(input)?;
        let f = self.1.parse(input)?;
        Ok(f(a))
    }

    fn compile(self, info: &mut ParlserInfo) -> ast::Parser {
        let left = self.0.compile(info);
        let right = self.1.compile(info);

        ast::Parser::Ap(Box::new(left), Box::new(right))
    }
}

#[derive(Clone, Debug)]
struct Map<B, P1: Parsler, F: FnOnce(P1::Output) -> B + Clone + Reflect + std::fmt::Debug>(P1, F);

impl<B, P1: Parsler, F: FnOnce(P1::Output) -> B + Clone + Reflect + std::fmt::Debug> Parsler for Map<B, P1, F> {
    type Output = B;
    fn parse(&self, input: &mut Chars) -> Result<Self::Output, String> {
        let a = self.0.parse(input)?;
        Ok(self.1.clone()(a))
    }

    fn compile(self, info: &mut ParlserInfo) -> ast::Parser {
        self.0.compile(info)
    }
}

#[derive(Clone, Debug)]
struct Then<P1: Parsler, P2: Parsler>(P1, P2);

impl<P1: Parsler, P2: Parsler> Parsler for Then<P1, P2> {
    type Output = P2::Output;
    fn parse(&self, input: &mut Chars) -> Result<Self::Output, String> {
        let _ = self.0.parse(input)?;
        let b = self.1.parse(input)?;
        Ok(b)
    }

    fn compile(self, info: &mut ParlserInfo) -> ast::Parser {
        let left = self.0.compile(info);
        let right = self.1.compile(info);

        ast::Parser::Then(Box::new(left), Box::new(right))
    }
}

#[derive(Clone, Debug)]
struct Before<P1: Parsler, P2: Parsler>(P1, P2);

impl<P1: Parsler, P2: Parsler> Parsler for Before<P1, P2> {
    type Output = P1::Output;
    fn parse(&self, input: &mut Chars) -> Result<Self::Output, String> {
        let a = self.0.parse(input)?;
        let _ = self.1.parse(input)?;
        Ok(a)
    }

    fn compile(self, info: &mut ParlserInfo) -> ast::Parser {
        let left = self.0.compile(info);
        let right = self.1.compile(info);

        ast::Parser::Before(Box::new(left), Box::new(right))
    }
}

#[derive(Clone, Debug)]
struct Or<P1: Parsler, P2: Parsler<Output = P1::Output>>(P1, P2);

impl<P1: Parsler, P2: Parsler<Output = P1::Output>> Parsler for Or<P1, P2> {
    type Output = P1::Output;
    fn parse(&self, input: &mut Chars) -> Result<Self::Output, String> {
        match self.0.parse(input) {
            Err(_) => self.1.parse(input),
            other => other,
        }
    }

    fn compile(self, info: &mut ParlserInfo) -> ast::Parser {
        let left = self.0.compile(info);
        let right = self.1.compile(info);

        ast::Parser::Or(Box::new(left), Box::new(right))
    }
}

#[derive(Clone, Debug)]
struct Branch<L, R, O, F1: Fn(L) -> O + Reflect, F2: Fn(R) -> O + Reflect, P1: Parsler<Output = Either<L, R>>, P2: Parsler<Output = F1>, P3: Parsler<Output = F2>>(P1, P2, P3);

impl<L, R, O, F1: Fn(L) -> O + Reflect, F2: Fn(R) -> O + Reflect, P1: Parsler<Output = Either<L, R>>, P2: Parsler<Output = F1>, P3: Parsler<Output = F2>> Parsler for Branch<L, R, O, F1, F2, P1, P2, P3> {
    type Output = O;
    fn parse(&self, input: &mut Chars) -> Result<Self::Output, String> {
        match self.0.parse(input)? {
            Either::Left(l) => self.1.parse(input).map(|f| f(l)),
            Either::Right(r) => self.2.parse(input).map(|f| f(r)),
        }
    }

    fn compile(self, info: &mut ParlserInfo) -> ast::Parser {
        let branch = self.0.compile(info);
        let left = self.1.compile(info);
        let right = self.2.compile(info);

        ast::Parser::Branch(Box::new(branch), Box::new(left), Box::new(right))
    }
}

struct Empty;

impl Parsler for Empty {
    type Output = ();
    fn parse(&self, _input: &mut Chars) -> Result<Self::Output, String> {
        Err("Expected empty".to_string())
    }

    fn compile(self, _info: &mut ParlserInfo) -> ast::Parser {
        ast::Parser::Empty
    }
}

#[derive(Clone, Debug)]
struct Try<P1: Parsler>(P1);

impl<P1: Parsler> Parsler for Try<P1> {
    type Output = P1::Output;
    fn parse(&self, input: &mut Chars) -> Result<Self::Output, String> {
        let mut copied_input = input.clone();
        let a = self.0.parse(&mut copied_input)?;
        *input = copied_input;
        Ok(a)
    }

    fn compile(self, info: &mut ParlserInfo) -> ast::Parser {
        let p = self.0.compile(info);
        ast::Parser::Try(Box::new(p))
    }
}

struct Look<P1: Parsler>(P1);

impl<P1: Parsler> Parsler for Look<P1> {
    type Output = P1::Output;
    fn parse(&self, input: &mut Chars) -> Result<Self::Output, String> {
        let a = self.0.parse(input)?;
        Ok(a)
    }

    fn compile(self, info: &mut ParlserInfo) -> ast::Parser {
        let p = self.0.compile(info);
        ast::Parser::Look(Box::new(p))
    }
}

struct NegLook<P1: Parsler>(P1);

impl<P1: Parsler<Output = ()>> Parsler for NegLook<P1> {
    type Output = ();
    fn parse(&self, input: &mut Chars) -> Result<Self::Output, String> {
        match self.0.parse(input) {
            Ok(_) => Err("Expected neg look".to_string()),
            Err(_) => Ok(()),
        }
    }

    fn compile(self, info: &mut ParlserInfo) -> ast::Parser {
        let p = self.0.compile(info);
        ast::Parser::NegLook(Box::new(p))
    }
}

#[derive(Clone, Debug)]
struct Pure<A>(A);

fn pure<A>(a: A) -> Pure<A> {
    Pure(a)
}

impl<A: Clone> Parsler for Pure<A> {
    type Output = A;
    fn parse(&self, _input: &mut Chars) -> Result<Self::Output, String> {
        let val = self.0.clone();
        Ok(val)
    }

    fn compile(self, _info: &mut ParlserInfo) -> ast::Parser {
        todo!()
    }
}

impl<P: Parsler + ?Sized> Parsler for Box<P> {
    type Output = P::Output;
    fn parse(&self, input: &mut Chars) -> Result<Self::Output, String> {
        self.as_ref().parse(input)
    }

    fn compile(self, info: &mut ParlserInfo) -> ast::Parser {
        // (*self).compile(info)
        todo!()
    }
}

// fn char(a: char) -> impl Parsler<Output = char> {

// }

// #[combinator]
// fn

// #[combinator]
// fn push_val<A>(mut v: Vec<A>) ->  {
//     |a| {
//         v.push(a);
//         v
//     }
// }

// fn sequence<A: Clone + 'static>(
//     parsers: Vec<Box<dyn Parsler<Output = A>>>,
// ) -> impl Parsler<Output = Vec<A>> {
//     parsers.into_iter().fold(
//         Box::new(pure(vec![])) as Box<dyn Parsler<Output = Vec<A>>>,
//         |acc: Box<dyn Parsler<Output = Vec<A>>>, p: Box<dyn Parsler<Output = A>>| {
//             Box::new(p.ap(acc.map(|mut v: Vec<A>| {
//                 move |a: A| {
//                     v.push(a);
//                     v
//                 }
//             }))) as Box<dyn Parsler<Output = Vec<A>>>
//         },
//     )
// }

// parser! {
//     pub let a: char = char('a');
//     pub let b: char = then(satisfy(|a| a == 'a'), pure(val('a')));
// }

#[derive(Clone, Debug)]
    struct VecPusher<A>(Vec<A>);
    impl<A> Reflect for VecPusher<A> {
        fn to_string(&self) -> String {
            format!("fn (a: char) {{ a == }}")
        }
    }

    impl<A> FnOnce<(A,)> for VecPusher<A> {
        type Output = Vec<A>;

        extern "rust-call" fn call_once(mut self, (c,): (A,)) -> Self::Output {
            self.0.push(c);
            self.0
        }
    }

#[combinator]
fn append<A>(v: Vec<A>) -> VecPusher<A> {
    VecPusher(v)
}


fn some<P: Parsler + Clone + 'static>(p: P) -> Box<dyn Parsler<Output = Vec<P::Output>>>
where
    P::Output: Clone + 'static {
    Box::new(p.clone().ap(LazyParser::new(|| many(p)).map(append)))
}

fn many<P: Parsler + Clone + 'static>(p: P) -> Box<dyn Parsler<Output = Vec<P::Output>>>
where
    P::Output: Clone + 'static {
    Box::new(LazyParser::new(|| some(p)).or(pure(vec![])))
}


fn main() {
    println!("Hello, world!, {:?}", is_a('a'));

    let parser = match_char('a').or(match_char('b'));

    let filtered_by = some(Try(match_char('a')));

    // println!("{:?}", filtered_by);

    let mut input = "aaab".chars();
    println!(
        "Hello, world!, {:?}, {}",
        filtered_by.parse(&mut input),
        input.as_str()
    );

    // println!("Hello, world!, {:?}", b("a"));
}
