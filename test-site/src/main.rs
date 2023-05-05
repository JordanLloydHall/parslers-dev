#![feature(unboxed_closures, fn_traits, type_name_of_val)]
use std::collections::HashMap;

use parslers_lib::ast::{self, Spec};
use parslers_macro::{combinator, parser};

trait Reflect {
    fn name(&self) -> &'static str {
        std::any::type_name::<Self>()
    }

    fn to_string(&self) -> &'static str;
}

#[combinator]
fn is_a(c: char) -> bool {
    c == 'a'
}

struct ParlserInfo {
    parslers: Spec,
    functions: HashMap<&'static str, Box<dyn Reflect>>,
}

struct Pure<A>(A);

fn pure<A>(a: A) -> Pure<A> {
    Pure(a)
}

struct Ap<B, F: Fn(P1::Output) -> B, P1: Parsler, P2: Parsler<Output = F>>(P1, P2);

struct Map<B, P1: Parsler, F: Fn(P1::Output) -> B + Clone>(Ap<B, F, P1, Pure<F>>);

struct Then<P1: Parsler, P2: Parsler>(
    Ap<
        P2::Output,
        fn(P2::Output) -> P2::Output,
        P2,
        Map<fn(P2::Output) -> P2::Output, P1, fn(P1::Output) -> fn(P2::Output) -> P2::Output>,
    >,
);

trait Parsler: Sized {
    type Output;

    fn parse<'a>(&self, input: &'a str) -> Result<(Self::Output, &'a str), String>;

    fn ap<B, F: Fn(Self::Output) -> B, P1: Parsler<Output = F>>(
        self,
        p1: P1,
    ) -> Ap<B, F, Self, P1> {
        Ap(self, p1)
    }

    fn map<B, F: Fn(Self::Output) -> B + Clone>(self, f: F) -> Map<B, Self, F> {
        Map(self.ap(Pure(f)))
    }

    fn then<B, P1: Parsler<Output = B>>(self, p1: P1) -> Then<Self, P1> {
        Then(p1.ap(self.map(|_| |a| a)))
    }

    fn pure<A>(a: A) -> Pure<A> {
        Pure(a)
    }
}

impl<B, F: Fn(P1::Output) -> B, P1: Parsler, P2: Parsler<Output = F>> Parsler for Ap<B, F, P1, P2> {
    type Output = B;
    fn parse<'a>(&self, input: &'a str) -> Result<(B, &'a str), String> {
        let (f, input) = self.1.parse(input)?;
        let (a, input) = self.0.parse(input)?;
        Ok((f(a), input))
    }
}

// struct Map<B, P1: Parsler, F: Fn(P1::Output) -> B>(P1, F);

impl<B, P1: Parsler, F: Fn(P1::Output) -> B + Clone> Parsler for Map<B, P1, F> {
    type Output = B;
    fn parse<'a>(&self, input: &'a str) -> Result<(Self::Output, &'a str), String> {
        self.0.parse(input)
    }
}

impl<P1: Parsler, P2: Parsler> Parsler for Then<P1, P2> {
    type Output = P2::Output;
    fn parse<'a>(&self, input: &'a str) -> Result<(Self::Output, &'a str), String> {
        self.0.parse(input)
    }
}

impl<A: Clone> Parsler for Pure<A> {
    type Output = A;
    fn parse<'a>(&self, input: &'a str) -> Result<(A, &'a str), std::string::String> {
        let val = self.0.clone();
        Ok((val, input))
    }
}
impl<A> Parsler for Box<dyn Parsler<Output = A>> {
    type Output = A;
    fn parse<'a>(&self, input: &'a str) -> Result<(Self::Output, &'a str), String> {
        self.as_ref().parse(input)
    }
}
fn sequence<A: Clone + 'static>(
    parsers: Vec<Box<dyn Parsler<Output = A>>>,
) -> impl Parsler<Output = Vec<A>> {
    parsers.into_iter().fold(
        Box::new(pure(vec![])) as Box<dyn Parsler<Output = Vec<A>>>,
        |acc: Box<dyn Parsler<Output = Vec<A>>>, p: Box<dyn Parsler<Output = A>>| {
            Box::new(p.ap(acc.map(|mut v: Vec<A>| {
                |a: A| {
                    v.push(a);
                    v
                }
            }))) as Box<dyn Parsler<Output = Vec<A>>>
        },
    )
}

// parser! {
//     pub let a: char = char('a');
//     pub let b: char = then(satisfy(|a| a == 'a'), pure(val('a')));
// }

fn main() {
    println!("Hello, world!, {:?}", is_a('a'));

    // println!("Hello, world!, {:?}", a("as"));

    // println!("Hello, world!, {:?}", b("a"));
}
