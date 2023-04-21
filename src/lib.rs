#![feature(specialization)]

#[derive(Debug)]
struct Pure<A>(A);

fn pure<A>(a: A) -> Pure<A> {
    Pure(a)
}

struct Ap<B, F: FnOnce(P1::Output) -> B, P1: Parsler, P2: Parsler<Output = F>>(P1, P2);

struct Map<B, P1: Parsler, F: Fn(P1::Output) -> B + Clone>(Ap<B, F, P1, Pure<F>>);

struct Then<P1: Parsler, P2: Parsler>(
    Ap<
        P2::Output,
        fn(P2::Output) -> P2::Output,
        P2,
        Map<fn(P2::Output) -> P2::Output, P1, fn(P1::Output) -> fn(P2::Output) -> P2::Output>,
    >,
);

trait Parsler {
    type Output;

    fn parse<'a>(&self, input: &'a str) -> Result<(Self::Output, &'a str), String>;

    fn ap<B, F: FnOnce(Self::Output) -> B, P1: Parsler<Output = F>>(
        self,
        p1: P1,
    )  -> Ap<B, F, Self, P1> where Self: Sized {
        Ap(self, p1)
    }

    fn map<B, F: Fn(Self::Output) -> B + Clone>(self, f: F) -> Map<B, Self, F> where Self: Sized  {
        Map(self.ap(Pure(f)))
    }

    fn then<B, P1: Parsler<Output = B>>(self, p1: P1) -> Then<Self, P1> where Self: Sized  {
        Then(p1.ap(self.map(|_| |a| a)))
    }
}



trait Opt {
    type Optimised;
    fn opt(self) -> Self::Optimised;
}

// default impl<T> Opt for T {
//     default type Optimised = Self;
//     fn opt(self) -> Self::Optimised {
//         self
//     }
// }

impl<A: Clone, B, F: FnOnce(A) -> B + Clone> Opt for Ap<B, F, Pure<A>, Pure<F>> {
    type Optimised = Pure<B>;
    fn opt(self) -> Self::Optimised {
        Pure(self.1.0(self.0.0))
    }
}

// default impl <B, F: FnOnce(P1::Output) -> B, P1: Parsler, P2: Parsler<Output = F>> Opt for Ap<B, F, P1, P2> {
//     type Optimised = Ap<B, F, P1, P2>;
//     fn opt(self) -> Self::Optimised {
//         Ap(self.0, self.1)
//     }
// }

default impl <B, F: FnOnce(P1::Output) -> B, P1: Parsler + Opt, P2: Parsler<Output = F> + Opt> Opt for Ap<B, F, P1, P2> {
    type Optimised = Ap<B, F, P1, P2>;
    fn opt(self) -> Self::Optimised {
        Ap(self.0, self.1)
    }
}

impl<B, F: FnOnce(P1::Output) -> B, P1: Parsler, P2: Parsler<Output = F>> Parsler for Ap<B, F, P1, P2> {
    type Output = B;
    fn parse<'a>(&self, input: &'a str) -> Result<(B, &'a str), String> {
        let (f, input) = self.1.parse(input)?;
        let (a, input) = self.0.parse(input)?;
        Ok((f(a), input))
    }
}

impl<B, P1: Parsler, F: Fn(P1::Output) -> B + Clone> Parsler for Map<B, P1, F> {
    type Output = B;
    fn parse<'a>(&self, input: &'a str) -> Result<(Self::Output, &'a str), String> {
        self.0.parse(input)
    }
}

struct Satisfy<F: Fn(char) -> bool>(F);

impl<F: Fn(char) -> bool> Parsler for Satisfy<F> {
    type Output = char;
    fn parse<'a>(&self, input: &'a str) -> Result<(Self::Output, &'a str), String> {
        input
            .chars()
            .next()
            .ok_or_else(|| "Unexpected end of input".to_string())
            .and_then(|c| {
                if self.0(c) {
                    Ok((c, &input[1..]))
                } else {
                    Err(format!("Unexpected character: {}", c))
                }
            })
    }
}

impl<A> Parsler for Box<dyn Parsler<Output = A>> {
    type Output = A;
    fn parse<'a>(&self, input: &'a str) -> Result<(Self::Output, &'a str), String> {
        self.as_ref().parse(input)
    }
}

// struct Sequence<A>(Vec<Box<dyn Parsler<Output = A>>>);

fn sequence<A: Clone + 'static>(parsers: Vec<Box<dyn Parsler<Output = A>>>) -> impl Parsler<Output = Vec<A>> {
    parsers.into_iter().fold(Box::new(pure(vec![])) as Box<dyn Parsler<Output = Vec<A>>>, |acc: Box<dyn Parsler<Output = Vec<A>>>, p: Box<dyn Parsler<Output = A>>| {
        Box::new(p.ap(acc.map(|mut v: Vec<A>| |a: A| {v.push(a); v}))) as Box<dyn Parsler<Output = Vec<A>>>
    })
}

// impl<A> Parsler for Sequence<A> {
//     type Output = Vec<A>;
//     fn parse<'a>(&self, input: &'a str) -> Result<(Self::Output, &'a str), String> {
//         self.0.iter().try_fold(vec![], |acc, p| {
//             let (a, input) = p.parse(input)?;
//             Ok((acc, a, input))
//         })
//     }
// }

struct LookAhead<P: Parsler>(P);

impl<P: Parsler> Parsler for LookAhead<P> {
    type Output = P::Output;
    fn parse<'a>(&self, input: &'a str) -> Result<(Self::Output, &'a str), String> {
        self.0.parse(input).map(|(output, _)| (output, input))
    }
}

const fn item() -> Satisfy<impl Fn(char) -> bool> {
    Satisfy(|_| true)
}

const fn char(c: char) -> Satisfy<impl Fn(char) -> bool> {
    Satisfy(move |c2| c == c2)
}

impl Parsler for char {
    type Output = char;
    fn parse<'a>(&self, input: &'a str) -> Result<(Self::Output, &'a str), String> {
        char(*self).parse(input)
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

// fn hello() -> Parsler<u32> {
//     dbg!(Parsler::map(Parsler::Pure('a'), Parsler::<fn(char) -> u32>::pure(|a| a as u32 )))
// }

// impl<A> Parsler<A> {
//     fn parse<E>(&self, input: &str) -> Result<A, E> {
//         todo!()
//     }

//     fn map<B, F: Fn(A) -> B>(self, px: Parsler<F>) -> Parsler<B> {

//     }

//     fn pure<B>(b: B) -> Parsler<B> {
//         Parsler::Pure(b)
//     }

// }

// fn ap<A, B, F: Fn() -> Parsler<B>, P: Fn(A) -> B>(p: Parsler<P>, px: F) -> Parsler<B> {
//     todo!()
// }

// impl<A, I, B> Parsler<A>
// where
//     A: Fn(I) -> B {

//     fn ap<F: FnOnce() -> Parsler<B>>(self, px: F) -> Parsler<C> {
//         todo!()
//     }
// }

// trait Parsler<A> {
//     type Choice<A>: Parsler<A>;
//     type AttemptChoice<A>: Parsler<A>;
//     type Sequence<A>: Parsler<A>;
//     type Traverse<B>: Parsler<B>;
//     type Skip<A>: Parsler<A>;
//     type Exactly<A>: Parsler<A>;
//     type Option<A>: Parsler<A>;
//     type Optional<A>: Parsler<A>;
//     type OptionalAs<A>: Parsler<A>;
//     type Decide<A>: Parsler<A>;
//     type Decade<A>: Parsler<A>;
// }

// impl Combinator {
//     fn optimise(self) -> Combinator {
//         match self {
//             Combinator::Or(left, right) => {
//                 let left = left.optimise();
//                 let right = right.optimise();
//                 match (left, right) {
//                     (Combinator::And(f1, s1), Combinator::And(f2, s2)) if f1 == f2 => {
//                         Combinator::And(f1, box Combinator::Or(s1, s2))
//                     }
//                     (l, r) => Combinator::Or(box l, box r),
//                 }
//             }
//             other => other,
//         }
//     }

//     fn to_rust(&self) -> String {
//         match self {
//             Combinator::Or(left, right) => {
//                 format!("or({}, {})", left.to_rust(), right.to_rust())
//             }
//             Combinator::Char(c) => {
//                 format!("char('{}')", c)
//             }
//             Combinator::And(left, right) => {
//                 format!("and({}, {})", left.to_rust(), right.to_rust())
//             }
//         }
//     }
// }

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_structure() {
        dbg!(pure('a')
            .then(pure('b'))
            .map(|x| (x as u32 as u8 + 2) as char)
            .parse(""));

        dbg!(char('a')
            .parse("a"));

        dbg!(sequence(vec![Box::new('a'), Box::new('b'), Box::new('c')]).parse("abc"));

        // dbg!(pure('a').ap(Pure(|c| (c as u32 + 1) as u8 as char)).opt());
    }
}

// trait ParserF {
//     type K;
//     type A;
//     type Pure: Fn(Self::A) -> Self;
//     type Satisfy<F, P2>: Fn (F) -> P2
//         where
//             P2: ParserF<K=Self::K, A=char>,
//             F: Fn(char) -> bool;
//     type Try: 
// }
