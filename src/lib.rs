#![feature(box_patterns)]

// enum Parsler<A, B> {
//     Pure(Pure<A>),
//     Ap(Ap<A, B>),
// }

// impl<A> Parsler<A> {
//     fn parse<'a>(&self, input: &'a str) -> Result<(A, &'a str), String> {

//     }
// }

struct Pure<A>(A);

fn pure<A>(a: A) -> Pure<A> {
    Pure(a)
}

struct Ap<B, F: Fn(P1::Output) -> B, P1: Parsler, P2: Parsler<Output = F>>(P1, P2);

struct Map<B, P1: Parsler, F: Fn(P1::Output) -> B + Clone>(Ap<B, F, P1, Pure<F>>);

struct Then<P1: Parsler, P2: Parsler>(Ap<P2::Output, fn(P2::Output) -> P2::Output, P2, Map<fn(P2::Output) -> P2::Output, P1, fn(P1::Output) -> fn(P2::Output) -> P2::Output>>);


trait Parsler: Sized {
    type Output;

    fn parse<'a>(&self, input: &'a str) -> Result<(Self::Output, &'a str), String>;

    fn ap<B, F: Fn(Self::Output) -> B, P1: Parsler<Output = F>>(self, p1: P1) -> Ap<B, F, Self, P1> {
        Ap(self, p1)
    }

    fn map<B, F: Fn(Self::Output) -> B + Clone>(self, f: F) -> Map<B, Self, F> {
        Map(self.ap(Pure(f)))
    }
    
    fn then<B, P1: Parsler<Output = B>>(self, p1: P1) -> Then<Self, P1> {
        Then(p1.ap(self.map(|_| |a| a)))
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
        dbg!(pure('a').then(pure('b')).map(|x| (x as u32 as u8 + 2) as char).parse(""));
    }
}
