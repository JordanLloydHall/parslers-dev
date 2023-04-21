#![allow(dead_code)]
use std::collections::{BinaryHeap, HashSet};

enum Expr {
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mult(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Number(i32),
}
fn digit(input: &str) -> Result<(char, &str), String> {
    or(
        char('0'),
        or(
            char('1'),
            or(
                char('2'),
                or(
                    char('3'),
                    or(
                        char('4'),
                        or(
                            char('5'),
                            or(char('6'), or(char('7'), or(char('8'), char('9')))),
                        ),
                    ),
                ),
            ),
        ),
    )(input)
}

fn or<I, I2, O, F1, F2, E>(f1: F1, f2: F2) -> impl Fn(I) -> Result<(O, I2), E>
where
    I: Clone,
    F1: Fn(I) -> Result<(O, I2), E>,
    F2: Fn(I) -> Result<(O, I2), E>,
{
    move |i: I| f1(i.clone()).or_else(|_| f2(i))
}

fn then<I, O1, O2, F1, F2, E>(f1: F1, f2: F2) -> impl Fn(I) -> Result<((O1, O2), I), E>
where
    F1: Fn(I) -> Result<(O1, I), E>,
    F2: Fn(I) -> Result<(O2, I), E>,
{
    move |i: I| f1(i).and_then(|(i1, i2)| f2(i2).map(|(i3, i4)| ((i1, i3), i4)))
}

fn char(c: char) -> impl Fn(&str) -> Result<(char, &str), String> {
    move |i: &str| {
        let mut chars = i.chars();
        if chars.next() == Some(c) {
            Ok((c, chars.as_str()))
        } else {
            Err(format!("Could not find char {c}"))
        }
    }
}

/// You're on to something here. We want to bring back an iterator and not a vector. I think we can do that by using the `Iterator` trait.
/// I'm not sure how to do that yet, but I think it's possible. I'll have to look into it more.
struct ManyIter<F, I, E>(F, I, Option<E>);

impl<I, O, F, E> Iterator for ManyIter<F, I, E>
where
    F: Fn(I) -> Result<(O, I), E>,
    I: Clone,
{
    type Item = O;

    fn next(&mut self) -> Option<Self::Item> {
        match self.0(self.1.clone()) {
            Ok((o, rest)) => {
                self.1 = rest;
                Some(o)
            }
            Err(e) => {
                self.2 = Some(e);
                None
            }
        }
    }
}

fn many0_iter<It, I, O, F>(f: F) -> impl Fn(I) -> Result<(It, I), String>
where
    I: Clone,
    F: Fn(I) -> Result<(O, I), String> + Copy,
    It: FromIterator<O>,
{
    move |i: I| {
        let mut iter = ManyIter(f, i, None);
        let result = It::from_iter(iter.by_ref());
        let rest = iter.1;
        Ok((result, rest))
    }
}

fn many0<I, O, F>(f: F) -> impl Fn(I) -> Result<(Vec<O>, I), String>
where
    I: Clone,
    F: Fn(I) -> Result<(O, I), String>,
{
    move |mut i: I| {
        let mut result = Vec::new();
        while let Ok((o, rest)) = f(i.clone()) {
            i = rest;
            result.push(o);
        }
        Ok((result, i))
    }
}

fn map<I, O1, O2, F, M, E>(f: F, m: M) -> impl Fn(I) -> Result<(O2, I), E>
where
    F: Fn(I) -> Result<(O1, I), E>,
    M: Fn(O1) -> O2,
{
    move |i: I| f(i).map(|(o1, rest)| (m(o1), rest))
}

fn map_res<I, O1, O2, F, M, E>(f: F, m: M) -> impl Fn(I) -> Result<(O2, I), E>
where
    F: Fn(I) -> Result<(O1, I), E>,
    M: Fn(O1) -> Result<O2, E>,
{
    move |i: I| f(i).and_then(|(o1, rest)| m(o1).map(|o2| (o2, rest)))
}

fn many1<I, O, F>(f: F) -> impl Fn(I) -> Result<(Vec<O>, I), String>
where
    I: Clone,
    F: Fn(I) -> Result<(O, I), String> + Clone,
{
    map(then(f.clone(), many0(f)), |(o, os)| {
        std::iter::once(o).chain(os).collect()
    })
}

fn foldl<I, O1, O2, Fi, F, M2>(fi: Fi, f: F, m: M2) -> impl Fn(I) -> Result<(O2, I), String>
where
    Fi: Fn(I) -> Result<(O2, I), String>,
    F: Fn(I) -> Result<(O1, I), String>,
    M2: Fn(O2, O1) -> O2,
    I: Clone,
{
    move |i: I| {
        let (mut o, mut rest) = fi(i)?;
        while let Ok((o2, rest2)) = f(rest.clone()) {
            o = m(o, o2);
            rest = rest2;
        }
        Ok((o, rest))
    }
}

fn try_foldl<I, O1, O2, Fi, F, M2, E>(fi: Fi, f: F, m: M2) -> impl Fn(I) -> Result<(O2, I), E>
where
    Fi: Fn(I) -> Result<(O2, I), E>,
    F: Fn(I) -> Result<(O1, I), E>,
    M2: Fn(O2, O1) -> Result<O2, E>,
    I: Clone,
    O2: Clone,
{
    move |i: I| {
        let (mut o, mut rest) = fi(i)?;
        while let Ok((o2, rest2)) = f(rest.clone()) {
            match m(o.clone(), o2) {
                Ok(o3) => {
                    o = o3;
                    rest = rest2;
                }
                Err(_) => break,
            }
        }
        Ok((o, rest))
    }
}

fn integer(input: &str) -> Result<(u32, &str), String> {
    fn to_digit(c: char) -> Result<u32, String> {
        c.to_digit(10).ok_or("Not a digit".to_string())
    }
    let inner_map = map_res(digit, to_digit);
    let outer_map = |d1, c| to_digit(c).map(|d2| d1 * 10 + d2);
    try_foldl(inner_map, digit, outer_map)(input)
}

// fn parse_expr(input: &str) -> Result<Expr, String> {
//     todo!()
// }

fn main() {
    // let parser = char('a');

    println!("{:?}", digit("wow"));
    println!("{:?}", integer("90"));

    let input = "42420wow";
    let output: Result<(Vec<char>, &str), String> = many0_iter(digit)(input);
    println!("{:?}", output);

    let input = "42420wow";
    let output: Result<(HashSet<char>, &str), String> = many0_iter(digit)(input);
    println!("{:?}", output);

    let input = "42420wow";
    let output: Result<(BinaryHeap<char>, &str), String> = many0_iter(digit)(input);
    println!("{:?}", output.map(|(h, i)| (h.into_sorted_vec(), i)));

    println!("Hello world!");
}
