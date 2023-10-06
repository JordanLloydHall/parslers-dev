#![allow(incomplete_features)]
#![feature(
    unboxed_closures,
    fn_traits,
    type_name_of_val,
    lazy_cell,
    impl_trait_in_fn_trait_return,
    return_position_impl_trait_in_trait,
    impl_trait_in_assoc_type
)]

use parslers_lib::{
    builder::Builder,
    parsler::{auxiliary::*, *},
    reflect::*,
};
use parslers_macro::reflect;

#[reflect]
pub fn append<A: 'static>(mut v: Vec<A>) -> impl FnOnce(Option<A>) -> Vec<A> {
    move |mut a| {
        if let Some(a) = a.take() {
            v.push(a);
        }
        v
    }
}

#[reflect]
pub fn option<A>(a: A) -> Option<A> {
    Some(a)
}

// fn separated_list<P>(
//     open_delim: char,
//     p: P,
//     sep: char,
//     close_delim: char,
// ) -> impl Parsler<Output = Vec<P::Output>> + Clone
// where
//     P: Parsler + Clone + 'static,
//     P::Output: Reflect + Clone + 'static,
// {
// }

fn array_type_one() -> impl Parsler<Output = Vec<char>> + Clone {
    ws(match_char('['))
        .then(
            many(ws(match_char('a')).before(ws(match_char(','))).attempt())
                .map(append)
                .ap(ws(match_char('a')).map(option).or(pure(None))),
        )
        .before(ws(match_char(']')))
}

fn array_type_two() -> impl Parsler<Output = Vec<char>> + Clone {
    ws(match_char('{'))
        .then(
            many(ws(match_char('b')).before(ws(match_char(';'))).attempt())
                .map(append)
                .ap(ws(match_char('b')).map(option).or(pure(None))),
        )
        .before(ws(match_char('}')))
}

fn main() {
    let out_dir = std::env::var("OUT_DIR").unwrap();
    Builder::new("arrays", array_type_one().or(array_type_two()))
        .reduce()
        .usage_analysis()
        .build(&format!("{out_dir}/arrays.rs"));
}
