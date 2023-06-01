#![feature(
    unboxed_closures,
    fn_traits,
    type_name_of_val,
    lazy_cell,
    impl_trait_in_fn_trait_return
)]
#![allow(warnings, unused)]

use std::process::Command;
use std::{collections::HashMap, marker::PhantomData, rc::Rc, str::Chars};

use either::Either;

mod debug_parser {
    include!(concat!(env!("OUT_DIR"), "/combinators.rs"));
}

use parslers_lib::parsler::*;
use parslers_lib::reflect::*;
use parslers_macro::reflect;

// TODO: Rearrange Ap
// TODO: Recursive parser holing
// TODO: Benchmark current parser
// TODO: Add whitespace to parser
// TODO: Delimited parser
// TODO: Separated parser
// TODO: Move JSON to separate crate

// fn compose_<A, B, C, G, F>(f: F) -> impl FnOnce(G) -> (Box<dyn FnOnce(A) -> C>)
// where
//     F: FnOnce(A) -> B + 'static,
//     G: FnOnce(B) -> C + 'static,
// {
//     move |g| Box::new(move |x| g(f(x)))
// }

fn main() {
    let input = &mut "aaabb".chars();
    println!("tes: {:?}, {}", debug_parser::test(input), input.as_str());
    // let parser = json();
    // let mut input = "".chars();
    // let mut input = include_str!("canada2.json").chars();
    // for _ in 0..1 {
    //     let mut input = input.clone();
    //     println!(
    //         "{:?}, {:?}",
    //         debug_parser::hello(&mut input).is_ok(),
    //         // ""
    //         // debug_parser::hello(&mut input),
    //         input.as_str()
    //     );
    // }
    // let c = |a: u32| a;

    // println!("{}", c as usize);
    // println!("{}", c as usize);
}
