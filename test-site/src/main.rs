#![feature(unboxed_closures, fn_traits, type_name_of_val, lazy_cell)]
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

fn main() {
    // let parser = json();
    // let mut input = include_str!("canada2.json").chars();
    let mut input = "\"hello\"".chars();
    println!("{:?}, {}", debug_parser::hello(&mut input), input.as_str());
}
