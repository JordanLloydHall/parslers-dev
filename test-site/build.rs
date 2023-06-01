#![feature(
    unboxed_closures,
    fn_traits,
    type_name_of_val,
    lazy_cell,
    impl_trait_in_fn_trait_return,
    return_position_impl_trait_in_trait,
    impl_trait_in_assoc_type
)]
use std::collections::HashMap;
use std::io::Write;
use std::process::Command;
use std::process::Output;
use std::process::Stdio;
use std::rc::Rc;

// use parslers_json::parslers_json::Json;
use parslers_lib::parsler::*;
use parslers_lib::reflect::*;
use parslers_macro::reflect;
use proc_macro2::TokenStream;

#[reflect]
fn times_by_two(n: u32) -> u32 {
    n * 2
}

#[reflect]
fn parse_u32(s: String) -> u32 {
    s.parse::<u32>().unwrap()
}

// mod hello {
//     fn some_rev_1146231101952406494(
//         input: &mut std::str::Chars,
//     ) -> Result<alloc::vec::Vec<char>, &'static str> {
//         { { Ok (f14) } . and_then (| f | { let result = { let copied_input = input . clone () ; let result = { { input . next () . ok_or ("Found EOF when character was expected") . and_then (| c | if (f19) (c) { Ok (c) } else { Err ("expected a specific character") }) } . and_then (| _ | { Ok ('0') }) } ; if result . is_err () { * input = copied_input ; } result } ; if result . is_ok () { result } else { { let result = { let copied_input = input . clone () ; let result = { { input . next () . ok_or ("Found EOF when character was expected") . and_then (| c | if (f20) (c) { Ok (c) } else { Err ("expected a specific character") }) } . and_then (| _ | { Ok ('1') }) } ; if result . is_err () { * input = copied_input ; } result } ; if result . is_ok () { result } else { { let result = { let copied_input = input . clone () ; let result = { { input . next () . ok_or ("Found EOF when character was expected") . and_then (| c | if (f21) (c) { Ok (c) } else { Err ("expected a specific character") }) } . and_then (| _ | { Ok ('2') }) } ; if result . is_err () { * input = copied_input ; } result } ; if result . is_ok () { result } else { { let result = { let copied_input = input . clone () ; let result = { { input . next () . ok_or ("Found EOF when character was expected") . and_then (| c | if (f22) (c) { Ok (c) } else { Err ("expected a specific character") }) } . and_then (| _ | { Ok ('3') }) } ; if result . is_err () { * input = copied_input ; } result } ; if result . is_ok () { result } else { { let result = { let copied_input = input . clone () ; let result = { { input . next () . ok_or ("Found EOF when character was expected") . and_then (| c | if (f23) (c) { Ok (c) } else { Err ("expected a specific character") }) } . and_then (| _ | { Ok ('4') }) } ; if result . is_err () { * input = copied_input ; } result } ; if result . is_ok () { result } else { { let result = { let copied_input = input . clone () ; let result = { { input . next () . ok_or ("Found EOF when character was expected") . and_then (| c | if (f24) (c) { Ok (c) } else { Err ("expected a specific character") }) } . and_then (| _ | { Ok ('5') }) } ; if result . is_err () { * input = copied_input ; } result } ; if result . is_ok () { result } else { { let result = { let copied_input = input . clone () ; let result = { { input . next () . ok_or ("Found EOF when character was expected") . and_then (| c | if (f25) (c) { Ok (c) } else { Err ("expected a specific character") }) } . and_then (| _ | { Ok ('6') }) } ; if result . is_err () { * input = copied_input ; } result } ; if result . is_ok () { result } else { { let result = { let copied_input = input . clone () ; let result = { { input . next () . ok_or ("Found EOF when character was expected") . and_then (| c | if (f26) (c) { Ok (c) } else { Err ("expected a specific character") }) } . and_then (| _ | { Ok ('7') }) } ; if result . is_err () { * input = copied_input ; } result } ; if result . is_ok () { result } else { { let result = { let copied_input = input . clone () ; let result = { { input . next () . ok_or ("Found EOF when character was expected") . and_then (| c | if (f27) (c) { Ok (c) } else { Err ("expected a specific character") }) } . and_then (| _ | { Ok ('8') }) } ; if result . is_err () { * input = copied_input ; } result } ; if result . is_ok () { result } else { { let copied_input = input . clone () ; let result = { { input . next () . ok_or ("Found EOF when character was expected") . and_then (| c | if (f28) (c) { Ok (c) } else { Err ("expected a specific character") }) } . and_then (| _ | { Ok ('9') }) } ; if result . is_err () { * input = copied_input ; } result } } } } } } } } } } } } } } } } } } } . map (| x | f (x))) } . and_then (| f | { many_rev_17877461323594952870 (input) } . map (| x | f (x)))
//     }
// }

// extern crate alloc;
// #[inline(always)]
// fn compose_<A, B, C, G, F>(g: G) -> impl FnOnce(F) -> (impl FnOnce(A) -> C + Sized)
// where
//     F: FnOnce(A) -> B + 'static,
//     G: FnOnce(B) -> C + 'static,
// {
//     move |f| move |x| g(f(x))
// }

// fn compose2<A, B, C, G, F>(g: G) -> impl FnOnce(F) -> impl FnOnce(A) -> C
// where
//     F: FnOnce(A) -> B + 'static,
//     G: FnOnce(B) -> C + 'static,
// {
//     move |f| move |x| g(f(x))
// }

use parslers_lib::parsler::*;

#[reflect]
fn inc(x: usize) -> usize {
    x + 1
}

fn count_as() -> impl Parsler<Output = Vec<char>> {
    match_char('a').map(append).ap(pure(vec![]))
}

#[reflect]
pub fn append_fn<A: 'static>(a: A) -> impl FnOnce(Vec<A>) -> Vec<A> {
    move |mut v| {
        v.push(a);
        v
    }
}

#[reflect(unbox)]
fn f1(c: char) -> Box<dyn FnOnce() -> char> {
    Box::new(move || c)
}
#[reflect]
fn f2(f: Box<dyn FnOnce() -> char>) -> char {
    f()
}
fn parser() -> impl Parsler<Output = char> {
    pure('a').map(f1).map(f2)
}

#[reflect]
fn is_a(c: char) -> bool {
    c == 'a'
}

#[reflect]
fn neg(b: bool) -> bool {
    !b
}

fn main() {
    let out_dir = std::env::var("OUT_DIR").unwrap();

    // let json = json();

    // let number = match_char('.')
    //     .attempt()
    //     .then(many(one_of('0'..='9')))
    //     .or(pure(vec![]))
    //     .ap(some(one_of('0'..='9'))
    //         .ap(match_char('-')
    //             .attempt()
    //             .or(pure('0'))
    //             .map(singleton)
    //             .map(concat))
    //         .map(zip))
    //     .map(parse_double)
    //     .map(json_number)

    // let null_or_bool_or_ num = null.or(boolean).or(number);

    // let non_recurse = number.or(string);

    // let input = "[1,2,3,4,5]";

    // println!("{}, {:?}", input, json.parse(&mut input.chars()).unwrap());

    // let filt = filtered_by(one_of('a'..='b'), is_a);

    // let number = Recognise(
    //     opt(match_char('-').attempt())
    //         .then(some(one_of('0'..='9')))
    //         .then(opt(match_char('.').attempt().then(many(one_of('0'..='9')))));

    let context = &mut parslers_lib::code_gen::CompileContext::new();

    // let mut p = parslers_lib::code_gen::compile("hello", json(), context);

    // context.optimise_named_parsers();

    eprintln!("{:#?}", context.parsers_with_unused);

    // eprintln!(
    //     "{:#?}",
    //     context
    //         .named_parsers
    //         .get( "many_rev_17877461323594952870_unused")
    //         .un wrap()
    //         .as_ref()
    //         .unwrap()
    //         .parser
    // );

    // p.parser = parslers_lib::ast::optimise(p.parser);

    // let test = some(match_char('a'));
    let test = pure('a').map(is_a).map(neg);

    let mut test = parslers_lib::code_gen::compile("test", test, context);

    // test.parser = test.parser.reduce_down();
    test.parser = test.parser.reduce();

    context.optimise_named_parsers();

    let parslers_out = parslers_lib::code_gen::gen_statement(&[test], context).to_string();

    // Write the combinators file to the output directoryas
    std::fs::write(format!("{}/combinators.rs", out_dir), parslers_out).unwrap();
}
