#![feature(unboxed_closures, fn_traits, type_name_of_val, lazy_cell)]
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

#[reflect]
fn is_a(a: &char) -> bool {
    a == &'a'
}

#[reflect]
fn json_bool(b: bool) -> parslers_json::Json {
    parslers_json::Json::Bool(b)
}

#[reflect]
fn json_string(s: String) -> parslers_json::Json {
    parslers_json::Json::String(s)
}

#[reflect]
fn json_number(f: f64) -> parslers_json::Json {
    parslers_json::Json::Number(f)
}

#[reflect]
fn json_array(a: Vec<parslers_json::Json>) -> parslers_json::Json {
    parslers_json::Json::Array(a)
}

#[reflect]
fn json_object(a: Vec<(String, parslers_json::Json)>) -> parslers_json::Json {
    parslers_json::Json::Object(a.into_iter().collect())
}

#[reflect]
fn object_unzip(
    (a, b): (parslers_json::Json, parslers_json::Json),
) -> (String, parslers_json::Json) {
    match a {
        parslers_json::Json::String(s) => (s, b),
        _ => unreachable!(),
    }
}

#[reflect]
fn zip<A: 'static>(a: A) -> Box<dyn FnOnce(A) -> (A, A)> {
    Box::new(move |b| (a, b))
}

#[reflect]
fn parse_double(s: String) -> f64 {
    s.parse::<f64>().unwrap()
}

fn array() -> impl Parsler<Output = parslers_json::Json> + Clone {
    (Rc::new(LazyParser::new(|| json().map(singleton).or(pure(vec![]))))
        as Rc<dyn Parsler<Output = Vec<parslers_json::Json>>>)
        .ap(match_char('[').attempt().then(
            many(
                (Rc::new(LazyParser::new(|| json()))
                    as Rc<dyn Parsler<Output = parslers_json::Json>>)
                    .before(opt(match_char(',').attempt())),
            )
            .map(concat),
        ))
        .before(match_char(']'))
        .map(json_array)
}

fn string() -> impl Parsler<Output = String> + Clone {
    match_char('\"')
        .attempt()
        .then(Recognise(many(not('\"').attempt())))
        .before(match_char('\"'))
}

fn object_item() -> impl Parsler<Output = (String, parslers_json::Json)> + Clone {
    (Rc::new(LazyParser::new(|| json())) as Rc<dyn Parsler<Output = parslers_json::Json>>)
        .ap(string().map(json_string).before(match_char(':')).map(zip))
        .map(object_unzip)
}

fn object() -> impl Parsler<Output = parslers_json::Json> + Clone {
    (object_item())
        .map(singleton)
        .or(pure(vec![]))
        .ap(match_char('{').attempt().then(
            many((object_item()).before(match_char(',').attempt().or(pure(' ')))).map(concat),
        ))
        .before(match_char('}'))
        .map(json_object)
}

fn json() -> impl Parsler<Output = parslers_json::Json> + Clone {
    let boolean = tag("true")
        .attempt()
        .then(pure(true))
        .or(tag("false").attempt().then(pure(false)))
        .map(json_bool);

    let null = tag("null").attempt().then(pure(parslers_json::Json::Null));

    let number = Recognise(
        opt(match_char('-').attempt())
            .then(some(one_of('0'..='9')))
            .then(opt(match_char('.').attempt().then(many(one_of('0'..='9'))))),
    )
    .map(parse_double)
    .map(json_number);

    NamedParser(
        null.or(boolean)
            .or(string().map(json_string))
            .or(number)
            .or(array())
            .or(object()),
        "json".to_owned(),
    )
}

// mod hello {
//     fn some_rev_1146231101952406494(
//         input: &mut std::str::Chars,
//     ) -> Result<alloc::vec::Vec<char>, &'static str> {
//         { { Ok (f14) } . and_then (| f | { let result = { let copied_input = input . clone () ; let result = { { input . next () . ok_or ("Found EOF when character was expected") . and_then (| c | if (f19) (c) { Ok (c) } else { Err ("expected a specific character") }) } . and_then (| _ | { Ok ('0') }) } ; if result . is_err () { * input = copied_input ; } result } ; if result . is_ok () { result } else { { let result = { let copied_input = input . clone () ; let result = { { input . next () . ok_or ("Found EOF when character was expected") . and_then (| c | if (f20) (c) { Ok (c) } else { Err ("expected a specific character") }) } . and_then (| _ | { Ok ('1') }) } ; if result . is_err () { * input = copied_input ; } result } ; if result . is_ok () { result } else { { let result = { let copied_input = input . clone () ; let result = { { input . next () . ok_or ("Found EOF when character was expected") . and_then (| c | if (f21) (c) { Ok (c) } else { Err ("expected a specific character") }) } . and_then (| _ | { Ok ('2') }) } ; if result . is_err () { * input = copied_input ; } result } ; if result . is_ok () { result } else { { let result = { let copied_input = input . clone () ; let result = { { input . next () . ok_or ("Found EOF when character was expected") . and_then (| c | if (f22) (c) { Ok (c) } else { Err ("expected a specific character") }) } . and_then (| _ | { Ok ('3') }) } ; if result . is_err () { * input = copied_input ; } result } ; if result . is_ok () { result } else { { let result = { let copied_input = input . clone () ; let result = { { input . next () . ok_or ("Found EOF when character was expected") . and_then (| c | if (f23) (c) { Ok (c) } else { Err ("expected a specific character") }) } . and_then (| _ | { Ok ('4') }) } ; if result . is_err () { * input = copied_input ; } result } ; if result . is_ok () { result } else { { let result = { let copied_input = input . clone () ; let result = { { input . next () . ok_or ("Found EOF when character was expected") . and_then (| c | if (f24) (c) { Ok (c) } else { Err ("expected a specific character") }) } . and_then (| _ | { Ok ('5') }) } ; if result . is_err () { * input = copied_input ; } result } ; if result . is_ok () { result } else { { let result = { let copied_input = input . clone () ; let result = { { input . next () . ok_or ("Found EOF when character was expected") . and_then (| c | if (f25) (c) { Ok (c) } else { Err ("expected a specific character") }) } . and_then (| _ | { Ok ('6') }) } ; if result . is_err () { * input = copied_input ; } result } ; if result . is_ok () { result } else { { let result = { let copied_input = input . clone () ; let result = { { input . next () . ok_or ("Found EOF when character was expected") . and_then (| c | if (f26) (c) { Ok (c) } else { Err ("expected a specific character") }) } . and_then (| _ | { Ok ('7') }) } ; if result . is_err () { * input = copied_input ; } result } ; if result . is_ok () { result } else { { let result = { let copied_input = input . clone () ; let result = { { input . next () . ok_or ("Found EOF when character was expected") . and_then (| c | if (f27) (c) { Ok (c) } else { Err ("expected a specific character") }) } . and_then (| _ | { Ok ('8') }) } ; if result . is_err () { * input = copied_input ; } result } ; if result . is_ok () { result } else { { let copied_input = input . clone () ; let result = { { input . next () . ok_or ("Found EOF when character was expected") . and_then (| c | if (f28) (c) { Ok (c) } else { Err ("expected a specific character") }) } . and_then (| _ | { Ok ('9') }) } ; if result . is_err () { * input = copied_input ; } result } } } } } } } } } } } } } } } } } } } . map (| x | f (x))) } . and_then (| f | { many_rev_17877461323594952870 (input) } . map (| x | f (x)))
//     }
// }

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

    // let null_or_bool_or_num = null.or(boolean).or(number);

    // let non_recurse = number.or(string);

    // let input = "[1,2,3,4,5]";

    // println!("{}, {:?}", input, json.parse(&mut input.chars()).unwrap());

    // let filt = filtered_by(one_of('a'..='b'), is_a);

    // let number = Recognise(
    //     opt(match_char('-').attempt())
    //         .then(some(one_of('0'..='9')))
    //         .then(opt(match_char('.').attempt().then(many(one_of('0'..='9')))));

    let context = &mut parslers_lib::code_gen::CompileContext::new();

    let mut p = parslers_lib::code_gen::compile("hello", json(), context);

    context.optimise_named_parsers();

    eprintln!("{:#?}", context.parsers_with_unused);

    // eprintln!(
    //     "{:#?}",
    //     context
    //         .named_parsers
    //         .get("many_rev_17877461323594952870_unused")
    //         .unwrap()
    //         .as_ref()
    //         .unwrap()
    //         .parser
    // );

    // p.parser = parslers_lib::ast::optimise(p.parser);

    let parslers_out = parslers_lib::code_gen::gen_statement(p, context).to_string();

    // Write the combinators file to the output directory
    std::fs::write(format!("{}/combinators.rs", out_dir), parslers_out).unwrap();
}
