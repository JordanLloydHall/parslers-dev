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
use proc_macro2::TokenStream;

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
fn parse_double((a, b): (Vec<char>, Vec<char>)) -> f64 {
    a.into_iter()
        .chain(std::iter::once('.'))
        .chain(b.into_iter())
        .collect::<String>()
        .parse::<f64>()
        .unwrap()
}

fn array() -> impl Parsler<Output = parslers_json::Json> + Clone {
    (Rc::new(LazyParser::new(|| json().map(singleton).or(pure(vec![]))))
        as Rc<dyn Parsler<Output = Vec<parslers_json::Json>>>)
        .ap(match_char('[').attempt().then(
            many(
                (Rc::new(LazyParser::new(|| json()))
                    as Rc<dyn Parsler<Output = parslers_json::Json>>)
                    .before(match_char(',').attempt().or(pure(' '))),
            )
            .map(concat),
        ))
        .before(match_char(']'))
        .map(json_array)
}

fn string() -> impl Parsler<Output = String> + Clone {
    match_char('\"')
        .attempt()
        .then(many(not('\"').attempt()))
        .before(match_char('\"'))
        .map(collect_string)
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

    let number = match_char('.')
        .attempt()
        .then(many(one_of('0'..='9')))
        .or(pure(vec![]))
        .ap(some(one_of('0'..='9'))
            .ap(match_char('-')
                .attempt()
                .or(pure('0'))
                .map(singleton)
                .map(concat))
            .map(zip))
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

pub fn pretty_print(tokens: TokenStream) -> Result<String, ()> {
    let tokens = tokens.to_string();

    let mut child = Command::new("rustfmt")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();

    let mut stdin = child.stdin.take().unwrap();
    write!(stdin, "{tokens}").unwrap();
    stdin.flush().unwrap();
    drop(stdin);

    let Output {
        status,
        stdout,
        stderr,
    } = child.wait_with_output().unwrap();
    let stdout = String::from_utf8_lossy(&stdout);
    let stderr = String::from_utf8_lossy(&stderr);

    if !status.success() {
        eprintln!("---- Stdout ----");
        eprintln!("{stdout}");
        eprintln!("---- Stderr ----");
        eprintln!("{stderr}");
        let code = status.code();
        match code {
            Some(code) => panic!("The `rustfmt` command failed with return code {code}"),
            None => panic!("The `rustfmt` command failed"),
        }
    }

    Ok(stdout.into())
}

fn main() {
    let out_dir = std::env::var("OUT_DIR").unwrap();

    // let json = json();
    let boolean = tag("true")
        .attempt()
        .then(pure(true))
        .or(tag("false").attempt().then(pure(false)))
        .map(json_bool);

    let number = match_char('.')
        .attempt()
        .then(many(one_of('0'..='9')))
        .or(pure(vec![]))
        .ap(some(one_of('0'..='9'))
            .ap(match_char('-')
                .attempt()
                .or(pure('0'))
                .map(singleton)
                .map(concat))
            .map(zip))
        .map(parse_double)
        .map(json_number);

    let null = tag("null").attempt().then(pure(parslers_json::Json::Null));

    let string = string().map(json_string);

    // let null_or_bool_or_num = null.or(boolean).or(number);

    let non_recurse = number.or(string);

    // let input = "[1,2,3,4,5]";

    // println!("{}, {:?}", input, json.parse(&mut input.chars()).unwrap());

    // let filt = filtered_by(one_of('a'..='b'), is_a);

    // let some = some(match_char('a'));

    let context = &mut parslers_lib::code_gen::CompileContext::new();

    let p = parslers_lib::code_gen::compile("hello", non_recurse, context);

    let parslers_out = parslers_lib::code_gen::gen_statement(p, context).to_string();

    // Write the combinators file to the output directory
    std::fs::write(format!("{}/combinators.rs", out_dir), parslers_out).unwrap();
}
