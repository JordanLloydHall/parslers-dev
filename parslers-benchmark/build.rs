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

use std::marker::PhantomData;

use auxiliary::*;
use parslers_branflakes::Branflakes;
use parslers_lib::builder::Builder;
use parslers_lib::parsler::*;
use parslers_lib::reflect::*;
use parslers_macro::reflect;

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
fn json_object(a: std::collections::HashMap<String, parslers_json::Json>) -> parslers_json::Json {
    parslers_json::Json::Object(a)
}

#[reflect]
fn zip(a: String) -> Box<dyn FnOnce(parslers_json::Json) -> (String, parslers_json::Json)> {
    Box::new(move |b| (a, b))
}

#[reflect]
fn parse_double(s: String) -> f64 {
    fast_float::parse(s).unwrap()
}

#[reflect]
fn digit(c: char) -> bool {
    ('0'..='9').contains(&c)
}

#[reflect]
fn digit19(c: char) -> bool {
    ('1'..='9').contains(&c)
}

fn array() -> impl Parsler<Output = parslers_json::Json> + Clone {
    ws(match_char('['))
        .then(many(json().before(opt(ws(match_char(','))))).map(append))
        .ap(json().map(option).or(pure(None)))
        .before(ws(match_char(']')))
        .map(json_array)
}

fn string() -> impl Parsler<Output = String> + Clone {
    match_char('\"')
        .then(Recognise(many(not('\"'))))
        .before(ws(match_char('\"')))
}

fn object_item() -> impl Parsler<Output = (String, parslers_json::Json)> + Clone {
    string().before(ws(match_char(':'))).map(zip).ap(json())
}

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

fn number() -> impl Parsler<Output = f64> + Clone {
    Recognise(
        opt(match_char('-'))
            .then(
                match_char('0')
                    .then(pure(()))
                    .or(Satisfy(digit19).then(many(Satisfy(digit))).then(pure(()))),
            )
            .then(opt(match_char('.').then(many(Satisfy(digit)))))
            .then(opt(match_char('e')
                .or(match_char('E'))
                .then(opt(match_char('-').or(match_char('+'))))
                .then(many(Satisfy(digit))))),
    )
    .map(parse_double)
}

#[reflect]
fn parse_usize(s: String) -> usize {
    s.parse().unwrap()
}

fn object() -> impl Parsler<Output = parslers_json::Json> + Clone {
    ws(match_char('{'))
        .then(many_map((object_item()).before(opt(ws(match_char(','))))).map(insert_opt))
        .ap(object_item().map(option).or(pure(None)))
        .before(ws(match_char('}')))
        .map(json_object)
}

fn json() -> impl Parsler<Output = parslers_json::Json> + Clone {
    let boolean = ws(tag("true"))
        .then(pure(true))
        .or(ws(tag("false")).then(pure(false)))
        .map(json_bool);

    let null = ws(tag("null")).then(pure(parslers_json::Json::Null));

    let number = ws(number()).map(json_number);

    name("json", || {
        null.or(boolean)
            .or(string().map(json_string))
            .or(number)
            .or(array())
            .or(object())
    })
}

#[reflect]
fn brainfuck_val(p: Vec<parslers_branflakes::Branflakes>) -> parslers_branflakes::BranflakesProgram {
    parslers_branflakes::BranflakesProgram(p)
}
#[reflect]
fn brainfuck_loop(p: parslers_branflakes::BranflakesProgram) -> parslers_branflakes::Branflakes {
    parslers_branflakes::Branflakes::Loop(p)
}

fn brainfuck_program() -> impl Parsler<Output = parslers_branflakes::BranflakesProgram> + Clone {
    let left = match_char('<').then(pure(Branflakes::Left));
    let right = match_char('>').then(pure(Branflakes::Right));
    let add = match_char('+').then(pure(Branflakes::Add));
    let sub = match_char('-').then(pure(Branflakes::Sub));
    let print = match_char('.').then(pure(Branflakes::Print));
    let read = match_char(',').then(pure(Branflakes::Read));
    let loop_ = || {
        match_char('[')
            .then(brainfuck_program())
            .map(brainfuck_loop)
            .before(match_char(']'))
    };
    name("brainfuck", move || {
        many(
            left.or(right)
                .or(add)
                .or(sub)
                .or(print)
                .or(read)
                .or(loop_()),
        )
        .map(brainfuck_val)
    })
}

#[reflect]
fn zip_prog(
    a: Vec<parslers_wacc::ASTWrapper<String, parslers_wacc::Function<String, String>>>,
) -> Box<
    dyn FnOnce(
        Vec<parslers_wacc::ASTWrapper<String, parslers_wacc::Stat<String, String>>>,
    ) -> (
        Vec<parslers_wacc::ASTWrapper<String, parslers_wacc::Function<String, String>>>,
        Vec<parslers_wacc::ASTWrapper<String, parslers_wacc::Stat<String, String>>>,
    ),
> {
    Box::new(move |b| (a, b))
}

#[reflect]
fn bridge_program(
    (a, b): (
        Vec<parslers_wacc::ASTWrapper<String, parslers_wacc::Function<String, String>>>,
        Vec<parslers_wacc::ASTWrapper<String, parslers_wacc::Stat<String, String>>>,
    ),
) -> parslers_wacc::Program<String, String> {
    parslers_wacc::Program(a, b)
}

fn wws<O>(p: impl Parsler<Output = O> + Clone) -> impl Parsler<Output = O> + Clone {
    p.before(many(
        one_of(" \t\n\r".chars()).then(pure(())).or(match_char('#')
            .then(many(not_one_of("\n\r".chars())))
            .then(pure(()))),
    ))
}

fn wtag(s: &str) -> impl Parsler<Output = &str> + Clone {
    wws(tag(s))
}

fn wchar(s: char) -> impl Parsler<Output = char> + Clone {
    wws(match_char(s))
}

fn wacc_program() -> impl Parsler<Output = parslers_wacc::Program<String, String>> + Clone {
    wws(pure(()))
        .then(wtag("begin"))
        .then(pure(vec![]))
        .map(zip_prog)
        .ap(wacc_stats(wtag("end")))
        .map(bridge_program)
    // Empty::default()
}

fn wacc_stats(
    p: impl Parsler + Clone,
) -> impl Parsler<Output = Vec<parslers_wacc::ASTWrapper<String, parslers_wacc::Stat<String, String>>>>
       + Clone {
    (many(wacc_stat().before(opt(ws(match_char(';')))))
        .map(append)
        .ap(wacc_stat().map(option).or(pure(None))))
    .before(p)
}

fn wrapp<T: Reflect>(t: T) -> parslers_wacc::ASTWrapper<String, T> {
    parslers_wacc::ASTWrapper(String::new(), t)
}

#[reflect]
fn wacc_exit(
    a: parslers_wacc::ExprWrap<String, String>,
) -> parslers_wacc::ASTWrapper<String, parslers_wacc::Stat<String, String>> {
    parslers_wacc::ASTWrapper(String::new(), parslers_wacc::Stat::Exit(a))
}

fn wacc_stat(
) -> impl Parsler<Output = parslers_wacc::ASTWrapper<String, parslers_wacc::Stat<String, String>>> + Clone
{
    wtag("skip")
        .then(pure(wrapp(parslers_wacc::Stat::Skip)))
        .or(wtag("exit").then(wacc_expr()).map(wacc_exit))
}

fn wacc_expr() -> impl Parsler<Output = parslers_wacc::ExprWrap<String, String>> + Clone {
    Empty::default()
}

fn main() {
    let out_dir = std::env::var("OUT_DIR").unwrap();

    Builder::new("parser", parser())
        // .add_parser("wacc", wacc_program())
        .reduce()
        .usage_analysis()
        .build(&format!("{out_dir}/optimised.rs"));

    // Builder::new("json", json())
    //     .add_parser("brainfuck", brainfuck_program())
    //     .reduce()
    //     .build(&format!("{out_dir}/reduced.rs"));

    // Builder::new("json", json())
    //     .add_parser("brainfuck", brainfuck_program())
    //     .usage_analysis()
    //     .build(&format!("{out_dir}/usage_analysed.rs"));

    // Builder::new("json", json())
    //     .add_parser("brainfuck", brainfuck_program())
    //     // .add_parser("wacc", wacc_program())
    //     .build(&format!("{out_dir}/unoptimised.rs"));
}
