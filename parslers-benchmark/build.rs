#![feature(
    unboxed_closures,
    fn_traits,
    type_name_of_val,
    lazy_cell,
    impl_trait_in_fn_trait_return,
    return_position_impl_trait_in_trait,
    impl_trait_in_assoc_type
)]

use std::rc::Rc;

use parslers_branflakes::Brainfuck;
use parslers_json::Json;
// use parslers_json::parslers_json::Json;
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
    ws(match_char('[').attempt())
        .then(many(json().before(opt(ws(match_char(',').attempt())))).map(append))
        .ap(json().map(option).or(pure(None)))
        .before(ws(match_char(']')))
        .map(json_array)
}

fn string() -> impl Parsler<Output = String> + Clone {
    match_char('\"')
        .attempt()
        .then(Recognise(many(not('\"').attempt())))
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
        opt(match_char('-').attempt())
            .then(
                match_char('0').attempt().then(pure(())).or(Satisfy(digit19)
                    .attempt()
                    .then(many(Satisfy(digit).attempt()))
                    .then(pure(()))),
            )
            .then(opt(match_char('.')
                .attempt()
                .then(many(Satisfy(digit).attempt()))))
            .then(opt(match_char('e')
                .attempt()
                .or(match_char('E').attempt())
                .then(opt(match_char('-').attempt().or(match_char('+').attempt())))
                .then(many(Satisfy(digit).attempt())))),
    )
    .map(parse_double)
}

#[reflect]
fn parse_usize(s: String) -> usize {
    s.parse().unwrap()
}
fn uint() -> impl Parsler<Output = usize> + Clone {
    Recognise(many(Satisfy(digit))).map(parse_usize)
}

fn object() -> impl Parsler<Output = parslers_json::Json> + Clone {
    ws(match_char('{').attempt())
        .then(many_map((object_item()).before(opt(ws(match_char(',').attempt())))).map(insert_opt))
        .ap(object_item().map(option).or(pure(None)))
        .before(ws(match_char('}')))
        .map(json_object)
}

fn json() -> impl Parsler<Output = parslers_json::Json> + Clone {
    let boolean = ws(tag("true").attempt())
        .then(pure(true))
        .or(ws(tag("false").attempt()).then(pure(false)))
        .map(json_bool);

    let null = ws(tag("null").attempt()).then(pure(parslers_json::Json::Null));

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
fn brainfuck_val(p: Vec<parslers_branflakes::Brainfuck>) -> parslers_branflakes::BrainfuckProgram {
    parslers_branflakes::BrainfuckProgram(p)
}
#[reflect]
fn brainfuck_loop(p: parslers_branflakes::BrainfuckProgram) -> parslers_branflakes::Brainfuck {
    parslers_branflakes::Brainfuck::Loop(p)
}

fn brainfuck_program() -> impl Parsler<Output = parslers_branflakes::BrainfuckProgram> + Clone {
    let left = match_char('<').attempt().then(pure(Brainfuck::Left));
    let right = match_char('>').attempt().then(pure(Brainfuck::Right));
    let add = match_char('+').attempt().then(pure(Brainfuck::Add));
    let sub = match_char('-').attempt().then(pure(Brainfuck::Sub));
    let print = match_char('.').attempt().then(pure(Brainfuck::Print));
    let read = match_char(',').attempt().then(pure(Brainfuck::Read));
    let loop_ = || {
        match_char('[')
            .attempt()
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

fn some_a() -> impl Parsler<Output = ()> + Clone {
    name("some_a", || match_char('a').then(opt(some_a())))
}

fn main() {
    let out_dir = std::env::var("OUT_DIR").unwrap();

    let context = &mut parslers_lib::code_gen::CompileContext::new();

    let mut p = parslers_lib::code_gen::compile("json", json(), context);
    let mut p_validate =
        parslers_lib::code_gen::compile("json_validate", json().then(pure(())), context);

    let mut brainfuck = parslers_lib::code_gen::compile("brainfuck", brainfuck_program(), context);
    let mut brainfuck_validate = parslers_lib::code_gen::compile(
        "brainfuck_validate",
        brainfuck_program().then(pure(())),
        context,
    );

    // let test = pure("1.2344".to_owned()).map(parse_double).map(singleton);

    // let mut test = parslers_lib::code_gen::compile("test", test, context);

    // test.parser = test.parser.reduce_down();

    p.parser = p.parser.reduce();
    p.parser
        .output_used_analysis(true, &mut context.parsers_with_unused);

    p_validate.parser = p_validate.parser.reduce();
    p_validate
        .parser
        .output_used_analysis(true, &mut context.parsers_with_unused);

    brainfuck.parser = brainfuck.parser.reduce();
    brainfuck
        .parser
        .output_used_analysis(true, &mut context.parsers_with_unused);

    brainfuck_validate
        .parser
        .output_used_analysis(true, &mut context.parsers_with_unused);
    brainfuck_validate.parser = brainfuck_validate.parser.reduce();

    context.optimise_named_parsers();

    let parslers_out = parslers_lib::code_gen::gen_statement(
        &[p, p_validate, brainfuck, brainfuck_validate],
        context,
    )
    .to_string();

    // wow

    // Write the combinators file to the output directoryas
    std::fs::write(format!("{}/combinators.rs", out_dir), parslers_out).unwrap();
}
