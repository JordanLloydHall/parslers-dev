#[macro_use]
extern crate criterion;

#[global_allocator]
static ALLOC: jemallocator::Jemalloc = jemallocator::Jemalloc;

use ::combine::{stream::position, EasyParser};
use criterion::{black_box, Criterion, Throughput};
use parslers_benchmark::{combine, nom};

use pest_grammars::json::*;

use pest::Parser;

mod debug_parser {
    #![allow(warnings, unused)]

    include!(concat!(env!("OUT_DIR"), "/combinators.rs"));
}

static CANADA: &str = include_str!("../canada.json");
fn canada_json(c: &mut Criterion) {
    // test once to make sure it parses correctly
    nom::json(CANADA).unwrap();
    debug_parser::json(&mut CANADA.chars()).unwrap();
    debug_parser::json_validate(&mut CANADA.chars()).unwrap();
    combine::json_value()
        .easy_parse(position::Stream::new(&CANADA[..]))
        .unwrap();

    let mut group = c.benchmark_group("json canada");

    group.throughput(Throughput::Bytes(CANADA.as_bytes().len() as u64));

    // println!("data:\n{:?}", json(data));
    group.bench_function("nom", |b| {
        b.iter(|| nom::json(black_box(CANADA)).unwrap());
    });

    group.bench_function("parslers", |b| {
        b.iter(|| debug_parser::json(black_box(&mut CANADA.chars())).unwrap());
    });

    group.bench_function("parslers validate", |b| {
        b.iter(|| debug_parser::json_validate(black_box(&mut CANADA.chars())).unwrap());
    });

    group.bench_function("serde-json", |b| {
        b.iter(|| serde_json::from_str::<serde_json::Value>(black_box(CANADA)).unwrap());
    });
    group.bench_function("combine", |b| {
        b.iter(|| {
            combine::json_value()
                .easy_parse(position::Stream::new(&CANADA[..]))
                .unwrap()
        });
    });

    group.bench_function("pest", |b| {
        b.iter(|| JsonParser::parse(Rule::json, CANADA).unwrap())
    });

    group.finish();
}

criterion_group!(benches, canada_json);
criterion_main!(benches);
