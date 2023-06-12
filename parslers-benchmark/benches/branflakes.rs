#[macro_use]
extern crate criterion;

#[global_allocator]
static ALLOC: jemallocator::Jemalloc = jemallocator::Jemalloc;

use ::combine::{stream::position, EasyParser};
use criterion::{black_box, Criterion, Throughput};
use parslers_benchmark::{combine, nom};

mod debug_parser {
    #![allow(warnings, unused)]

    include!(concat!(env!("OUT_DIR"), "/combinators.rs"));
}

static LOSTKNG: &str = include_str!("../LostKng.b");
fn branflakes(c: &mut Criterion) {
    // test once to make sure it parses correctly
    nom::brainfuck(LOSTKNG).unwrap();
    debug_parser::brainfuck(&mut LOSTKNG.chars()).unwrap();
    debug_parser::json_validate(&mut LOSTKNG.chars()).unwrap();
    combine::json_value()
        .easy_parse(position::Stream::new(&LOSTKNG[..]))
        .unwrap();

    let mut group = c.benchmark_group("branflakes");

    group.throughput(Throughput::Bytes(LOSTKNG.as_bytes().len() as u64));

    group.bench_function("nom", |b| {
        b.iter(|| nom::brainfuck(black_box(LOSTKNG)).unwrap());
    });

    group.bench_function("parslers", |b| {
        b.iter(|| debug_parser::brainfuck(black_box(&mut LOSTKNG.chars())).unwrap());
    });

    group.bench_function("parslers validate", |b| {
        b.iter(|| debug_parser::brainfuck_validate(black_box(&mut LOSTKNG.chars())).unwrap());
    });

    group.finish();
}

criterion_group!(benches, branflakes);
criterion_main!(benches);
