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

use parslers_branflakes::Branflakes;
use parslers_lib::{
    builder::Builder,
    parsler::{auxiliary::*, *},
    reflect::*,
};
use parslers_macro::reflect;

#[reflect]
fn branflakes_val(
    p: Vec<parslers_branflakes::Branflakes>,
) -> parslers_branflakes::BranflakesProgram {
    parslers_branflakes::BranflakesProgram(p)
}
#[reflect]
fn branflakes_loop(p: parslers_branflakes::BranflakesProgram) -> parslers_branflakes::Branflakes {
    parslers_branflakes::Branflakes::Loop(p)
}

fn branflakes_program() -> impl Parsler<Output = parslers_branflakes::BranflakesProgram> + Clone {
    let left = match_char('<').then(pure(Branflakes::Left));
    let right = match_char('>').then(pure(Branflakes::Right));
    let add = match_char('+').then(pure(Branflakes::Add));
    let sub = match_char('-').then(pure(Branflakes::Sub));
    let print = match_char('.').then(pure(Branflakes::Print));
    let read = match_char(',').then(pure(Branflakes::Read));
    let loop_ = || {
        match_char('[')
            .then(branflakes_program())
            .map(branflakes_loop)
            .before(match_char(']'))
    };
    name("branflakes", move || {
        many(
            left.or(right)
                .or(add)
                .or(sub)
                .or(print)
                .or(read)
                .or(loop_()),
        )
        .map(branflakes_val)
    })
}

fn main() {
    let out_dir = std::env::var("OUT_DIR").unwrap();
    Builder::new("branflakes", branflakes_program())
        .add_parser("branflakes_validate", branflakes_program().then(pure(())))
        .reduce()
        .usage_analysis()
        .build(&format!("{out_dir}/branflakes.rs"));
}
