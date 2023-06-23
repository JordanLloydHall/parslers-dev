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

use auxiliary::*;
use parslers_branflakes::Branflakes;
use parslers_lib::builder::Builder;
use parslers_lib::parsler::*;
use parslers_lib::reflect::*;
use parslers_macro::reflect;


fn main() {
    let out_dir = std::env::var("OUT_DIR").unwrap();

    // Builder::new("wacc", pure(()))
    //     .reduce()
    //     .usage_analysis()
    //     .build(&format!("{out_dir}/optimised.rs"));
}
