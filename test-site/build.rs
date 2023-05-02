fn main() {
    println!("cargo:rerun-if-changed=combinators.rs");

    let out_dir = std::env::var("OUT_DIR").unwrap();

    // Read the combinators file
    let combinators = std::fs::read_to_string("combinators.rs").unwrap();

    // Parse the combinators file
    // let combinators = parser::parse_spec(combinators.into());

    // Write the combinators file to the output directory
    std::fs::write(format!("{}/combinators.rs", out_dir), combinators).unwrap();
}
