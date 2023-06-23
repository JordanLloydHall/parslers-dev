mod branflakes {
    #![allow(warnings, unused)]
    include!(concat!(env!("OUT_DIR"), "/branflakes.rs"));
}

fn main() {
    let data = std::fs::read_to_string("LostKng.b").unwrap();

    let chars = &mut data.chars();
    let parsed = branflakes::branflakes(chars);
    println!("Output: {:?}", parsed.is_ok());
    println!("Remainder: {:?}", chars.as_str());

    let chars = &mut data.chars();
    let validate = branflakes::branflakes_validate(chars);
    println!("Validate: {:?}", validate.is_ok());
    println!("Remainder: {:?}", chars.as_str());
}
