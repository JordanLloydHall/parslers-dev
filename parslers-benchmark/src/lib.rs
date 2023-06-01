pub mod combine;
pub mod nom;

// mod debug_parser {
//     #![allow(warnings, unused)]

//     include!(concat!(env!("OUT_DIR"), "/combinators.rs"));
// }

#[cfg(test)]

mod tests {

    mod debug_parser {
        #![allow(warnings, unused)]

        include!(concat!(env!("OUT_DIR"), "/combinators.rs"));
    }

    use super::*;
    #[test]
    fn it_works() {
        let string = include_str!("../large-file2.json");
        let mut chars = &mut string.chars();
        let output = debug_parser::json(&mut chars);
        let remainder = chars.as_str();

        println!("{:?}", output);
        println!(
            "{}: {}",
            remainder.as_ptr() as usize - string.as_ptr() as usize,
            &remainder[..20]
        );

        assert!(output.is_ok());
    }
}
