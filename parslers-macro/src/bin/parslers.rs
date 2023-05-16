// parsler!(
//     let parker = (('a''b') | ('a''c'))
// );

fn is_a(c: char) -> bool {
    c == 'a'
}

// let char: char = |a: char| then(satisfy(|c| c == a), pure(val(a)))
// parser! {
//     pub let a: char = then(satisfy(is_a), pure(val('a')));
// }

fn main() {
    // println!("{:?}", parsler("ab"));
    // println!("{:?}", a("a"));
}

// #[cfg(test)]
// mod tests {
//     use super::*;

//     #[test]
//     fn it_works() {
//         assert_eq!(parsler("ab"), Ok((('a', 'b'), "")));
//     }
// }
