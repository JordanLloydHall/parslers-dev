mod combinators {
    include!(concat!(env!("OUT_DIR"), "/combinators.rs"));
}

fn main() {
    println!("Hello, world!, {}", combinators::is_a('c'))
}
