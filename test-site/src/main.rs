use parslers_macro::parser;


fn is_a(c: char) -> bool {
    c == 'a'
}

parser! {
    pub let a: char = char('a');
    pub let b: char = then(satisfy(|a| a == 'a'), pure(val('a')));
}

fn main() {
    println!("Hello, world!, {:?}", a("b"));

    println!("Hello, world!, {:?}", a("as"));

    println!("Hello, world!, {:?}", b("a"));
}
