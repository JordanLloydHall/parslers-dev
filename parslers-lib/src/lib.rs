pub mod ast {
    #[derive(Debug, PartialEq, Clone)]
    pub struct Spec {
        pub statements: Vec<Statement>,
    }

    #[derive(Debug, PartialEq, Clone)]
    pub struct Statement {
        pub public: bool,
        pub ident: String,
        pub type_: syn::Type,
        pub parser: Parser,
    }

    #[derive(Debug, PartialEq, Clone)]
    pub enum Parser {
        Ident(String),
        Pure(PureVal),
        Satisfy(Func),
        Try(Box<Parser>),
        Look(Box<Parser>),
        NegLook(Box<Parser>),
        Ap(Box<Parser>, Box<Parser>),
        Then(Vec<Parser>),
        Before(Vec<Parser>),
        Or(Vec<Parser>),
        Empty,
        Branch(Box<Parser>, Box<Parser>, Box<Parser>),
    }

    #[derive(Debug, PartialEq, Clone)]
    pub enum PureVal {
        Val(String),
        Func(Func),
    }

    #[derive(Debug, PartialEq, Clone)]
    pub struct Func {
        pub ident: syn::Expr,
    }
}
