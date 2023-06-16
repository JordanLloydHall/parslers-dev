//! This module defines the types for the AST structure that is produced by the
//! parser and consumed by the semantic analyser.
//!
//! References to the source code represented in the tree are kept to allow for
//! beautiful error messages from the semantic analyser.
//!
//! The AST is generic for variable identifiers, this allows for renaming to
//! occur (as is required for our symbol table generation).
//!
//! Function identifiers are contained withing strings, storing both the name
//! and place in the source code.
//!
//! WrapSpans are extensively used to associate parts of the AST with spans from
//! the source code.
//! 
#![recursion_limit = "512"]


// use parslers_macro::Reflect;
use parslers_lib::reflect::Reflect;
use parslers_macro::Reflected;

#[derive(Reflected)]
struct Foo<T: Reflect> {
    bar: T,
    buzz: String,
}

pub mod parser;

/// A wrapper for AST nodes containing the span of the original code for which
/// the wrapped structure represents.

#[derive(Clone, Debug, Hash, PartialEq, Eq, Reflected)]
pub struct ASTWrapper<W: Reflect, T: Reflect>(pub W, pub T);

/// Identifier for the generic type, used to differentiate between
/// different generic types.
pub type GenericId = u64;

/// Type specification in WACC.
#[derive(Clone, Debug, Hash, PartialEq, Eq, Reflected)]
pub enum Type {
    /// Void function type
    Void,

    /// Integer primitive type.
    Int,

    /// Boolean primitive type.
    Bool,

    /// Character primitive type.
    Char,

    /// String primitive type.
    String,

    /// Any type (matches all). Used for type coercion
    /// ```text
    /// pair(pair, pair)
    /// ```
    /// ```
    /// Type::Pair(box Type::Pair(box Type::Any, box Type::Any), box Type::Pair(box Type::Any, box Type::Any))
    /// ```
    Any,

    /// Used as a generic type. alone it matches any, but can be used by binary
    /// operators to specify many Any types that must match.
    ///
    /// e.g equality (== and !=) are of type:
    /// - Output:      Bool
    /// - Left input:  Generic(0)
    /// - Right input: Generic(0)
    ///
    /// e.g if we introduced an expression for combining pairs (:=:)
    /// - Output:      Pair(Generic(0), Generic(1))
    /// - Left input:  Pair(Generic(0), Generic(1))
    /// - Right input: Pair(Generic(0), Generic(1))
    Generic(GenericId),

    /// Typed pair type. The arguments to the nameless tuple are the types of
    /// the first and second field of the pair respectively.
    /// ```text
    /// pair(int, bool)
    /// pair(int[], pair(char, char)[])
    /// ```
    /// ```
    /// // First type:
    /// Type::Pair(box Type::Int, box Type::Bool)
    ///
    /// // Second type:
    /// Type::Pair(
    ///     box Type::Array(box Type::Int, 1),
    ///     box Type::Array(box Type::Pair(box Type::Char, box Type::Char), 1),
    /// )
    /// ```
    Pair(Box<Type>, Box<Type>),

    /// Multidimentional array. The first argument is the type of elements in
    /// the arrays. The second argument is the dimensionality of the array.
    ///
    /// **Note**: For multidimensional arrays, the type is the element type,
    /// and the size is the number of dimensions.
    /// ```text
    /// int[][][]
    /// ```
    /// ```
    /// Type::Array(box Type::Int, 3)
    /// ```
    Array(Box<Type>, usize),
}

/// All unary operators supported by WACC and used in [expressions](Expr).
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, Reflected)]
pub enum UnOp {
    /// Logical negation
    Neg,
    /// Integer negation
    Minus,
    /// Gets the length of an array as an integer.
    Len,
    /// Gets the ascii value associated with a character as an integer.
    Ord,
    /// Gets the character associated with a given integer ascii value.
    Chr,
    /// Getting first from a pair
    Fst,
    /// Getting second from a pair
    Snd,
}

/// Binary Operators supported by WACC available for use in [expressions](Expr).
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, Reflected)]
pub enum BinOp {
    /// Integer addition (+).
    Add,
    /// Integer subtraction(-).
    Sub,
    /// Integer multiplication (*).
    Mul,
    /// Integer division (/).
    Div,
    /// Integer modulus.
    Mod,
    /// Comparison greater-than (>).
    Gt,
    /// Comparison greater-than or equal (>=).
    Gte,
    /// Comparison less-than (<).
    Lt,
    /// Comparison less-than or equal (<=).
    Lte,
    /// Comparison equality (==).
    Eq,
    /// Comparison not equal (!=).
    Ne,
    /// Logical conjunction/and (&&).
    And,
    /// Logical disjunction/or (||).
    Or,
    /// Create a pair from two values
    Newpair,
}

/// Expression Data Type
///
/// # Examples:
/// ## Example of nested binary operators
/// ```text
/// (4 - 3) + (9 * 7)
/// ```
/// ```
/// WrapSpan(
///     "chr ((1 + 1) + (3 * -2))",
///     Expr::UnOp(
///         UnOp::Chr,
///         box WrapSpan(
///             "(1 + 1) + (3 * -2)",
///             Expr::BinOp(
///                 box WrapSpan(
///                     "1 + 1",
///                     Expr::BinOp(
///                         box WrapSpan("1", Expr::Int(1)),
///                         BinOp::Mul,
///                         box WrapSpan("1", Expr::Int(1)),
///                     ),
///                 ),
///                 BinOp::Add,
///                 box WrapSpan(
///                     "3 * -2",
///                     Expr::BinOp(
///                         box WrapSpan("3", Expr::Int(3)),
///                         BinOp::Mul,
///                         box WrapSpan(
///                             "-2",
///                             Expr::UnOp(UnOp::Minus, box WrapSpan("2", Expr::Int(2))),
///                         ),
///                     ),
///                 ),
///             ),
///         ),
///     ),
/// )
/// ```  
#[derive(Clone, Debug, Hash, PartialEq, Eq, Reflected)]
pub enum Expr<W: Reflect, IdRepr: Reflect> {
    /// null value that can be used for pair values.
    /// ```text
    /// pair(int, int) example = null
    /// ```
    /// ```
    /// WrapSpan(
    ///     "pair(int, int) example = null",
    ///     Stat::Def(
    ///         Type::Pair(box Type::Int, box Type::Int),
    ///         "example",
    ///         AssignRhs::Expr(WrapSpan("null", Expr::Null)),
    ///     ),
    /// )
    /// ```
    Null,

    /// Integer constants
    /// ```text
    /// int a = 9
    /// ```
    /// ```
    /// WrapSpan(
    ///     "int a = 9",
    ///     Stat::Def(
    ///         Type::Int,
    ///         "a",
    ///         AssignRhs::Expr(WrapSpan("9", Expr::Int(9))),
    ///     ),
    /// );
    /// ```
    Int(i32),

    /// Boolean constants
    /// ```text
    /// bool a = true
    /// ```
    /// ```
    /// WrapSpan(
    ///     "bool a = true",
    ///     Stat::Def(
    ///         Type::Bool,
    ///         "a",
    ///         AssignRhs::Expr(WrapSpan("true", Expr::Bool(true))),
    ///     ),
    /// );
    /// ```
    Bool(bool),

    /// Character constants
    /// ```text
    /// char a = 'a'
    /// ```
    /// ```
    /// WrapSpan(
    ///     "char a = 'a'",
    ///     Stat::Def(
    ///         Type::Char,
    ///         "a",
    ///         AssignRhs::Expr(WrapSpan("'a'", Expr::Char('a'))),
    ///     ),
    /// );
    /// ```
    Char(char),

    /// String Constants
    /// ```text
    /// string a = "hello world"
    /// ```
    /// ```
    /// WrapSpan(
    ///     "string a = \"hello world\"",
    ///     Stat::Def(
    ///         Type::String,
    ///         "a",
    ///         AssignRhs::Expr(WrapSpan("\"hello world\"", Expr::String(String::from("hello world")))),
    ///     ),
    /// );
    /// ```
    String(String),

    /// Variable reference
    /// ```text
    /// int a = 7 ;
    /// int b = a;
    /// ```
    /// ```
    /// vec![
    ///     WrapSpan(
    ///         "int a = 7",
    ///         Stat::Def(Type::Int, "a", AssignRhs::Expr(WrapSpan("7", Expr::Int(7)))),
    ///     ),
    ///     WrapSpan(
    ///         "int b = a",
    ///         Stat::Def(
    ///             Type::Int,
    ///             "b",
    ///             AssignRhs::Expr(WrapSpan("a", Expr::Var("a"))),
    ///         ),
    ///     ),
    /// ];
    /// ```
    Var(IdRepr),

    /// Array indexing
    /// ```text
    /// # Given some int[][] b
    /// int a = b[3][5] ;
    /// ```
    /// ```
    /// WrapSpan(
    ///     "int a = b[3][5]",
    ///     Stat::Def(
    ///         Type::Int,
    ///         "a",
    ///         AssignRhs::Expr(WrapSpan(
    ///             "b[3][5]",
    ///             Expr::ArrayElem(
    ///                 "b",
    ///                 vec![WrapSpan("3", Expr::Int(3)), WrapSpan("5", Expr::Int(5))],
    ///             ),
    ///         )),
    ///     ),
    /// )
    /// ```
    ArrayElem(IdRepr, Vec<ExprWrap<W, IdRepr>>),

    /// Unary operator application determined by the [UnOp enum](UnOp).
    /// ```text
    /// bool a = !true ;
    /// ```
    /// ```
    /// WrapSpan(
    ///     "bool a = true",
    ///     Stat::Def(
    ///         Type::Bool,
    ///         "a",
    ///         AssignRhs::Expr(WrapSpan(
    ///             "true",
    ///             Expr::UnOp(UnOp::Neg, box WrapSpan("true", Expr::Bool(true))),
    ///         )),
    ///     ),
    /// )
    /// ```
    UnOp(UnOp, Box<ExprWrap<W, IdRepr>>),

    /// Binary operator application determined by the [BinOp enum](BinOp).
    /// ```text
    /// int a = 3 + 7 ;
    /// ```
    /// ```
    /// WrapSpan(
    ///     "int a = 3 + 7",
    ///     Stat::Def(
    ///         Type::Int,
    ///         "a",
    ///         AssignRhs::Expr(WrapSpan(
    ///             "3 + 7",
    ///             Expr::BinOp(
    ///                 box WrapSpan("3", Expr::Int(3)),
    ///                 BinOp::Add,
    ///                 box WrapSpan("7", Expr::Int(7)),
    ///             ),
    ///         )),
    ///     ),
    /// )
    /// ```
    BinOp(Box<ExprWrap<W, IdRepr>>, BinOp, Box<ExprWrap<W, IdRepr>>),
}

/// Alias for WarpSpans around expressions
pub type ExprWrap<W, IdRepr> = ASTWrapper<W, Expr<W, IdRepr>>;

/// Lefthand side of assignments.
/// ```text
/// AssignLhs = AssignRhs ;
/// ```
#[derive(Clone, Debug, Hash, PartialEq, Eq, Reflected)]
pub enum AssignLhs<W: Reflect, IdRepr: Reflect> {
    /// Variable assignment.
    /// ```text
    /// int a = 3 ;
    /// ```
    Var(IdRepr),

    /// Array element assignment.
    /// ```text
    /// int[] a = [1,2,3,4] ;
    /// a[0] = 9 ;
    /// ```
    ArrayElem(IdRepr, Vec<ExprWrap<W, IdRepr>>),

    /// Assign to the first element of a pair
    /// ```text
    /// pair(int, bool) a = newpair(1, true) ;
    /// fst a = 3 ;
    /// ```
    PairFst(ExprWrap<W, IdRepr>),

    /// Assign to the second element of a pair
    /// ```text
    /// pair(int, bool) a = newpair(1, true) ;
    /// snd a = false ;
    /// ```
    PairSnd(ExprWrap<W, IdRepr>),
}

/// Righthand side of assignments.
/// ```text
/// AssignLhs = AssignRhs ;
/// ```
#[derive(Clone, Debug, Hash, PartialEq, Eq, Reflected)]
pub enum AssignRhs<W: Reflect, IdRepr: Reflect> {
    /// Assigns an expression.
    /// ```text
    /// int a = 3 + (4 * 5) ;
    /// ```
    Expr(ExprWrap<W, IdRepr>),

    /// Array assignment by expressions of the same type.
    /// ```text
    /// int[] int_arr = [2, 3 + 3, 4 * 7, 0] ;
    /// ```
    Array(ArrayWrap<W, IdRepr>),

    /// Assign the return value of a function, with arguments.
    /// ```text
    /// int add(int a, int b) is
    ///     return a + b
    /// end
    ///
    /// int a = add(3,4) ;
    /// ```
    Call(ASTWrapper<W, String>, Vec<ExprWrap<W, IdRepr>>),
}

/// Alias for WrapSpans around array literals.
type ArrayWrap<W, IdRepr> = ASTWrapper<W, Vec<ExprWrap<W, IdRepr>>>;

/// Statements for assignment, control flow and definitions.
#[derive(Clone, Debug, Hash, PartialEq, Eq, Reflected)]
pub enum Stat<W: Reflect, IdRepr: Reflect> {
    /// A _no-operation_ instruction.
    Skip,

    /// Variable Definition
    /// ```text
    /// int a = 99;
    /// ```
    Def(Type, IdRepr, AssignRhs<W, IdRepr>),

    /// Assignment (to variable, parts of pairs or arrays).
    /// ```text
    /// int a = 9 ;
    /// ```
    /// ```text
    /// int[] arr = [1,2,3,4] ;
    /// a[1] = call function(3, 4) ;
    /// ```
    Assign(AssignLhs<W, IdRepr>, AssignRhs<W, IdRepr>),

    /// Read from standard input.
    /// ```text
    /// int a = 9 ;
    /// read a;
    /// ```
    Read(AssignLhs<W, IdRepr>),

    /// Free dynamically allocated data structures.
    /// ```text
    /// pair(int, int) a_pair = newpair(3,3) ;
    /// free a_pair ;
    /// ```
    Free(ExprWrap<W, IdRepr>),

    /// Return value from a subroutine/function.
    /// ```text
    /// int double(int b) is
    ///     return 2 * b
    /// end
    /// ```
    Return(Option<ExprWrap<W, IdRepr>>),

    /// End the program and return the exit code.
    /// ```text
    /// exit 0 ;
    /// ```
    Exit(ExprWrap<W, IdRepr>),

    /// Print text to the console.
    /// ```text
    /// int a = 'a' ;
    /// print a ;
    /// ```
    Print(ExprWrap<W, IdRepr>),

    /// Print text to the console and start a new line.
    /// ```text
    /// int a = 'a' ;
    /// print a + 9 ;
    /// ```
    PrintLn(ExprWrap<W, IdRepr>),

    /// If-Else statement to alter control flow.
    /// ```text
    ///
    /// if 3 = 4 - 1
    /// then
    ///     print 'a'
    /// else
    ///     print 'b'
    /// fi ;
    /// ```
    If(
        ExprWrap<W, IdRepr>,
        Vec<StatWrap<W, IdRepr>>,
        Vec<StatWrap<W, IdRepr>>,
    ),

    /// While Loop control flow structure.
    /// ```text
    /// int n = 0 ;
    /// while n < 10 do
    ///     n = n + 1
    /// done ;
    /// ```
    While(ExprWrap<W, IdRepr>, Vec<StatWrap<W, IdRepr>>),

    /// Code block to include multiple statements
    /// ```text
    /// begin
    ///     int a = 4;
    ///     a = 120
    /// end ;
    /// ```
    Block(Vec<StatWrap<W, IdRepr>>),

    /// Void function call
    /// ```text
    /// begin
    ///     call print_hello_world()
    /// end ;
    /// ```
    VoidCall(ASTWrapper<W, String>, Vec<ExprWrap<W, IdRepr>>),
}

/// Alias for WarpSpans around statements
pub type StatWrap<W, IdRepr> = ASTWrapper<W, Stat<W, IdRepr>>;

/// Formal parameter used in functions, containing the type and identifier.
#[derive(Debug, PartialEq, Eq, Reflected, Clone)]
pub struct Param<IdRepr: Reflect>(pub Type, pub IdRepr);

/// Function definition with the return type, name, parameters, and code body.
/// ```text
/// int sum(int a, int, b) is
///     return a + b
/// end
/// ```
/// ```
/// WrapSpan(
///     "int sum(int a, int, b)",
///     Function(
///         Type::Int,
///         "sum",
///         vec![
///             WrapSpan("int a", Param(Type::Int, "a")),
///             WrapSpan("int b", Param(Type::Int, "b")),
///         ],
///         vec![WrapSpan(
///             "return a + b",
///             Stat::Return(WrapSpan(
///                 "a + b",
///                 Expr::BinOp(
///                     box WrapSpan("a", Expr::Var("a")),
///                     BinOp::Add,
///                     box WrapSpan("b", Expr::Var("b")),
///                 ),
///             )),
///         )],
///     ),
/// )
/// ```
#[derive(Debug, PartialEq, Eq, Reflected, Clone)]
pub struct Function<W: Reflect, IdRepr: Reflect>(
    pub Type,
    pub ASTWrapper<W, String>,
    pub Vec<ASTWrapper<W, Param<IdRepr>>>,
    pub Vec<StatWrap<W, IdRepr>>,
);

/// Alias for WrapSpans around functions.
pub type FunWrap<W, IdRepr> = ASTWrapper<W, Function<W, IdRepr>>;

/// Program is the root of the abstract syntax tree, containing all function
/// definitions and the main program body, with all nested structures being
/// associated with the original source code by [wrappers](ASTWrapper).
#[derive(Debug, Reflected, Clone)]
pub struct Program<W: Reflect, IdRepr: Reflect>(pub Vec<FunWrap<W, IdRepr>>, pub Vec<StatWrap<W, IdRepr>>);
