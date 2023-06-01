use parslers_lib::reflect::Reflect;

#[derive(Clone, Debug)]

pub struct BrainfuckProgram(pub Vec<Brainfuck>);

#[derive(Clone, Debug)]

pub enum Brainfuck {
    Add,
    Sub,
    Left,
    Right,
    Read,
    Print,
    Loop(BrainfuckProgram),
}
// Brainfuck::Null => "Null".to_owned(),
// Brainfuck::Bool(b) => format!("Bool({})", b.reflect()),
// Brainfuck::Number(n) => format!("Number({})", n.reflect()),
// Brainfuck::String(s) => format!("String({})", s.reflect()),
// Brainfuck::Array(a) => format!("Array({})", a.reflect()),
// Brainfuck::Object(o) => format!("Object({})", o.reflect()),

impl Reflect for BrainfuckProgram {
    fn reflect(&self) -> String {
        format!("{}({})", std::any::type_name::<Self>(), self.0.reflect())
    }
}

impl Reflect for Brainfuck {
    fn reflect(&self) -> String {
        format!(
            "{}::{}",
            std::any::type_name::<Self>(),
            match self {
                Brainfuck::Add => "Add".to_owned(),
                Brainfuck::Sub => "Sub".to_owned(),
                Brainfuck::Left => "Left".to_owned(),
                Brainfuck::Right => "Right".to_owned(),
                Brainfuck::Read => "Read".to_owned(),
                Brainfuck::Print => "Print".to_owned(),
                Brainfuck::Loop(l) => format!("Loop({})", l.reflect()),
            }
        )
    }
}
