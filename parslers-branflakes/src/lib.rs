use parslers_lib::reflect::Reflect;
use parslers_macro::*;

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

pub fn branflakes_parser(input: &str) -> Result<BrainfuckProgram, String> {
    let mut stack = vec![];
    let mut curr_vec = vec![];
    for c in input.chars() {
        match c {
            '+' => curr_vec.push(Brainfuck::Add),
            '-' => curr_vec.push(Brainfuck::Sub),
            '.' => curr_vec.push(Brainfuck::Print),
            ',' => curr_vec.push(Brainfuck::Read),
            '<' => curr_vec.push(Brainfuck::Left),
            '>' => curr_vec.push(Brainfuck::Right),
            '[' => {
                stack.push(curr_vec);
                curr_vec = vec![];
            }
            ']' => {
                let last = curr_vec;
                let top_of_stack = stack.pop().ok_or("Missing '['".to_owned())?;
                curr_vec = top_of_stack;
                curr_vec.push(Brainfuck::Loop(BrainfuckProgram(last)));
            }
            _ => return Err("Encountered invalid char".to_owned()),
        }
    }

    Ok(BrainfuckProgram(curr_vec))
}

pub fn branflakes_parser_validate(input: &str) -> Result<(), String> {
    let mut nested_level = 0;
    for c in input.chars() {
        match c {
            '+' |
            '-' |
            '.' |
            ',' |
            '<' |
            '>' => {}
            '[' => {
                nested_level += 1
            }
            ']' => {
                nested_level -= 1;
            }
            _ => return Err("Encountered invalid char".to_owned()),
        }
    }

    if nested_level == 0 {
        Ok(())
    } else {
        Err("Missing ']'".to_owned())
    }
}
