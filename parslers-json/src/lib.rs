use std::collections::HashMap;

use parslers_lib::reflect::Reflect;

#[derive(Clone, Debug)]
pub enum Json {
    Null,
    Bool(bool),
    Number(f64),
    String(String),
    Array(Vec<Json>),
    Object(HashMap<String, Json>),
}

impl Reflect for Json {
    fn reflect(&self) -> String {
        format!(
            "{}::{}",
            std::any::type_name::<Self>(),
            match self {
                Json::Null => "Null".to_owned(),
                Json::Bool(b) => format!("Bool({})", b.reflect()),
                Json::Number(n) => format!("Number({})", n.reflect()),
                Json::String(s) => format!("String({})", s.reflect()),
                Json::Array(a) => format!("Array({})", a.reflect()),
                Json::Object(o) => format!("Object({})", o.reflect()),
            }
        )
    }
}
