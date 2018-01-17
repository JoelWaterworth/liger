use std::collections::HashMap;
use super::typed_ast::Type;

#[derive(Debug, Clone)]
pub struct Environment {
    variable_number: u64,
    scopes: Vec<HashMap<String, Variable>>,
}

impl Environment {
    pub fn new() -> Self {
        Self{scopes: vec![HashMap::new()], variable_number: 0}
    }
    pub fn new_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }
    pub fn get(&self, k: &String) -> Option<Variable> {
        for map in self.scopes.iter().rev() {
            match map.get(k) {
                Some(val) => return Some(val.clone()),
                None => {},
            }
        }
        return None;
    }
    pub fn insert(&mut self, k: String, val: Variable) -> Option<Variable> {
        self.scopes.last_mut().unwrap().insert(k, val)
    }
    pub fn up_scope(&mut self) {
        self.scopes.pop();
    }
    pub fn top_scope(&mut self) {
        self.scopes.resize(1, HashMap::new());
    }
    pub fn new_num(&mut self) -> u64 {
        self.variable_number += 1;
        self.variable_number
    }
}

#[derive(Debug, Clone)]
pub enum DeclaredIN {
    Local,
    Argument,
    Module,
    Global,
    Extern,
    Link,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Id {
    Number(u64),
    Name(String),
}

impl<'a> From<&'a Id> for String {
    fn from(ty: &Id) -> String {
        match ty {
            &Id::Number(ref x) => format!("{}", x),
            &Id::Name(ref x) => x.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub id: Id,
    pub dec: DeclaredIN,
    pub ty: Box<Type>,
}

impl Variable {
    pub fn init(id: Id, dec: DeclaredIN, ty: Type) -> Self {
        Self { id, dec, ty: box ty}
    }
    pub fn to_string(&self) -> String {
        match self.id {
            Id::Number(ref x) => match self.dec {
                DeclaredIN::Local => format!("loc{}", x),
                DeclaredIN::Argument => format!("arg{}", x),
                _ => panic!("{:?}", self)
            },
            Id::Name(ref x) => x.clone(),
        }
    }
}