use parsing::ast;
use std::collections::{HashSet, HashMap};
use std::hash::{Hasher, Hash};

pub struct Globals {
    pub functions: HashSet<Function>,
    pub structs: HashSet<Struct>
}

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Int,
    Function(Function),
    Struct(String),
    Unit,
    Bool,
}
impl Eq for Type {}

#[derive(Clone, Debug)]
pub struct ConstVal {
    pub ty: Type,
    pub val: ast::Lit
}

impl PartialEq for ConstVal {
    fn eq(&self, other: &ConstVal) -> bool {
        self.val == other.val
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Pattern {
    Wild,
    Binding {
        name: String,
        ty: Type,
    },
    Constant {
        value: ConstVal
    }
}

#[derive(Clone, Debug)]
pub struct Function {
    pub name: String,
    pub args_ty: Vec<Type>,
    pub ret_ty: Box< Type>,
    pub cases: Body
}

impl PartialEq for Function {
    fn eq(&self, other: &Function) -> bool {
        self.name == other.name
    }
}

#[derive(Clone, Debug)]
pub enum Body {
    BuiltInFunc(String),
    Cases(Vec<Case>)
}

impl Hash for Function {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

impl Eq for Function {}

#[derive(Clone, Debug)]
pub struct Case {
    pub args: Vec<Pattern>,
    pub statements: Vec<Statement>
}

#[derive(Clone, Debug)]
pub enum Statement {
    Assignment{
        ty: Type,
        name: String,
        expr: Box<Expr>
    },
    If{
        condition: Box<Expr>,
        true_statements: Vec<Statement>,
        false_statements: Vec<Statement>
    },
    Let{
        ty: Type,
        name: String,
        expr: Box<Expr>
    },
    FunctionCall(Box<Expr>, Vec<Expr>),
    MethodCall(Box<Expr>, String, Vec<Expr>),
    Return{
        ty: Type,
        expr: Box<Expr>
    },
}

#[derive(Clone, Debug)]
pub enum Expr {
    Var(String),
    BinaryExpr(ast::BinaryOperator, Box<Expr>, Box<Expr>),
    UnaryExpr(ast::UnaryOperator, Box<Expr>),
    Lambda(Vec<String>, Box<Expr>),
    App(Box<Expr>, Vec<Expr>),
    If(Box<Expr>, Vec<Statement>, Vec<Statement>),
    Case(Box<Expr>, Vec<(Pattern, Box<Expr>)>),
    Lit(ast::Lit),
    MethodCall(Box<Expr>, String, Vec<Expr>),
    StructInit(Type, Vec<(String, Expr)>),
}

#[derive(Clone, Debug)]
pub struct Struct {
    pub name: String,
    pub args: HashMap<String, Type>,
    pub methods: HashMap<String, Function>,
}

impl Hash for Struct {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

impl PartialEq for Struct {
    fn eq(&self, other: &Struct) -> bool {
        self.name == other.name
    }
}

impl Eq for Struct {}