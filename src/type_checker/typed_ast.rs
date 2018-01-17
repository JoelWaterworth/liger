use parsing::ast;
use std::collections::{HashMap};
use std::hash::{Hasher};
use super::enviroment::Variable;

#[derive(Clone, Debug)]
pub struct Globals {
    pub functions: HashMap<String, Function>,
    pub structs: HashMap<String, Struct>,
    pub enums: HashMap<String, Enum>,
}

pub type FunctionType = (Vec<Type>, Type);

#[derive(Clone, Debug)]
pub enum Type {
    Int,
    Function(Box<FunctionType>),
    Struct(String),
    Unit,
    Bool,
    Enum(String),
    Array(Box<Type>, usize),
    Cell(Box<Type>),
    Module,
}

impl<'a> From<&'a Type> for String {
    fn from(ty: &Type) -> Self {
        match ty {
            &Type::Int => String::from("i64"),
            &Type::Unit => String::from("void"),
            &Type::Struct(ref n) => format!("%{}", n),
            x => panic!("{:?}", x),
        }
    }
}

#[allow(dead_code)]
fn extract_from_cell(ty: &Type) -> Type {
    match ty {
        &Type::Cell(ref x) => {
            let t = (**x).clone();
            match t {
                Type::Cell(_) => panic!(""),
                _ => {}
            }
            t
        },
        x => x.clone()
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Type) -> bool {
        match (self, other) {
            (&Type::Function(ref left), &Type::Function(ref right)) => left == right,
            (&Type::Struct(ref left),   &Type::Struct(ref right))   => left == right,
            (&Type::Enum(ref left),     &Type::Enum(ref right))     => left == right,
            (&Type::Cell(ref left),     &Type::Cell(ref right))     => left == right,
            (&Type::Cell(ref left),     right)                      => left.as_ref() == right,
            (left,                      &Type::Cell(ref right))     => left == right.as_ref(),
            (&Type::Int,                &Type::Int)                 => true,
            (&Type::Unit,               &Type::Unit)                => true,
            (&Type::Bool,               &Type::Bool)                => true,
            _                                                       => false
        }
    }
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

#[derive(Debug, Clone)]
pub struct Enum {
    pub name: String,
    pub members: Vec<(String, Vec<Type>)>,
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

#[derive(Debug, Clone)]
pub struct Function {
    pub var: Variable,
    pub cases: Body
}

impl PartialEq for Function {
    fn eq(&self, other: &Function) -> bool {
        self.var.id == other.var.id
    }
}

#[derive(Clone, Debug)]
pub enum Body {
    BuiltInFunc(String),
    Cases(Vec<Case>),
    Declare,
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
        l_expr: Box<LExpr>,
        expr: Box<Expr>
    },
    If{
        condition: Box<Expr>,
        true_statements: Vec<Statement>,
        false_statements: Vec<Statement>
    },
    Let{
        var: Variable,
        expr: Box<Expr>
    },
    FunctionCall(Box<Expr>, Vec<Expr>, Type),
    MethodCall(Box<Expr>, String, Vec<Expr>),
    Return{
        ty: Type,
        expr: Box<Expr>
    },
}

#[derive(Clone, Debug)]
pub enum Expr {
    Var(Variable),
    BinaryExpr(ast::BinaryOperator, Box<Expr>, Box<Expr>, Type),
    UnaryExpr(ast::UnaryOperator, Box<Expr>, Type),
    Lambda(Vec<String>, Box<Expr>, Type),
    App(Box<Expr>, Vec<Expr>, Type),
    If(Box<Expr>, Vec<Statement>, Vec<Statement>, Type),
    Case(Box<Expr>, Vec<(Pattern, Box<Expr>)>, Type),
    Lit(ast::Lit, Type),
    MethodCall(Box<Expr>, Type, String, Vec<Expr>, Type),
    StructInit(Type, Vec<(String, Expr)>),
    EnumInit(Type, String, Vec<Expr>),
    SliceInit(Vec<Expr>, Type),
    Index(Box<Expr>, Box<Expr>, Type)
}

impl Expr {
    pub fn get_type(&self) -> Type {
        match self {
            &Expr::Var(ref var) => var.ty.as_ref().clone(),
            &Expr::BinaryExpr(_, _, _, ref ty) => ty.clone(),
            &Expr::UnaryExpr(_, _, ref ty) => ty.clone(),
            &Expr::Lambda(_, _, ref ty) => ty.clone(),
            &Expr::App(_, _, ref ty) => ty.clone(),
            &Expr::If(_, _, _, ref ty) => ty.clone(),
            &Expr::Case(_, _, ref ty) => ty.clone(),
            &Expr::Lit(_, ref ty) => ty.clone(),
            &Expr::MethodCall(_, _, _, _, ref ty) => ty.clone(),
            &Expr::StructInit(ref ty, _) => ty.clone(),
            &Expr::EnumInit(ref ty, _, _) => ty.clone(),
            &Expr::Index(_, _, ref ty) => ty.clone(),
            x => panic!("{:?}", x),
        }
    }
}

#[derive(Clone, Debug)]
pub enum LExpr {
    Var(Variable),
    MethodCall(Box<LExpr>, String, Vec<Expr>),
}

#[derive(Clone, Debug)]
pub struct Struct {
    pub var: Variable,
    pub args: HashMap<String, (Type, i32)>,
    pub methods: HashMap<String, Function>,
}

impl PartialEq for Struct {
    fn eq(&self, other: &Struct) -> bool {
        self.var.id == other.var.id
    }
}

impl Eq for Struct {}