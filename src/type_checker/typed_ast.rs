use parsing::ast;
use std::collections::{HashSet, HashMap};
use std::hash::{Hasher, Hash};

#[derive(Clone, Debug)]
pub struct Globals {
    pub functions: HashSet<Function>,
    pub structs: HashMap<String, Struct>
}

#[derive(Clone, Debug)]
pub enum Type {
    Int,
    Function(Function),
    Struct(String),
    Unit,
    Bool,
    Cell(Box<Type>)
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

fn extract_from_cell(ty: &Type) -> Type {
    match ty {
        &Type::Cell(ref x) => {
            let t = (**x).clone();
            match t {
                Type::Cell(ref y) => panic!(""),
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
        l_expr: Box<LExpr>,
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
    Var(String, Type),
    BinaryExpr(ast::BinaryOperator, Box<Expr>, Box<Expr>, Type),
    UnaryExpr(ast::UnaryOperator, Box<Expr>, Type),
    Lambda(Vec<String>, Box<Expr>, Type),
    App(Box<Expr>, Vec<Expr>, Type),
    If(Box<Expr>, Vec<Statement>, Vec<Statement>, Type),
    Case(Box<Expr>, Vec<(Pattern, Box<Expr>)>, Type),
    Lit(ast::Lit, Type),
    MethodCall(Box<Expr>, String, Vec<Expr>, Type),
    StructInit(Type, Vec<(String, Expr)>),
}

#[derive(Clone, Debug)]
pub enum LExpr {
    Var(String),
    MethodCall(Box<LExpr>, String, Vec<Expr>),
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