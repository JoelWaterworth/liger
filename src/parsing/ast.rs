#[derive(Clone, Debug)]
pub enum BinaryOperator{
    Mul,
    Div,
    And,

    Add,
    Sub,

    Or,

    BitwiseAnd,
    BitwiseXor,
    BitwiseOr,
    IsEqualTo,
    IsNotEqualTo,
    GT,
    LT,
    GE,
    LE,
}

#[derive(Clone, Debug)]
pub enum UnaryOperator {
    Neg,
    Not,
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub enum Match {
    WildCard(String),
    Constructor(String,Vec<Match>),
    Lit(Lit),
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub enum Expr {
    Var(String),
    BinaryExpr(BinaryOperator, Box<Expr>, Box<Expr>),
    UnaryExpr(UnaryOperator, Box<Expr>),
    Lambda(Vec<String>, Box<Expr>),
    App(Box<Expr>, Vec<Expr>),
    If(Box<Expr>, Vec<Statement>, Vec<Statement>),
    Case(Box<Expr>, Vec<(Match, Box<Expr>)>),
    Lit(Lit),
    MethodCall(Box<Expr>, String, Vec<Expr>),
    StructInit(Type, Vec<(String, Expr)>),
    EnumInit(Type, String, Vec<Expr>),
    SliceInit(Vec<Expr>),
    Index(Box<Expr>, Box<Expr>)
}

#[derive(Clone, Debug)]
pub enum LExpr {
    Var(String),
    MethodCall(Box<LExpr>, String, Vec<Expr>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Lit {
    Integral(u64)
}

#[derive(Clone, Debug)]
pub enum Statement {
    Assignment(Box<LExpr>, Box<Expr>),
    If(Box<Expr>, Vec<Statement>, Vec<Statement>),
    Let(String, Box<Expr>),
    LetMut(String, Box<Expr>),
    FunctionCall(Box<Expr>, Vec<Expr>),
    MethodCall(Box<Expr>, String, Vec<Expr>),
    Return(Box<Expr>),
}

#[derive(Clone, Debug)]
pub struct FuncCase {
    pub matches: Vec<Match>,
    pub body: Vec<Statement>,
}

#[derive(Clone, Debug)]
pub struct FieldDef {
    pub name: String,
    pub ty: Type,
}

#[derive(Clone, Debug)]
pub enum Type {
    Named(String),
    SelfT,
    Cell(Box<Type>),
}

impl Type {
    pub fn get_name(&self) -> Option<&String> {
        match self {
            &Type::Named(ref n) => Some(n),
            _ => None
        }
    }
}

#[derive(Clone, Debug)]
pub struct FunctionDefinition {
    pub name: String,
    pub generics: Vec<(Type, Vec<Type>)>,
    pub arg_types: Vec<Type>,
    pub ret_type: Type,
    pub cases: Vec<FuncCase>
}

#[derive(Clone, Debug)]
pub enum Declaration {
    FunctionDef(FunctionDefinition),
    StructDef(String, Vec<FieldDef>, Vec<FunctionDefinition>),
    Trait(String, Vec<FunctionDefinition>),
    Enum(String, Vec<EnumField>),
    Link(String),
    Extern(FunctionDefinition)
}

#[derive(Clone, Debug)]
pub struct EnumField {
    pub name: String,
    pub body: Vec<Type>
}

#[derive(Clone, Debug)]
pub struct SourceFile {
    pub imports: Vec<String>,
    pub decls: Vec<Declaration>,
}