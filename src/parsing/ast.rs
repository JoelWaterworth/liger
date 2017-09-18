#[derive(Clone, Debug)]
pub enum BinaryOperator{
    Pow,

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

#[derive(Clone, Debug)]
pub enum Match {
    WildCard(String),
    Constructor(String,Vec<Match>)
}

#[derive(Clone, Debug)]
pub enum Expr {
    Var(String),
    BinaryExpr(BinaryOperator, Box<Expr>, Box<Expr>),
    UnaryExpr(UnaryOperator, Box<Expr>),
    Lambda(Vec<String>, Box<Expr>),
    App(Box<Expr>, Vec<Expr>),
    If(Box<Expr>, Vec<Statement>, Vec<Statement>),
    Case(Box<Expr>, Vec<(Match, Box<Expr>)>),
    LiteralUint(u64),
    MethodCall(Box<Expr>, String, Vec<Expr>),
}

//foo.method(x)

#[derive(Clone, Debug)]
pub enum Statement {
    Assignment(String, Box<Expr>),
    If(Box<Expr>, Vec<Statement>, Vec<Statement>),
    Let(String, Box<Expr>),
    FunctionCall(Box<Expr>, Vec<Expr>),
    MethodCall(Box<Expr>, String, Vec<Expr>),
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
}

#[derive(Clone, Debug)]
pub enum Declaration {
    FunctionDef(String, Vec<Type>, Type, Vec<FuncCase>),
    StructDef(String, Vec<FieldDef>),
}

#[derive(Clone, Debug)]
pub struct SourceFile {
    pub imports: Vec<String>,
    pub decls: Vec<Declaration>,
}