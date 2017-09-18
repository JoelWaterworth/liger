use parsing::ast::*;
use parsing::tokenizer::Token;

pub fn parse_source_file(tokens: &mut Vec<Token>) -> SourceFile {
    let mut decls = Vec::new();
    let mut imports = Vec::new();
    loop {
        let n = clone_nested(tokens.last());
        match n {
            Some(Token::Identifier(_)) => decls.push(parse_function_decl(tokens)),
            Some(Token::Struct) => decls.push(parse_struct(tokens)),
            Some(Token::Import) => imports.push(parse_import(tokens)),
            Some(a) => panic!("unexpect {?} in parse_source_file"),
            None => break
        }
    }
    SourceFile {decls, imports}
}
pub fn parse_import(tokens: &mut Vec<Token>) -> String {
    panic!("")
}
pub fn parse_binary_operator(tokens: &mut Vec<Token>) -> BinaryOperator {
    panic!("")
}
pub fn parse_unary_operator(tokens: &mut Vec<Token>) -> UnaryOperator {
    panic!("")
}
pub fn parse_match(tokens: &mut Vec<Token>) -> Match {
    panic!("")
}

pub fn parse_term(tokens: &mut Vec<Token>) -> Expr {
    let t = tokens.pop().unwrap();
    match t {
        Token::Integer(x) => Expr::LiteralUint(x),
        Token::Identifier(x) => Expr::Var(x),
        _ => panic!("unexpected {:?}", t)
    }
}

fn clone_nested<T: Clone>(x: Option<&T>) -> Option<T> {
    match x {
        Some(t) => Some(t.clone()),
        None => None
    }
}

fn parse_expr_0(tokens: &mut Vec<Token>) -> Expr {
    let mut output = parse_term(tokens);
    loop {
        let n = clone_nested(tokens.last());
        let nn = clone_nested(tokens.last());
        match (n,nn) {
            (Some(Token::OpenParen), _) => {
                tokens.pop();
                let mut v = Vec::new();
                loop {
                    let t = clone_nested(tokens.last()).unwrap();
                    match t {
                        Token::CloseParen => {
                            tokens.pop();
                            break
                        },
                        _ => {}
                    }
                    v.push(parse_expr(tokens));
                    let nt = tokens.pop().unwrap();
                    match nt {
                        Token::CloseParen => break,
                        Token::Comma => {}
                        _ => {}
                    }
                }
                output = Expr::App(Box::new(output), v)
            },
            _ => return output
        }
    }
}

fn parse_expr_1(tokens: &mut Vec<Token>) -> Expr {
    let mut output = parse_expr_0(tokens);
    loop{
        let n = clone_nested(tokens.last());
        match n {
            Some(Token::Mul) => {
                tokens.pop();
                let ne = parse_expr_0(tokens);
                output = Expr::BinaryExpr(BinaryOperator::Mul, Box::new(output), Box::new(ne))
            }
            Some(Token::Div) => {
                tokens.pop();
                let ne = parse_expr_0(tokens);
                output = Expr::BinaryExpr(BinaryOperator::Div, Box::new(output), Box::new(ne))
            }
            Some(Token::Add) => {
                tokens.pop();
                let ne = parse_expr_0(tokens);
                output = Expr::BinaryExpr(BinaryOperator::Add, Box::new(output), Box::new(ne))
            }
            _ => return output
        }
    }
}

fn parse_expr_2(tokens: &mut Vec<Token>) -> Expr {
    let mut output = parse_expr_0(tokens);
    loop{
        let n = clone_nested(tokens.last());
        match n {
            Some(Token::Add) => {
                tokens.pop();
                let ne = parse_expr_0(tokens);
                output = Expr::BinaryExpr(BinaryOperator::Add, Box::new(output), Box::new(ne))
            }
            Some(Token::Sub) => {
                tokens.pop();
                let ne = parse_expr_0(tokens);
                output = Expr::BinaryExpr(BinaryOperator::Sub, Box::new(output), Box::new(ne))
            }
            _ => return output
        }
    }
}

fn parse_expr_3(tokens: &mut Vec<Token>) -> Expr {
    let mut output = parse_expr_2(tokens);
    loop{
        let n = clone_nested(tokens.last());
        match n {
            Some(Token::Or) => {
                tokens.pop();
                let ne = parse_expr_2(tokens);
                output = Expr::BinaryExpr(BinaryOperator::Or, Box::new(output), Box::new(ne))
            }
            _ => return output
        }
    }
}

fn parse_expr_4(tokens: &mut Vec<Token>) -> Expr {
    let mut output = parse_expr_3(tokens);
    loop{
        let n = clone_nested(tokens.last());
        match n {
            Some(Token::BitwiseAnd) => {
                tokens.pop();
                let ne = parse_expr_3(tokens);
                output = Expr::BinaryExpr(BinaryOperator::BitwiseAnd, Box::new(output), Box::new(ne))
            }
            Some(Token::BitwiseOr) => {
                tokens.pop();
                let ne = parse_expr_3(tokens);
                output = Expr::BinaryExpr(BinaryOperator::BitwiseOr, Box::new(output), Box::new(ne))
            }
            Some(Token::BitwiseXor) => {
                tokens.pop();
                let ne = parse_expr_3(tokens);
                output = Expr::BinaryExpr(BinaryOperator::BitwiseXor, Box::new(output), Box::new(ne))
            }
            Some(Token::IsEqualTo) => {
                tokens.pop();
                let ne = parse_expr_3(tokens);
                output = Expr::BinaryExpr(BinaryOperator::IsEqualTo, Box::new(output), Box::new(ne))
            }
            Some(Token::IsNotEqualTo) => {
                tokens.pop();
                let ne = parse_expr_3(tokens);
                output = Expr::BinaryExpr(BinaryOperator::IsNotEqualTo, Box::new(output), Box::new(ne))
            }
            Some(Token::GT) => {
                tokens.pop();
                let ne = parse_expr_3(tokens);
                output = Expr::BinaryExpr(BinaryOperator::GT, Box::new(output), Box::new(ne))
            }
            Some(Token::LT) => {
                tokens.pop();
                let ne = parse_expr_3(tokens);
                output = Expr::BinaryExpr(BinaryOperator::LT, Box::new(output), Box::new(ne))
            }
            Some(Token::GE) => {
                tokens.pop();
                let ne = parse_expr_3(tokens);
                output = Expr::BinaryExpr(BinaryOperator::GE, Box::new(output), Box::new(ne))
            }
            Some(Token::LE) => {
                tokens.pop();
                let ne = parse_expr_3(tokens);
                output = Expr::BinaryExpr(BinaryOperator::LE, Box::new(output), Box::new(ne))
            }
            _ => return output
        }
    }
}

fn parse_expr(tokens: &mut Vec<Token>) -> Expr {
    parse_expr_4(tokens)
}



pub fn expect_token(tokens: &mut Vec<Token>, token: Token) {
    let t = tokens.pop().unwrap();
    if t != token {
        panic!("unexpected token {:?} should be {:?}", t, token)
    }
}

pub fn parse_statements(tokens: &mut Vec<Token>) -> Vec<Statement> {
    let mut v = Vec::new();
    expect_token(tokens, Token::OpenCurly);
    loop {
        let t = match tokens.pop() {
            Some(x) => x,
            None => break
        };
        match t {
            Token::Let => {
                let name = tokens.pop().unwrap();
                match name {
                    Token::Identifier(x) => {
                        expect_token(tokens, Token::Equal);
                        let val = parse_expr(tokens);
                        expect_token(tokens, Token::SemiColon);
                        v.push(Statement::Let(x, Box::new(val)));
                    }
                    _ => panic!("")
                }
            },
            Token::If => {
                expect_token(tokens, Token::OpenParen);
                let expr = parse_expr(tokens);
                expect_token(tokens, Token::CloseParen);
                let fs = parse_statements(tokens);
                expect_token(tokens, Token::Else);
                let ss = parse_statements(tokens);
                v.push(Statement::If(Box::new(expr),fs,ss));
            }

            Token::CloseCurly => break,
            _ => panic!("")
        }
    }
    v
}

pub fn parse_field_def(tokens: &mut Vec<Token>) -> FieldDef {
    let t = tokens.pop().unwrap();
    match t {
        Token::Identifier(name) => {
            expect_token(tokens, Token::Colon);
            let nt = tokens.pop().unwrap();
            match nt {
                Token::Identifier(type_name) => FieldDef {name, ty: Type::Named(type_name)},
                _ => panic!("expected type")
            }
        }
        _ => panic!("expected name found {:?}", t)
    }
}

pub fn parse_function_decl(tokens: &mut Vec<Token>) -> Declaration {
    let t = tokens.pop().unwrap();
    match t {
        Token::Identifier(name) => {
            expect_token(tokens, Token::Colon);
            expect_token(tokens, Token::OpenParen);

            let mut types = Vec::new();

            loop {
                let nt = tokens.pop().unwrap();
                match nt {
                    Token::Identifier(ty) => {
                        types.push(Type::Named(ty));
                        let nnt = tokens.pop().unwrap();
                        match nnt {
                            Token::Comma => {},
                            Token::CloseParen => break,
                            _ => panic!("unexpected {:?}", nnt)
                        }
                    },
                    _ => panic!("")
                }
            }
            expect_token(tokens, Token::RightArrow);

            let nt = tokens.pop().unwrap();

            let ret = match nt {
                Token::Identifier(r) => r,
                _ => panic!("expected return type")
            };

            let mut cases = Vec::new();

            loop {
                let nc = clone_nested(tokens.last());
                match nc {
                    Some(Token::Identifier(x)) => {
                        if x != name {
                            break
                        }
                        tokens.pop();
                        cases.push(parse_func_case(tokens));
                    },
                    _ => break
                }
            }
            return Declaration::FunctionDef(name, types, Type::Named(ret), cases)
        }
        _ => panic!("")
    }
}

pub fn parse_struct(tokens: &mut Vec<Token>) -> Declaration {
    let t = tokens.pop().unwrap();
    match t {
        Token::Struct => {
            let name_token = tokens.pop().unwrap();
            let name = match name_token {
                Token::Identifier(x) => x,
                _ => panic!("parsing struct and expected structs name")
            };
            expect_token(tokens, Token::OpenCurly);
            let mut v = Vec::new();
            loop {
                v.push(parse_field_def(tokens));
                let nt = clone_nested(tokens.last()).unwrap();
                match nt {
                    Token::Identifier(_) => {},
                    Token::CloseCurly => {
                        tokens.pop();
                        return Declaration::StructDef(name, v)
                    },
                    Token::Comma => {
                        tokens.pop();
                        let nnt = clone_nested(tokens.last()).unwrap();
                        match nnt {
                            Token::Identifier(_) => {},
                            Token::CloseCurly => {
                                tokens.pop();
                                return Declaration::StructDef(name, v)
                            },
                            _ => panic!("")
                        }
                    },
                    _ => panic!("")
                }
            }
        }
        _ => panic!("")
    }
}

pub fn parse_func_case(tokens: &mut Vec<Token>) -> FuncCase {
    let mut v = Vec::new();
    loop {
        let t = tokens.pop().unwrap();
        match t.clone() {
            Token::Equal => break,
            Token::Identifier(s) => {
                v.push(Match::WildCard(s))
            },
            _ => panic!("unexpected function case {:?}", t)
        }
    }
    let s = parse_statements(tokens);
    FuncCase{matches: v, body: s}
}