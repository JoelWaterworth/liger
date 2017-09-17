use parsing::ast::*;
use parsing::tokenizer::Token;

pub fn parse_source_file(tokens: &mut Vec<Token>) -> SourceFile {
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
        _ => panic!("")
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
    loop{
        let n = clone_nested(tokens.last());
        match n {
            Some(Token::Mul) => {
                tokens.pop();
                let ne = parse_term(tokens);
                output = Expr::BinaryExpr(BinaryOperator::Mul, Box::new(output), Box::new(ne))
            }
            Some(Token::Div) => {
                tokens.pop();
                let ne = parse_term(tokens);
                output = Expr::BinaryExpr(BinaryOperator::Div, Box::new(output), Box::new(ne))
            }
            Some(Token::Add) => {
                tokens.pop();
                let ne = parse_term(tokens);
                output = Expr::BinaryExpr(BinaryOperator::Add, Box::new(output), Box::new(ne))
            }
            _ => return output
        }
    }
}

fn parse_expr_1(tokens: &mut Vec<Token>) -> Expr {
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

fn parse_expr_2(tokens: &mut Vec<Token>) -> Expr {
    let mut output = parse_expr_1(tokens);
    loop{
        let n = clone_nested(tokens.last());
        match n {
            Some(Token::Or) => {
                tokens.pop();
                let ne = parse_expr_1(tokens);
                output = Expr::BinaryExpr(BinaryOperator::Or, Box::new(output), Box::new(ne))
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
            Some(Token::BitwiseAnd) => {
                tokens.pop();
                let ne = parse_expr_2(tokens);
                output = Expr::BinaryExpr(BinaryOperator::BitwiseAnd, Box::new(output), Box::new(ne))
            }
            Some(Token::BitwiseOr) => {
                tokens.pop();
                let ne = parse_expr_2(tokens);
                output = Expr::BinaryExpr(BinaryOperator::BitwiseOr, Box::new(output), Box::new(ne))
            }
            Some(Token::BitwiseXor) => {
                tokens.pop();
                let ne = parse_expr_2(tokens);
                output = Expr::BinaryExpr(BinaryOperator::BitwiseXor, Box::new(output), Box::new(ne))
            }
            Some(Token::IsEqualTo) => {
                tokens.pop();
                let ne = parse_expr_2(tokens);
                output = Expr::BinaryExpr(BinaryOperator::IsEqualTo, Box::new(output), Box::new(ne))
            }
            Some(Token::IsNotEqualTo) => {
                tokens.pop();
                let ne = parse_expr_2(tokens);
                output = Expr::BinaryExpr(BinaryOperator::IsNotEqualTo, Box::new(output), Box::new(ne))
            }
            Some(Token::GT) => {
                tokens.pop();
                let ne = parse_expr_2(tokens);
                output = Expr::BinaryExpr(BinaryOperator::GT, Box::new(output), Box::new(ne))
            }
            Some(Token::LT) => {
                tokens.pop();
                let ne = parse_expr_2(tokens);
                output = Expr::BinaryExpr(BinaryOperator::LT, Box::new(output), Box::new(ne))
            }
            Some(Token::GE) => {
                tokens.pop();
                let ne = parse_expr_2(tokens);
                output = Expr::BinaryExpr(BinaryOperator::GE, Box::new(output), Box::new(ne))
            }
            Some(Token::LE) => {
                tokens.pop();
                let ne = parse_expr_2(tokens);
                output = Expr::BinaryExpr(BinaryOperator::LE, Box::new(output), Box::new(ne))
            }
            _ => return output
        }
    }
}

fn parse_expr(tokens: &mut Vec<Token>) -> Expr {
    parse_expr_3(tokens)
}



pub fn expect_token(tokens: &mut Vec<Token>, token: Token) {
    let t = tokens.pop().unwrap();
    if t != token {
        panic!("unexpected token {:?} should be {:?}", t, token)
    }
}

pub fn parse_statement(tokens: &mut Vec<Token>) -> Statement {
    let t = tokens.pop().unwrap();
    match t {
        Token::Let => {
            let name = tokens.pop().unwrap();
            match name {
                Token::Identifier(x) => {
                    expect_token(tokens, Token::Equal);
                    let val = parse_expr(tokens);
                    expect_token(tokens, Token::SemiColon);
                    Statement::Let(x, Box::new(val))
                }
                _ => panic!("")
            }
        }
        _ => panic!("")
    }
}

pub fn parse_field_def(tokens: &mut Vec<Token>) -> FieldDef {
    panic!("")
}

pub fn parse_type(tokens: &mut Vec<Token>) -> Type {
    panic!("")
}

pub fn parse_declaration(tokens: &mut Vec<Token>) -> Declaration {
    panic!("")
}