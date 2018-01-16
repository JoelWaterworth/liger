use parsing::ast::*;
use parsing::tokenizer::{Token, TokenPos};

pub fn parse_source_file(tokens: &mut Vec<TokenPos>) -> SourceFile {
    let mut decls = Vec::new();
    let mut imports = Vec::new();
    loop {
        match clone_nested(tokens.last()) {
            Some((Token::Identifier(_), _)) => decls.push(Declaration::FunctionDef( parse_function(tokens))),
            Some((Token::Struct, _)) => decls.push(parse_struct(tokens)),
            Some((Token::Trait, _)) => decls.push(parse_trait(tokens)),
            Some((Token::Import, _)) => imports.push(parse_import(tokens)),
            Some((Token::Enum, _)) => decls.push(parse_enum(tokens)),
            Some((Token::Link, _)) => decls.push(parse_link(tokens)),
            Some((Token::Extern, _)) => {
                for func in parse_extern(tokens) {
                    decls.push(Declaration::FunctionDef(func))
                }
            },
            Some(n) => panic!("unexpect {:?} in parse_source_file", n),
            None => break
        }
    }
    SourceFile {decls, imports}
}

fn parse_link(tokens: &mut Vec<TokenPos>) -> Declaration {
    expect_token(tokens, Token::Link);
    expect_token(tokens, Token::Quote);
    expect_identifier(tokens);
    expect_token(tokens, Token::Colon);
    let name = expect_identifier(tokens);
    expect_token(tokens, Token::Quote);
    Declaration::Link(name)
}

fn parse_extern(tokens: &mut Vec<TokenPos>) -> Vec<FunctionDefinition> {
    expect_token(tokens, Token::Extern);
    expect_token(tokens, Token::OpenCurly);
    let mut v = Vec::new();
    loop {
        match clone_nested(tokens.last()) {
            Some((Token::Identifier(_), _)) => {
                v.push(parse_function_type(tokens));
                expect_token(tokens, Token::SemiColon);
            },
            Some((Token::CloseCurly, _)) => {
                tokens.pop();
                break
            },
            x => panic!("{:?}", x)
        }
    }
    v
}

fn parse_type(tokens: &mut Vec<TokenPos>) -> Type {
    match tokens.pop().unwrap() {
        (Token::Identifier(name), _) => Type::Named(name),
        x => panic!("{:?}", x)
    }
}

fn parse_enum(tokens: &mut Vec<TokenPos>) -> Declaration {
    expect_token(tokens, Token::Enum);
    let name = expect_identifier(tokens);
    expect_token(tokens, Token::OpenCurly);
    let mut fields = Vec::new();
    loop {
        match tokens.pop().unwrap() {
            (Token::Identifier(name), _) => {
                let mut body = Vec::new();
                match clone_nested(tokens.last()).unwrap() {
                    (Token::OpenParen, _) => {
                        tokens.pop();
                        loop {
                            match clone_nested(tokens.last()).unwrap() {
                                (Token::Identifier(_), _) => {
                                    body.push(parse_type(tokens));
                                    match clone_nested(tokens.last()).unwrap() {
                                        (Token::CloseParen, _) => {
                                            tokens.pop();
                                            break
                                            match tokens.pop().unwrap() {
                                                (Token::Comma, _) => {},
                                                (Token::CloseCurly, _) => break,
                                                x => panic!("{:?}", x)
                                            }
                                        },
                                        (Token::Comma, _) => {
                                            tokens.pop();
                                        },
                                        x => panic!("{:?}", x)
                                    }
                                },
                                (Token::CloseParen, _) => {
                                    tokens.pop();
                                    break
                                    match tokens.pop().unwrap() {
                                        (Token::Comma, _) => {},
                                        (Token::CloseCurly, _) => break,
                                        x => panic!("{:?}", x)
                                    }
                                },
                                x => panic!("{:?}", x)
                            }
                        }
                    },
                    (Token::Comma, _) => {
                        tokens.pop();
                    },
                    (Token::CloseCurly, _) => {
                        tokens.pop();
                        break
                    },
                    x => panic!("{:?}", x)
                };
                fields.push(EnumField{name, body});
            },
            (Token::CloseCurly, _) => break,
            x => panic!("{:?}", x)
        };
    };
    Declaration::Enum(name, fields)
}

#[allow(unused_variables)]
fn parse_import(tokens: &mut Vec<TokenPos>) -> String {
    panic!("")
}

fn parse_struct_args(tokens: &mut Vec<TokenPos>) -> Vec<(String, Expr)> {
    expect_token(tokens, Token::OpenCurly);
    let mut v = Vec::new();
    loop {
        let field = tokens.pop();
        match field {
            Some((Token::Identifier(f), _)) => {
                expect_token(tokens, Token::Colon);
                let expr = parse_expr(tokens);
                v.push((f, expr));
                match tokens.pop() {
                    Some((Token::Comma, _)) => {},
                    Some((Token::CloseCurly, _)) => return v,
                    _ => panic!("")
                }
            }
            x => panic!("unexpected {:?}", x)
        }
    }
}

fn if_expected_than<F: Fn(&mut Vec<TokenPos>)>(tokens: &mut Vec<TokenPos>, expected: Token, f: F) {
    match clone_nested(tokens.last()) {
        Some((x, _)) => if x == expected {
            f(tokens);
        },
        None => {}
    }
}

pub fn parse_term(tokens: &mut Vec<TokenPos>) -> Expr {
    match tokens.pop().unwrap() {
        (Token::Integer(x), _) => Expr::Lit(Lit::Integral(x)),
        (Token::Identifier(x), _) => {
            match clone_nested(tokens.last()) {
                Some((Token::OpenCurly, _)) => {
                    let args = parse_struct_args(tokens);
                    Expr::StructInit(Type::Named(x), args)
                }
                Some((Token::Colon, _)) => {
                    tokens.pop();
                    expect_token(tokens, Token::Colon);
                    let n = expect_identifier(tokens);
                    let mut arg = Vec::new();

                    match clone_nested(tokens.last()) {
                        Some((Token::OpenParen, _)) => {
                            if_expected_than(tokens, Token::CloseParen, |t| {
                                t.pop();
                            });
                            tokens.pop();
                            loop {
                                arg.push(parse_expr(tokens));
                                match clone_nested(tokens.last()) {
                                    Some((Token::CloseParen, _)) => {
                                        tokens.pop();
                                        break
                                    },
                                    Some((Token::Comma, _)) => {},
                                    x => panic!("{:?}", x)
                                }
                            }
                        },
                        _ => {}
                    };
                    return Expr::EnumInit(Type::Named(x), n, arg);
                }
                _ => Expr::Var(x)
            }
        },
        (Token::If, _) => {
            let (expr, fs, ss) = parse_if_statement(tokens);
            Expr::If(Box::new(expr), fs, ss)
        },
        (Token::OpenSquare, _) => {
            let mut v = Vec::new();
            loop {
                v.push(parse_expr(tokens));
                match tokens.pop().unwrap() {
                    (Token::CloseSquare, _) => return Expr::SliceInit(v),
                    (Token::Comma, _) => {},
                    t => panic!("unexpected {:?}", t),
                }
            }
        },
        t => panic!("unexpected {:?}", t)
    }
}

fn clone_nested<T: Clone>(x: Option<&T>) -> Option<T> {
    match x {
        Some(t) => Some(t.clone()),
        None => None
    }
}

fn function_args(tokens: &mut Vec<TokenPos>) -> Option<Vec<Expr>> {
    let mut v = Vec::new();
    match tokens.pop() {
        Some((t, _)) => if t != Token::OpenParen {
            return None;
        },
        _ => {}
    };

    loop {
        let t = clone_nested(tokens.last()).unwrap();
        match t {
            (Token::CloseParen, _) => {
                tokens.pop();
                break
            },
            _ => {}
        }
        v.push(parse_expr(tokens));
        let nt = tokens.pop().unwrap();
        match nt {
            (Token::CloseParen, _) => break,
            (Token::Comma, _) => {}
            _ => {}
        }
    }
    Some(v)
}

fn parse_expr_0(tokens: &mut Vec<TokenPos>) -> Expr {
    let mut output = parse_term(tokens);
    loop {
        match clone_nested(tokens.last()) {
            Some((Token::OpenParen, _)) => {
                let v = function_args(tokens).unwrap();
                output = Expr::App(Box::new(output), v)
            },
            Some((Token::Dot, _)) => {
                tokens.pop();
                let x = tokens.pop();
                let name = match x {
                    Some((Token::Identifier(y), _)) => y,
                    _ => panic!("expected function name found {:?}", x)
                };
                match clone_nested(tokens.last()) {
                    Some((Token::OpenParen, _)) => {
                        let v = function_args(tokens).unwrap();
                        output = Expr::MethodCall(Box::new(output), name, v)
                    },
                    _ => output = Expr::MethodCall(Box::new(output), name, Vec::new())
                }
            },
            Some((Token::OpenSquare, _)) => {
                tokens.pop();
                let index = parse_expr(tokens);
                expect_token(tokens, Token::CloseSquare);
                output = Expr::Index(Box::new(output), Box::new(index))
            }
            _ => return output
        }
    }
}

fn parse_expr_1(tokens: &mut Vec<TokenPos>) -> Expr {
    let mut output = parse_expr_0(tokens);
    loop{
        match clone_nested(tokens.last()) {
            Some((Token::Mul, _)) => {
                tokens.pop();
                let ne = parse_expr_0(tokens);
                output = Expr::BinaryExpr(BinaryOperator::Mul, Box::new(output), Box::new(ne))
            }
            Some((Token::Div, _)) => {
                tokens.pop();
                let ne = parse_expr_0(tokens);
                output = Expr::BinaryExpr(BinaryOperator::Div, Box::new(output), Box::new(ne))
            }
            _ => return output
        }
    }
}

fn parse_expr_2(tokens: &mut Vec<TokenPos>) -> Expr {
    let mut output = parse_expr_1(tokens);
    loop{
        match clone_nested(tokens.last()) {
            Some((Token::Add, _)) => {
                tokens.pop();
                let ne = parse_expr_1(tokens);
                output = Expr::BinaryExpr(BinaryOperator::Add, Box::new(output), Box::new(ne))
            }
            Some((Token::Sub, _)) => {
                tokens.pop();
                let ne = parse_expr_1(tokens);
                output = Expr::BinaryExpr(BinaryOperator::Sub, Box::new(output), Box::new(ne))
            }
            _ => return output
        }
    }
}

fn parse_expr_3(tokens: &mut Vec<TokenPos>) -> Expr {
    let mut output = parse_expr_2(tokens);
    loop{
        match clone_nested(tokens.last()) {
            Some((Token::Or, _)) => {
                tokens.pop();
                let ne = parse_expr_2(tokens);
                output = Expr::BinaryExpr(BinaryOperator::Or, Box::new(output), Box::new(ne))
            }
            _ => return output
        }
    }
}

fn parse_expr_4(tokens: &mut Vec<TokenPos>) -> Expr {
    let mut output = parse_expr_3(tokens);
    loop{
        match clone_nested(tokens.last()) {
            Some((Token::BitwiseAnd, _)) => {
                tokens.pop();
                let ne = parse_expr_3(tokens);
                output = Expr::BinaryExpr(BinaryOperator::BitwiseAnd, Box::new(output), Box::new(ne))
            }
            Some((Token::BitwiseOr, _)) => {
                tokens.pop();
                let ne = parse_expr_3(tokens);
                output = Expr::BinaryExpr(BinaryOperator::BitwiseOr, Box::new(output), Box::new(ne))
            }
            Some((Token::BitwiseXor, _)) => {
                tokens.pop();
                let ne = parse_expr_3(tokens);
                output = Expr::BinaryExpr(BinaryOperator::BitwiseXor, Box::new(output), Box::new(ne))
            }
            Some((Token::IsEqualTo, _)) => {
                tokens.pop();
                let ne = parse_expr_3(tokens);
                output = Expr::BinaryExpr(BinaryOperator::IsEqualTo, Box::new(output), Box::new(ne))
            }
            Some((Token::IsNotEqualTo, _)) => {
                tokens.pop();
                let ne = parse_expr_3(tokens);
                output = Expr::BinaryExpr(BinaryOperator::IsNotEqualTo, Box::new(output), Box::new(ne))
            }
            Some((Token::GT, _)) => {
                tokens.pop();
                let ne = parse_expr_3(tokens);
                output = Expr::BinaryExpr(BinaryOperator::GT, Box::new(output), Box::new(ne))
            }
            Some((Token::LT, _)) => {
                tokens.pop();
                let ne = parse_expr_3(tokens);
                output = Expr::BinaryExpr(BinaryOperator::LT, Box::new(output), Box::new(ne))
            }
            Some((Token::GE, _)) => {
                tokens.pop();
                let ne = parse_expr_3(tokens);
                output = Expr::BinaryExpr(BinaryOperator::GE, Box::new(output), Box::new(ne))
            }
            Some((Token::LE, _)) => {
                tokens.pop();
                let ne = parse_expr_3(tokens);
                output = Expr::BinaryExpr(BinaryOperator::LE, Box::new(output), Box::new(ne))
            }
            _ => return output
        }
    }
}

fn parse_expr(tokens: &mut Vec<TokenPos>) -> Expr {
    parse_expr_4(tokens)
}

fn parse_bracketed_l_expr(tokens: &mut Vec<TokenPos>) -> LExpr {
    expect_token(tokens, Token::OpenParen);
    let l_expr = parse_l_expr(tokens);
    expect_token(tokens, Token::CloseParen);
    l_expr
}

fn parse_l_expr_0(tokens: &mut Vec<TokenPos>) -> LExpr {
    match clone_nested(tokens.last()) {
        Some((Token::OpenParen, _)) => parse_bracketed_l_expr(tokens),
        Some((Token::Identifier(name), _)) => {
            tokens.pop();
            LExpr::Var(name)
        },
        x => panic!("failed to parse l_expression at {:?}", x)
    }
}

fn parse_l_expr_1(tokens: &mut Vec<TokenPos>) -> LExpr {
    let mut l_expr = parse_l_expr_0(tokens);
    loop {
        match clone_nested(tokens.last()) {
            Some((Token::Dot, _)) => {
                tokens.pop();
                let field = expect_identifier(tokens);
                l_expr = LExpr::MethodCall(Box::new(l_expr), field, Vec::new());
            },
            _ => return l_expr
        }
    }
}

fn parse_l_expr(tokens: &mut Vec<TokenPos>) -> LExpr {
    parse_l_expr_1(tokens)
}

fn expect_token(tokens: &mut Vec<TokenPos>, token: Token) {
    let t = tokens.pop().unwrap();
    if t.0 != token {
        panic!("unexpected token {:?} should be {:?}", t, token)
    }
}

#[allow(dead_code)]
fn expect_tokens(tokens: &mut Vec<TokenPos>, expected_tokens: &[Token]) {
    for token in expected_tokens.iter() {
        expect_token(tokens, token.clone());
    }
}

pub fn expect_identifier(tokens: &mut Vec<TokenPos>) -> String {
    let t = tokens.pop().unwrap();
    match t.0 {
        Token::Identifier(x) => x,
        _ => panic!("not a identifier it is in fact a {:?}", t)
    }
}

pub fn parse_statements(tokens: &mut Vec<TokenPos>) -> Vec<Statement> {
    let mut v = Vec::new();
    expect_token(tokens, Token::OpenCurly);
    loop {
        match clone_nested(tokens.last()) {
            Some((Token::Let, _)) => {
                tokens.pop();
                match tokens.pop().unwrap() {
                    (Token::Identifier(x), _) => {
                        expect_token(tokens, Token::Equal);
                        let val = parse_expr(tokens);
                        expect_token(tokens, Token::SemiColon);
                        v.push(Statement::Let(x, Box::new(val)));
                    },
                    (Token::Mut, _) => {
                        let s = expect_identifier(tokens);
                        expect_token(tokens, Token::Equal);
                        let val = parse_expr(tokens);
                        expect_token(tokens, Token::SemiColon);
                        v.push(Statement::LetMut(s, Box::new(val)));
                    }
                    x => panic!("{:?}", x)
                }
            },
            Some((Token::If, _)) => {
                tokens.pop();
                let (expr, fs, ss) = parse_if_statement(tokens);
                v.push(Statement::If(Box::new(expr),fs,ss));
            }
            Some((Token::CloseCurly, _)) => {
                tokens.pop();
                break
            },
            Some((Token::Return, _)) => {
                tokens.pop();
                let ret = parse_expr(tokens);
                expect_token(tokens, Token::SemiColon);
                v.push(Statement::Return(Box::new(ret)));
            },
            None => break,
            t => {
                let mut assign_tokens = tokens.clone();
                let mut func_tokens = tokens.clone();
                let mut method_tokens = tokens.clone();

                if let Some(assignment) = parse_assignment(&mut assign_tokens) {
                    v.push(assignment);
                    *tokens = assign_tokens;
                } else if let Some(func_call) = parse_function_call(&mut func_tokens) {
                    v.push(func_call);
                    *tokens = func_tokens;
                } else if let Some(method_call) = parse_method_call(&mut method_tokens) {
                    v.push(method_call);
                    *tokens = method_tokens;
                } else {
                    panic!("{:?}", t)
                }
            }
        }
    }
    v
}

fn parse_assignment(tokens: &mut Vec<TokenPos>) -> Option<Statement> {
    let l_expr = parse_l_expr(tokens);
    match tokens.pop() {
        Some((Token::Equal, _)) => {
            let expr = parse_expr(tokens);
            expect_token(tokens, Token::SemiColon);
            return Some(Statement::Assignment(Box::new(l_expr), Box::new(expr)))
        },
        _ => return None
    }
}

fn parse_function_call(tokens: &mut Vec<TokenPos>) -> Option<Statement> {
    match tokens.pop() {
        Some((Token::Identifier(func), _)) => {
            if let Some(args) = function_args(tokens) {
                expect_token(tokens, Token::SemiColon);
                Some(Statement::FunctionCall(Box::new(Expr::Var(func)), args))
            } else {
                return None
            }
        }
        _ => return None
    }
}

fn parse_method_call(tokens: &mut Vec<TokenPos>) -> Option<Statement> {
    let mut method = None;
    match tokens.pop() {
        Some((Token::Identifier(var), _)) => {
            let mut expr = Expr::Var(var);
            loop {
                match tokens.pop() {
                    Some((Token::SemiColon, _)) => return method,
                    Some((Token::Dot, _)) => {
                        let field = expect_identifier(tokens);
                        match clone_nested(tokens.last()) {
                            Some((Token::OpenParen, _)) => {
                                let args = function_args(tokens).unwrap();
                                method = Some(Statement::MethodCall(Box::new(expr.clone()), field.clone(), args.clone()));
                                expr = Expr::MethodCall(Box::new(expr.clone()), field, args.clone());
                            },
                            _ => {
                                method = Some(Statement::MethodCall(Box::new(expr.clone()), field.clone(), Vec::new()));
                                expr = Expr::MethodCall(Box::new(expr.clone()), field, Vec::new());
                            }
                        }
                    },
                    _ => return None
                }
            };
        },
        _ => return None
    }
}

fn parse_if_statement(tokens: &mut Vec<TokenPos>) -> (Expr, Vec<Statement>, Vec<Statement>) {
    let expr = parse_expr(tokens);
    let fs = parse_statements(tokens);
    let t = clone_nested(tokens.last());
    match t {
        Some((Token::Else, _)) => {
            tokens.pop().unwrap();
            let ss = parse_statements(tokens);
            return (expr, fs, ss)
        }
        _ => return (expr, fs, Vec::new())
    }


}

pub fn parse_field_def(tokens: &mut Vec<TokenPos>) -> FieldDef {
    let t = tokens.pop().unwrap();
    match t {
        (Token::Identifier(name), _) => {
            match tokens.pop().unwrap() {
                (Token::Identifier(type_name), _) => FieldDef {name, ty: Type::Named(type_name)},
                (Token::Mut, _) => {
                    let type_name = expect_identifier(tokens);
                    FieldDef {name, ty: Type::Cell(Box::new(Type::Named(type_name)))}
                }
                _ => panic!("expected type")
            }
        }
        _ => panic!("expected name found {:?}", t)
    }
}

pub fn parse_function_type(tokens: &mut Vec<TokenPos>) -> FunctionDefinition {
    let name = expect_identifier(tokens);
    expect_token(tokens, Token::Colon);
    let mut generics: Vec<(Type, Vec<Type>)> = Vec::new();
    match tokens.last().unwrap().clone() {
        (Token::GT, _) => {
            'inner: loop {
                match tokens.pop().unwrap() {
                    (Token::Identifier(ty), _) => {
                        match clone_nested(tokens.last()) {
                            Some((Token::Colon, _)) => {
                                tokens.pop();
                                let mut traits = Vec::new();
                                loop {
                                    match tokens.pop().unwrap() {
                                        (Token::Identifier(ty), _) => {
                                            traits.push(Type::Named(ty));
                                            match clone_nested(tokens.last()) {
                                                Some((Token::Add, _)) => {},
                                                _ => {
                                                    break
                                                }
                                            };
                                        },
                                        x => panic!("{:?}", x)
                                    };
                                };
                                generics.push((Type::Named(ty), traits))
                            },
                            _ => {
                                generics.push((Type::Named(ty), Vec::new()))
                            }
                        };
                        match clone_nested(tokens.last()) {
                            Some((Token::Comma, _)) => {
                                tokens.pop();
                            },
                            Some((Token::LT, _)) => {
                                tokens.pop();
                                break
                            },
                            x => panic!("{:?}", x)
                        }
                    },
                    (Token::LT, _) => break,
                    x => panic!("{:?}", x)
                }
            }
        },
        _ => {}
    };
    expect_token(tokens, Token::OpenParen);

    let mut types = Vec::new();

    loop {
        match tokens.pop().unwrap() {
            (Token::Identifier(ty), _) => {
                types.push(Type::Named(ty));
                match tokens.pop().unwrap() {
                    (Token::Comma, _) => {},
                    (Token::CloseParen, _) => break,
                    x => panic!("unexpected {:?}", x)
                }
            },
            (Token::SelfTok, _) => {
                types.push(Type::SelfT);
                match tokens.pop().unwrap() {
                    (Token::Comma, _) => {},
                    (Token::CloseParen, _) => break,
                    x => panic!("unexpected {:?}", x)
                }
            },
            (Token::CloseParen, _) => break,
            x => panic!("unexpected {:?}", x)
        }
    }
    expect_token(tokens, Token::RightArrow);

    let ret_type = match tokens.pop().unwrap() {
        (Token::Identifier(r), _) => Type::Named(r),
        (Token::SelfTok, _) => Type::SelfT,
        _ => panic!("expected return type")
    };

    FunctionDefinition{
        name,
        generics,
        arg_types: types,
        ret_type,
        cases: Vec::new()
    }
}

pub fn parse_function(tokens: &mut Vec<TokenPos>) -> FunctionDefinition {
    let mut function_def = parse_function_type(tokens);
    let mut cases = Vec::new();

    loop {
        let nc = clone_nested(tokens.last());
        match nc {
            Some((Token::Identifier(x), _)) => {
                if x != function_def.name {
                    break
                }
                tokens.pop();
                cases.push(parse_func_case(tokens));
            },
            _ => break
        }
    }
    function_def.cases = cases;
    return function_def
}

pub fn parse_trait(tokens: &mut Vec<TokenPos>) -> Declaration {
    match tokens.pop().unwrap() {
        (Token::Trait, _) => {
            let name = expect_identifier(tokens);
            expect_token(tokens, Token::OpenCurly);
            let mut funcs = Vec::new();
            loop {
                match tokens.last().unwrap().clone() {
                    (Token::Identifier(_), _) => {
                        funcs.push(parse_function_type(tokens));
                        expect_token(tokens, Token::SemiColon);
                    },
                    (Token::CloseCurly, _) => {
                        tokens.pop();
                        return Declaration::Trait(name, funcs)
                    },
                    x => panic!("{:?}", x)
                }
            }
        },
        x => panic!("{:?}", x)
    }
}

pub fn parse_struct(tokens: &mut Vec<TokenPos>) -> Declaration {
    match tokens.pop().unwrap() {
        (Token::Struct, _) => {
            let name = expect_identifier(tokens);
            expect_token(tokens, Token::OpenCurly);
            let mut fields = Vec::new();
            let mut funcs = Vec::new();
            loop {
                let mut f = tokens.clone();
                match f.pop().unwrap() {
                    (Token::Identifier(_), _) => {
                        match f.pop().unwrap() {
                            (Token::Identifier(_), _) => {
                                fields.push(parse_field_def(tokens));
                            },
                            (Token::Mut, _) => {
                                fields.push(parse_field_def(tokens));
                            },
                            (Token::Colon, _) => {
                                funcs.push(parse_function(tokens));
                            }
                            x => panic!("{:?} is not expected", x)
                        }
                    },
                    (Token::CloseCurly, _) => {
                        tokens.pop();
                        return Declaration::StructDef(name, fields, funcs)
                    },

                    x => panic!("{:?}", x)
                }
            }
        }
        x => panic!("{:?}", x)
    }
}

pub fn parse_func_case(tokens: &mut Vec<TokenPos>) -> FuncCase {
    let mut v = Vec::new();
    loop {
        match tokens.pop().unwrap() {
            (Token::Equal, _) => break,
            (Token::Identifier(s), _) => {
                v.push(Match::WildCard(s))
            },
            (Token::Integer(x), _) => {
                v.push(Match::Lit(Lit::Integral(x)))
            }
            t => panic!("unexpected function case {:?}", t)
        }
    }
    let s = parse_statements(tokens);
    FuncCase{matches: v, body: s}
}