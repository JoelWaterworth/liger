use parsing::ast::*;
use parsing::tokenizer::Token;

pub fn parse_source_file(tokens: &mut Vec<Token>) -> SourceFile {
    let mut decls = Vec::new();
    let mut imports = Vec::new();
    loop {
        let n = clone_nested(tokens.last());
        match n.clone() {
            Some(Token::Identifier(_)) => decls.push(Declaration::FunctionDef( parse_function_decl(tokens))),
            Some(Token::Struct) => decls.push(parse_struct(tokens)),
            Some(Token::Import) => imports.push(parse_import(tokens)),
            Some(_) => panic!("unexpect {:?} in parse_source_file", n),
            None => break
        }
    }
    SourceFile {decls, imports}
}

pub fn parse_import(tokens: &mut Vec<Token>) -> String {
    panic!("")
}

fn parse_struct_args(tokens: &mut Vec<Token>) -> Vec<(String, Expr)> {
    expect_token(tokens, Token::OpenCurly);
    let mut v = Vec::new();
    loop {
        let field = tokens.pop();
        match field {
            Some(Token::Identifier(f)) => {
                expect_token(tokens, Token::Colon);
                let expr = parse_expr(tokens);
                v.push((f, expr));
                match tokens.pop() {
                    Some(Token::Comma) => {},
                    Some(Token::CloseCurly) => return v,
                    _ => panic!("")
                }
            }
            _ => panic!("")
        }
    }
}

pub fn parse_term(tokens: &mut Vec<Token>) -> Expr {
    let t = tokens.pop().unwrap();
    match t {
        Token::Integer(x) => Expr::Lit(Lit::Integral(x)),
        Token::Identifier(x) => {
            let t = clone_nested(tokens.last());
            match t {
                Some(Token::OpenCurly) => {
                    let args = parse_struct_args(tokens);
                    Expr::StructInit(Type::Named(x), args)
                }
                _ => Expr::Var(x)
            }
        },
        Token::If => {
            let (expr, fs, ss) = parse_if_statement(tokens);
            Expr::If(Box::new(expr), fs, ss)
        }
        _ => panic!("unexpected {:?}", t)
    }
}

fn clone_nested<T: Clone>(x: Option<&T>) -> Option<T> {
    match x {
        Some(t) => Some(t.clone()),
        None => None
    }
}

fn function_args(tokens: &mut Vec<Token>) -> Vec<Expr> {
    let mut v = Vec::new();
    expect_token(tokens, Token::OpenParen);
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
    v
}

fn parse_expr_0(tokens: &mut Vec<Token>) -> Expr {
    let mut output = parse_term(tokens);
    loop {
        let n = clone_nested(tokens.last());
        match n {
            Some(Token::OpenParen) => {
                let v = function_args(tokens);
                output = Expr::App(Box::new(output), v)
            },
            Some(Token::Dot) => {
                tokens.pop();
                let x = tokens.pop();
                let name = match x {
                    Some(Token::Identifier(y)) => y,
                    _ => panic!("expected function name found {:?}", x)
                };
                let n = clone_nested(tokens.last());
                match n {
                    Some(Token::OpenParen) => {
                        let v = function_args(tokens);
                        output = Expr::MethodCall(Box::new(output), name, v)
                    }
                    _ => output = Expr::MethodCall(Box::new(output), name, Vec::new())
                }
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
            _ => return output
        }
    }
}

fn parse_expr_2(tokens: &mut Vec<Token>) -> Expr {
    let mut output = parse_expr_1(tokens);
    loop{
        let n = clone_nested(tokens.last());
        match n {
            Some(Token::Add) => {
                tokens.pop();
                let ne = parse_expr_1(tokens);
                output = Expr::BinaryExpr(BinaryOperator::Add, Box::new(output), Box::new(ne))
            }
            Some(Token::Sub) => {
                tokens.pop();
                let ne = parse_expr_1(tokens);
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

fn parse_bracketed_l_expr(tokens: &mut Vec<Token>) -> LExpr {
    println!("bracket");
    expect_token(tokens, Token::OpenParen);
    let l_expr = parse_l_expr(tokens);
    expect_token(tokens, Token::CloseParen);
    l_expr
}

fn parse_l_expr_0(tokens: &mut Vec<Token>) -> LExpr {
    match clone_nested(tokens.last()) {
        Some(Token::OpenParen) => parse_bracketed_l_expr(tokens),
        Some(Token::Identifier(name)) => {
            tokens.pop();
            LExpr::Var(name)
        },
        x => panic!("failed to parse l_expression at {:?}", x)
    }
}

fn parse_l_expr_1(tokens: &mut Vec<Token>) -> LExpr {
    let mut l_expr = parse_l_expr_0(tokens);
    loop {
        match clone_nested(tokens.last()) {
            Some(Token::Dot) => {
                tokens.pop();
                let field = expect_identifier(tokens);
                l_expr = LExpr::MethodCall(Box::new(l_expr), field, Vec::new());
            },
            _ => return l_expr
        }
    }
}

fn parse_l_expr(tokens: &mut Vec<Token>) -> LExpr {
    parse_l_expr_1(tokens)
}

fn expect_token(tokens: &mut Vec<Token>, token: Token) {
    let t = tokens.pop().unwrap();
    if t != token {
        panic!("unexpected token {:?} should be {:?}", t, token)
    }
}

fn expect_tokens(tokens: &mut Vec<Token>, expected_tokens: &[Token]) {
    for token in expected_tokens.iter() {
        expect_token(tokens, token.clone());
    }
}

pub fn method_calls(tokens: &mut Vec<Token>, tar: Expr) -> Statement {
    let mut  target = tar;
    loop {
        tokens.pop();
        match tokens.pop().clone() {
            Some(Token::Identifier(name)) => {
                match clone_nested(tokens.last()) {
                    Some(Token::OpenParen) => {
                        let args = function_args(tokens);
                        let nm = clone_nested(tokens.last());
                        match nm {
                            Some(Token::Dot) => target = Expr::MethodCall(Box::new(target), name, args),
                            _ => return Statement::MethodCall(Box::new(target), name, args)
                        }
                    }
                    Some(Token::Equal) => {
                        tokens.pop();
                        return Statement::Assignment(Box::new(Expr::MethodCall(Box::new(target), name, Vec::new())), Box::new(parse_expr(tokens)))
                    }
                    t => panic!("expected name found {:?}", t)
//                        {
//                        let nm = clone_nested(tokens.last());
//                        match nm {
//                            Some(Token::Dot) => target = Expr::MethodCall(Box::new(target), name, Vec::new()),
//                            _ => return Statement::MethodCall(Box::new(target), name, Vec::new())
//                        }
//                    }
                }
            }
            t => panic!("expected name found {:?}", t)
        }
    }
}

pub fn expect_identifier(tokens: &mut Vec<Token>) -> String {
    let t = tokens.pop().unwrap();
    match t {
        Token::Identifier(x) => x,
        _ => panic!("not a identifier it is in fact a {:?}", t)
    }
}

pub fn parse_statements(tokens: &mut Vec<Token>) -> Vec<Statement> {
    let mut v = Vec::new();
    expect_token(tokens, Token::OpenCurly);
    loop  {
        match clone_nested(tokens.last()) {
            Some(Token::Let) => {
                tokens.pop();
                let name = tokens.pop().unwrap();
                match name {
                    Token::Identifier(x) => {
                        expect_token(tokens, Token::Equal);
                        let val = parse_expr(tokens);
                        expect_token(tokens, Token::SemiColon);
                        v.push(Statement::Let(x, Box::new(val)));
                    },
                    Token::Mut => {
                        let s = expect_identifier(tokens);
                        expect_token(tokens, Token::Equal);
                        let val = parse_expr(tokens);
                        expect_token(tokens, Token::SemiColon);
                        v.push(Statement::LetMut(s, Box::new(val)));
                    }
                    _ => panic!("")
                }
            },
            Some(Token::If) => {
                tokens.pop();
                let (expr, fs, ss) = parse_if_statement(tokens);
                v.push(Statement::If(Box::new(expr),fs,ss));
            }
            Some(Token::CloseCurly) => {
                tokens.pop();
                break
            },
            Some(Token::Return) => {
                tokens.pop();
                let ret = parse_expr(tokens);
                v.push(Statement::Return(Box::new(ret)));
            },
            None => break,
            t => {
                println!("start {:?}", t);
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
//            Some(Token::Identifier(name)) => {
//                let nt = clone_nested(tokens.last());
//                match nt {
//                    Some(Token::Equal) => {
//                        tokens.pop();
//                        let ret = parse_expr(tokens);
//                        v.push(Statement::Assignment(Box::new(Expr::Var(name)), Box::new(ret)));
//                    },
//                    Some(Token::OpenParen) => {
//                        let args = function_args(tokens);
//                        v.push(Statement::FunctionCall(Box::new(Expr::Var(name)), args));
//                    },
//                    Some(Token::Dot) => {
//
//                        v.push(method_calls(tokens, Expr::Var(name)))
//                    },
//                    _ => panic!("")
//                }
//            }
        }
    }
    v
}

fn parse_assignment(tokens: &mut Vec<Token>) -> Option<Statement> {
    let l_expr = parse_l_expr(tokens);
    match tokens.pop() {
        Some(Token::Equal) => {
            let expr = parse_expr(tokens);
            expect_token(tokens, Token::SemiColon);
            return Some(Statement::Assignment_2(Box::new(l_expr), Box::new(expr)))
        },
        _ => return None
    }
}

fn parse_function_call(tokens: &mut Vec<Token>) -> Option<Statement> {
    match tokens.pop() {
        Some(Token::Identifier(func)) => {
            let args = function_args(tokens);
            Some(Statement::FunctionCall(Box::new(Expr::Var(func)), args))
        }
        _ => return None
    }
}

fn parse_method_call(tokens: &mut Vec<Token>) -> Option<Statement> {
    None
}

fn parse_if_statement(tokens: &mut Vec<Token>) -> (Expr, Vec<Statement>, Vec<Statement>) {
    let expr = parse_expr(tokens);
    let fs = parse_statements(tokens);
    let t = clone_nested(tokens.last());
    match t {
        Some(Token::Else) => {
            tokens.pop().unwrap();
            let ss = parse_statements(tokens);
            return (expr, fs, ss)
        }
        _ => return (expr, fs, Vec::new())
    }


}

pub fn parse_field_def(tokens: &mut Vec<Token>) -> FieldDef {
    let t = tokens.pop().unwrap();
    match t {
        Token::Identifier(name) => {
            expect_token(tokens, Token::Colon);
            match tokens.pop().unwrap() {
                Token::Identifier(type_name) => FieldDef {name, ty: Type::Named(type_name)},
                Token::Mut => {
                    let type_name = expect_identifier(tokens);
                    FieldDef {name, ty: Type::Cell(Box::new(Type::Named(type_name)))}
                }
                _ => panic!("expected type")
            }
        }
        _ => panic!("expected name found {:?}", t)
    }
}

pub fn parse_method(tokens: &mut Vec<Token>) -> FunctionDefinition {
    let t = tokens.pop().unwrap();
    match t {
        Token::Identifier(name) => {
            expect_token(tokens, Token::Colon);
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
                    Token::CloseParen => break,
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
            return FunctionDefinition{name, arg_types: types, ret_type: Type::Named(ret), cases}
        }
        _ => panic!("")
    }
}

pub fn parse_function_arg(tokens: &mut Vec<Token>) -> (Vec<Type>, Type) {
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
            Token::CloseParen => break,
            _ => panic!("unexpected {:?}", nt)
        }
    }
    expect_token(tokens, Token::RightArrow);

    let nt = tokens.pop().unwrap();

    let ret = match nt {
        Token::Identifier(r) => r,
        _ => panic!("expected return type")
    };
    return (types, Type::Named(ret))
}

pub fn parse_function_decl(tokens: &mut Vec<Token>) -> FunctionDefinition {
    let t = tokens.pop().unwrap();
    match t {
        Token::Identifier(name) => {
            expect_tokens(tokens, &[Token::Colon, Token::Colon, Token::OpenParen]);

            let (arg_types, ret_type) = parse_function_arg(tokens);

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
            return FunctionDefinition{name, arg_types, ret_type, cases}
        }
        _ => panic!("")
    }
}

pub fn method_def(tokens: &mut Vec<Token>) -> FunctionDefinition {
    let t = tokens.pop().unwrap();
    match t {
        Token::Identifier(name) => {
            expect_tokens(tokens, &[Token::Colon, Token::Colon, Token::OpenParen]);

            let nt = tokens.last().unwrap().clone();

            match nt {
                Token::SelfTok => {
                    tokens.pop();

                    let (mut arg_types, ret_type) = parse_function_arg(tokens);
                    arg_types.insert(0, Type::SelfT);
                    let cases = parse_function_body(tokens, &name);
                    return FunctionDefinition{name, arg_types, ret_type, cases}
                },
                _ => {
                    let (arg_types, ret_type) = parse_function_arg(tokens);
                    let cases = parse_function_body(tokens, &name);
                    return FunctionDefinition{name, arg_types, ret_type, cases}
                }
            }
        }
        _ => panic!("")
    }
}

pub fn parse_function_body(tokens: &mut Vec<Token>, name: &String) -> Vec<FuncCase> {
    let mut cases = Vec::new();

    loop {
        let nc = clone_nested(tokens.last());
        match nc {
            Some(Token::Identifier(ref x)) => {
                if x != name {
                    break
                }
                tokens.pop();
                cases.push(parse_func_case(tokens));
            },
            _ => break
        }
    }
    if cases.len() > 0 {
        return cases
    } else {
        panic!("no function cases")
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
            let mut fields = Vec::new();
            let mut funcs = Vec::new();
            loop {
                let mut f = tokens.clone();
                match f.pop().unwrap() {
                    Token::Identifier(_) => {
                        expect_token(&mut f, Token::Colon);
                        match f.pop().unwrap() {
                            Token::Identifier(_) => {
                                fields.push(parse_field_def(tokens));
                            },
                            Token::Mut => {
                                fields.push(parse_field_def(tokens));
                            }
                            _ => panic!("")
                        }
                    },
                    Token::CloseCurly => {
                        tokens.pop();
                        return Declaration::StructDef(name, fields, funcs)

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
            Token::Integer(x) => {
                v.push(Match::Lit(Lit::Integral(x)))
            }
            _ => panic!("unexpected function case {:?}", t)
        }
    }
    let s = parse_statements(tokens);
    FuncCase{matches: v, body: s}
}