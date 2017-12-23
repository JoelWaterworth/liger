use parsing::ast::{SourceFile, Lit};
use parsing::ast;
use std::collections::HashMap;
use std::collections::HashSet;
use type_checker::typed_ast::*;
pub mod typed_ast;

type Environment<'a> = HashMap<&'a String, Type>;

pub fn type_check_ast<'a>(sf: &'a SourceFile) -> Globals {
    let mut functions = Vec::new();
    let mut shm = HashMap::new();
    for decl in &sf.decls {
        match decl {
            &ast::Declaration::FunctionDef(ref x) => {functions.push(x);},
            &ast::Declaration::StructDef(ref name, ref fields, ref methods) => {shm.insert(name, (fields, methods));},
        };
    };
    let mut structs = HashMap::new();
    let struct_iter = shm.iter();
    let mut methods = HashMap::new();
    for (name, &(ref fields, ref funcs)) in struct_iter.clone() {
        let mut f = HashMap::new();
        for field in fields.iter() {
            f.insert(field.name.clone(), Type::from_with_map(&field.ty, &shm));
        };
        let method_type: FunctionTypes = funcs.iter().map(|func|{
            create_func_ty(func, &shm)
        }).collect();
        structs.insert(*name, (Struct{name: (*name).clone(), args: f, methods: HashMap::new()}, method_type));
        methods.insert((*name).clone(), *funcs);
    };

    let function_types:FunctionTypes = functions.iter().map(|func|{
        create_func_ty(func, &shm)
    }).collect();
    let mut typed_functions = HashMap::new();
    typed_functions.insert(String::from("print"),
                           Function{
        name: String::from("print"),
        args_ty: vec![Type::Int],
        ret_ty: Box::new(Type::Unit),
        cases: Body::BuiltInFunc(String::from("print"))
    });

    let mut type_checker = TypeChecker{function_types, structs, typed_functions};
    type_checker.parse_methods(methods);
    type_checker.parse_functions(functions);
    type_checker.global()
}

impl Type {
    pub fn from_with_map<T>(ty: &ast::Type, map: &HashMap<&String, T>) -> Type {
        match ty {
            &ast::Type::Named(ref x) => match x.as_str() {
                "i64" => Type::Int,
                "IO" => Type::Unit,
                _ => {
                    match map.get(x) {
                        Some(_) => Type::Struct(x.clone()),
                        None => panic!("{:?} is not expected", x)
                    }
                }
            }
            _ => panic!("")
        }
    }
}
type FunctionTypes<'a> = HashMap<&'a String, FunctionType>;
type FunctionType = (Vec<Type>, Type);

fn create_func_ty<'a, T>(func: &'a ast::FunctionDefinition, map: &HashMap<&String, T>) -> (&'a String, FunctionType) {
    let name = &func.name;
    let ret = Type::from_with_map( &func.ret_type, map);
    let arg_types:Vec<Type> = func.arg_types.iter().map(|arg| {
        Type::from_with_map(arg,map)
    }).collect();
    (name, (arg_types, ret))
}

struct TypeChecker<'a> {
    function_types: FunctionTypes<'a>,
    structs: HashMap<&'a String, (Struct, FunctionTypes<'a>)>,
    typed_functions: HashMap<String, Function>
}

impl<'a> TypeChecker<'a> {
    pub fn parse_methods(&mut self, methods: HashMap<String, &Vec<ast::FunctionDefinition>>) {
        for (name, ms) in methods.iter() {
            let methods = (*ms).into_iter().map(|method| {
                (method.name.clone(), self.eval_func(method, self.structs.get(name).unwrap().1.get(&method.name).unwrap()))
            }).collect();
            if let Some(x) = self.structs.get_mut(name){
                x.0.methods = methods;
            }
        }
    }

    pub fn parse_functions(&mut self, funcs: Vec<&ast::FunctionDefinition>) {
        for func in funcs.iter() {
            let function = self.eval_func(*func, self.function_types.get(&func.name).unwrap());
            self.typed_functions.insert(func.name.clone(), function);
        }
    }

    pub fn eval_func(&self, func: &ast::FunctionDefinition, func_ty: &FunctionType) -> Function {
        let args_ty = func_ty.0.clone();
        let cases = self.eval_func_cases(&func.cases, &args_ty, &func_ty.1);
        Function{name: func.name.clone(), args_ty, ret_ty: Box::new(func_ty.1.clone()), cases}
    }

    fn eval_func_cases<'b>(&self, cases: &'b Vec<ast::FuncCase>, arg_type: &'b Vec<Type>, ret_type: &'a Type) -> Body {
        Body::Cases(
            cases.iter().map(|case: &'b ast::FuncCase| {
                let mut env: Environment<'b> = HashMap::new();
                let pattern: Vec<Pattern> = case.matches.iter().zip(arg_type).map(|(ref m, ref ty)| {
                    match m {
                        &&ast::Match::WildCard(ref name) => {
                            env.insert(name, (*ty).clone());
                            Pattern::Binding {name: name.clone(), ty: (*ty).clone()}
                        },
                        &&ast::Match::Lit(ast::Lit::Integral(n)) => {
                            if **ty != Type::Int {
                                panic!("")
                            }
                            Pattern::Constant {value: ConstVal{ty: (*ty).clone(), val: ast::Lit::Integral(n)}}
                        }
                        _ => unimplemented!()
                    }
                }).collect();
                let statements = self.eval_statements(&case.body, &mut env, ret_type);
                Case{args: pattern, statements}
            }).collect()
        )
    }

    fn eval_expr(&self, expr: &ast::Expr, env: &Environment) -> (Expr, Type) {
        match expr {
            &ast::Expr::StructInit(ref ty, ref args) => {
                match ty {
                    &ast::Type::Named(ref x) => {
                        let s = &self.structs.get(x).unwrap().0;
                        let mut fields = Vec::new();
                        for &(ref arg_name, ref arg_expr) in args.into_iter() {
                            let arg_type = s.args.get(arg_name).unwrap();
                            let (typed_expr, expr_type) = self.eval_expr(arg_expr, env );
                            if arg_type != &expr_type {
                                panic!("{:?} should be {:?}, not {:?}", arg_name, arg_type, expr_type)
                            };
                            fields.push((arg_name.clone(), typed_expr));
                        }
                        (Expr::StructInit(Type::Struct(x.clone()), fields), Type::Struct(x.clone()))
                    },
                    &ast::Type::SelfT => panic!("")
                }
            },
            &ast::Expr::Lit(Lit::Integral(n)) => (Expr::Lit(Lit::Integral(n.clone())), Type::Int),
            &ast::Expr::Var(ref n) => (Expr::Var(n.clone()), env.get(n).unwrap().clone()),
            &ast::Expr::MethodCall(ref target, ref field, ref args) =>
                (Expr::MethodCall(Box::new(self.eval_expr(target, env).0), field.clone(), args.iter().map(|e| {
                    self.eval_expr(e, env).0
                }).collect()), self.eval_method_call(target, field, args, env)),
            &ast::Expr::App(ref target, ref args) => {
                let (t, a, r) = self.function_call(target, args, env);
                (Expr::App(Box::new(t), a), r)
            },
            &ast::Expr::BinaryExpr(ref op, ref left, ref right) => (
                    Expr::BinaryExpr(op.clone(),
                                  Box::new(self.eval_expr(left, env).0),
                                  Box::new(self.eval_expr(right, env).0)),
                 self.eval_operator(op, left, right, env)
                ),
            x => panic!("{:?} is not implemented", x)
        }
    }

    fn eval_operator(&self, op: &ast::BinaryOperator, left: &ast::Expr, right: &ast::Expr, env: &Environment) -> Type {
        let l = self.eval_expr(left, env);
        let r = self.eval_expr(right, env);
        match op {
            &ast::BinaryOperator::Add => {
                match (l.1, r.1) {
                    (Type::Int, Type::Int) => Type::Int,
                    x => panic!("{:?} is not implemented", x)
                }
            },
            x => panic!("{:?} is not implemented", x)
        }
    }

    fn eval_method_call(&self, target: &ast::Expr, field: &String, args: &Vec<ast::Expr>, env: &Environment) -> Type {
        match target {
            &ast::Expr::Var(ref name) => {
                match env.get(name) {
                    Some(&Type::Struct(ref name)) => {
                        let s = self.structs.get(name).unwrap();
                        match s.0.args.get(field) {
                            Some(x) => return x.clone(),
                            None => {
                                match s.1.get(field) {
                                    Some(&(ref arg_ty, ref ret_ty)) => ret_ty.clone(),
                                    _ => panic!("")
                                }
                            }
                        }
                    }
                    _ => panic!("")
                }
            },
            x => panic!("{:?} is not implermented", x)
        }
    }

    fn eval_statements<'b>(&self, statements: &'b Vec<ast::Statement>, env: &mut Environment<'b> ,ret_type: &Type) -> Vec<Statement> {
        statements.iter().map(|statement: &'b ast::Statement| {
            match statement {
                &ast::Statement::Let(ref name, ref expr) => {
                    let (expr, ty) = self.eval_expr(expr, env);
                    env.insert(name, ty.clone());
                    Statement::Let {name: name.clone(), ty, expr: Box::new(expr)}
                },
                &ast::Statement::LetMut(ref name, ref expr) => {
                    let (expr, ty) = self.eval_expr(expr, env);
                    env.insert(name, Type::Cell(Box::new(ty.clone())));
                    Statement::Let {name: name.clone(), ty: Type::Cell(Box::new(ty)), expr: Box::new(expr)}
                }
                &ast::Statement::Return(ref expr) => {
                    let ty = self.eval_expr(expr, env);
                    if ty.1 == *ret_type {
                        Statement::Return {ty: ty.1, expr: Box::new(ty.0)}
                    } else {
                        panic!("")
                    }
                },
                &ast::Statement::Assignment(ref name, ref expr) => {
                    let (expr, ty) = self.eval_expr(expr, env);
                    let var_ty = env.get(name);
                    match var_ty {
                        Some(&Type::Cell(ref c_ty)) => {
                            if **c_ty == ty {
                                Statement::Assignment{name: name.clone(), expr: Box::new(expr)}
                            } else {
                                panic!("the expression yeilds the wrong type")
                            }
                        },
                        _ => panic!("assignment on immutable type")
                    }
                }
                &ast::Statement::FunctionCall(ref expr, ref args) => {
                    let (t, a, r) = self.function_call(expr,args,env);
                    Statement::FunctionCall(Box::new(t), a)
                }
                _ => unimplemented!()
            }
        }).collect()
    }
    pub fn function_call(&self, func: &ast::Expr, args: &Vec<ast::Expr>, env: &Environment) -> (Expr, Vec<Expr>, Type) {
        match func {
            &ast::Expr::Var(ref name) => {
                let ty = self.function_types.get(name);
                match ty {
                    Some(&(ref args_ty, ref ret_type)) => {
                        let mut typed_args = Vec::new();
                        for (arg, ty) in args.iter().zip(args_ty.iter()) {
                            let eval_arg = self.eval_expr(arg, env);
                            if ty.clone() != eval_arg.1 {
                                panic!("expected {:?} not {:?}", ty, eval_arg)
                            }
                            typed_args.push(eval_arg.0);
                        };
                        return ( Expr::Var(name.clone()), typed_args, ret_type.clone())
                    },
                    None => {
                        match self.typed_functions.get(name) {
                            Some(&Function{ref name, ref args_ty, ref ret_ty, ref cases}) => {
                                let mut typed_args = Vec::new();
                                for (arg, ty) in args.iter().zip(args_ty.iter()) {
                                    let eval_arg = self.eval_expr(arg, env);
                                    if ty.clone() != eval_arg.1 {
                                        panic!("expected {:?} not {:?}", ty, eval_arg)
                                    }
                                    typed_args.push(eval_arg.0);
                                };
                                return ( Expr::Var(name.clone()), typed_args, *ret_ty.clone())
                            },
                            _ => panic!("")
                        }
                    }
                }
            },
            _ => panic!("")
        }
    }

    pub fn global(&self) -> Globals {
        let structs:HashSet<Struct> = self.structs.iter().map(|(_, &(ref s, _))| {
            s.clone()
        }).collect();
        let functions:HashSet<Function> = self.typed_functions.iter().map(|(_, ref s)| {
            (*s).clone()
        }).collect();
        Globals { functions, structs}
    }
}

/*
pub fn type_check_source_file(sf: &SourceFile) {
    let mut type_checker = TypeChecker{globals: HashMap::new(), structs: HashMap::new()};
    type_checker.globals.insert(String::from("print"), Type::Function(vec![Type::Int], Box::new(Type::Unit)));
    let mut fhm = Vec::new();
    let mut hm = HashMap::new();
    for n in sf.decls.iter() {
        match n {
            &Declaration::FunctionDef(ref func) => {
                fhm.push(func.clone());
            },
            &Declaration::StructDef(ref name, ref fields, ref funcs) => {
                hm.insert(name.clone(), (fields.clone(), funcs.clone()));
            }
        };
    };
    let mut methods = Vec::new();

    for (name, &(ref fields, ref func)) in hm.iter() {
        let mut fds = HashMap::new();
        for field in fields.iter() {
            let ty = match field.ty {
                ast::Type::Named(ref x) => match x.as_str() {
                    "i64" => Type::Int,
                    _ => {
                        if hm.contains_key(x) {
                            Type::Struct(x.clone())
                        } else {
                            panic!("{:?} could not be found", x)
                        }
                    }
                },
                _ => panic!("")
            };
            match fds.insert(field.name.clone(), ty) {
                Some(x) => panic!("{:?}", x),
                None => {}
            };
        }

        match type_checker.structs.insert(name.clone(), (fds, HashMap::new()))  {
            Some(x) => panic!("{:?}", x),
            None => {}
        };
    };
    for (name, &(_, ref func)) in hm.iter() {
        let mut functions = HashMap::new();
        for f in func.iter() {
            let mut arg_types = Vec::new();
            for arg in f.arg_types.iter() {
                let ty = type_checker.eval_type(arg);
                arg_types.push(ty);
            }
            let ret_type = type_checker.eval_type(&f.ret_type);
            functions.insert(f.name.clone(), Type::Function(arg_types, Box::new(ret_type)));
        }
        type_checker.structs.get_mut(name).unwrap().1 = functions;
        methods.push((name.clone(), func.clone()));
    }

    for func in fhm.iter() {
        let mut arg_types = Vec::new();
        for arg in func.arg_types.iter() {
            let ty = type_checker.eval_type(arg);
            arg_types.push(ty);
        }
        let ret_type = type_checker.eval_type(&func.ret_type);
        type_checker.globals.insert(func.name.clone(), Type::Function(arg_types, Box::new(ret_type)));
    };
    type_checker.eval_functions(&fhm);
    type_checker.eval_methods(&methods);
}

#[derive(Clone, Debug)]
struct TypeChecker {
    globals: HashMap<String,Type>,
    structs: HashMap<String,(HashMap<String, Type>, HashMap<String, Type>)>,
}

fn expect_function(typ: &Type) -> (Vec<Type>, Box<Type>) {
    match typ {
        &Type::Function(ref ty, ref ret) => (ty.clone(), ret.clone()),
        _ => panic!("")
    }
}

impl TypeChecker {

    fn eval_body(&self, func_def: &FunctionDefinition, statements: &Vec<Statement>,env: &mut Environment) {
        let return_type = self.eval_type(&func_def.ret_type);
        for statement in statements.iter() {
            match statement {
                &Statement::Let(ref name, ref expr) => {
                    let expr_type = self.eval_expr(env, expr);
                    env.insert(name.clone(), expr_type);
                },
                &Statement::Return(ref expr) => {
                    let expr_type = self.eval_expr(env, expr);
                    if expr_type != return_type {
                        panic!("expected {:?} not {:?}", return_type, expr_type)
                    }
                }
                &Statement::FunctionCall(ref func, ref args) => {self.function_call(func, args, env);},
                _ => panic!("{:?} is not implemented", statement)
            }
        };
    }

    pub fn function_call(&self, func: &Expr, args: &Vec<Expr>, env: &Environment) -> Type {
        match func {
            &Expr::Var(ref name) => {
                let ty = self.globals.get(name);
                match ty {
                    Some(&Type::Function(ref arg_types, ref ret_type)) => {
                        for (arg, ty) in args.iter().zip(arg_types.iter()) {
                            let eval_arg = self.eval_expr(env, arg);
                            if ty.clone() != eval_arg {
                                panic!("expected {:?} not {:?}", ty, eval_arg)
                            }
                            return *ret_type.clone()
                        }
                        panic!("")
                    },
                    _ => panic!("")
                }
            },
            _ => panic!("")
        }
    }

    fn eval_method_call(&self, target: &Expr, field: &String, args: &Vec<Expr>, env: &Environment) -> Type {
        match target {
            &Expr::Var(ref name) => {
                match env.get(name) {
                    Some(&Type::Struct(ref name)) => {
                        let s = self.structs.get(name).unwrap();
                        match s.0.get(field) {
                            Some(x) => return x.clone(),
                            None => {
                                match s.1.get(field) {
                                    Some(&Type::Function(ref arg_ty, ref ret_ty)) => *ret_ty.clone(),
                                    _ => panic!("")
                                }
                            }
                        }
                    }
                    _ => panic!("")
                }
            },
            x => panic!("{:?} is not implermented", x)
        }
    }

    pub fn eval_functions(&self, funcs: &Vec<FunctionDefinition>) {
        for func in funcs.iter() {
            let ty = expect_function(self.globals.get(&func.name).unwrap());
            for case in func.cases.iter() {
                let mut env:Environment = HashMap::new();
                for (n, case_arg) in case.matches.iter().enumerate() {
                    match case_arg {
                        &Match::WildCard(ref arg) => {
                            let x = match ty.0.get(n) {
                                Some(y) => y.clone(),
                                None => panic!("{:?} \n {:?}", func, ty)
                            };
                            env.insert(arg.clone(), ty.0[n].clone());
                        },
                        _ => panic!("")
                    }
                };
                self.eval_body(func, &case.body, &mut env);
            }
        }
    }

    pub fn eval_methods(&self, structs: &Vec<(String, Vec<FunctionDefinition>)>) {
        for &(ref name, ref funcs) in structs.iter() {
            let str = &self.structs.get(name).unwrap().1;
            for func in funcs.iter() {
                let ty = str.get(&func.name).unwrap();
                let (arg_type, ret_type) = match ty {
                    &Type::Function(ref args, ref ret) => (args, ret),
                    _ => panic!("")
                };

                for case in func.cases.iter() {
                    let mut env:Environment = HashMap::new();
                    for (n, case_arg) in case.matches.iter().enumerate() {
                        match case_arg {
                            &Match::WildCard(ref arg) => {
                                env.insert(arg.clone(), arg_type[n].clone());
                            },
                            _ => panic!("")
                        }
                    };
                    self.eval_body(func, &case.body, &mut env);
                }
            }
        }
    }

    fn eval_type(&self, ty: &ast::Type) -> Type {
        match ty {
            &ast::Type::Named(ref x) => match x.as_str() {
                "i64" => Type::Int,
                "IO" => Type::Unit,
                _ => {
                    if self.structs.contains_key(x) {
                        Type::Struct(x.clone())
                    } else {
                        panic!("{:?} could not be found", x)
                    }
                }
            },
            &ast::Type::SelfT => panic!("")
        }
    }
    fn eval_expr(&self, env: &Environment, expr: &Expr)-> Type {
        match expr {
            &Expr::StructInit(ref ty, ref args) => {
                match ty {
                    &ast::Type::Named(ref x) => {
                        let s = &self.structs.get(x).unwrap().0;
                        for &(ref arg_name, ref arg_expr) in args.iter() {
                            let arg_type = s.get(arg_name).unwrap();
                            let expr_type = self.eval_expr(env, arg_expr);
                            if arg_type != &expr_type {
                                panic!("{:?} should be {:?}, not {:?}", arg_name, arg_type, expr_type)
                            }
                        }
                        Type::Struct(x.clone())
                    },
                    &ast::Type::SelfT => panic!("")
                }
            },
            &Expr::Lit(Lit::Integral(_)) => Type::Int,
            &Expr::Var(ref n) => env.get(n).unwrap().clone(),
            &Expr::MethodCall(ref target, ref field, ref args) => self.eval_method_call(target, field, args, env),
            &Expr::App(ref target, ref args) => self.function_call(target, args, env),
            &Expr::BinaryExpr(ref op, ref left, ref right) => self.eval_operator(op, left, right, env),
            x => panic!("{:?} is not implemented", x)
        }
    }

    fn eval_operator(&self, op: &BinaryOperator, left: &Expr, right: &Expr, env: &Environment) -> Type {
        let l = self.eval_expr(env, left);
        let r = self.eval_expr(env, right);
        match op {
            &BinaryOperator::Add => {
                match (l,r) {
                    (Type::Int, Type::Int) => Type::Int,
                    x => panic!("{:?} is not implemented", x)
                }
            },
            x => panic!("{:?} is not implemented", x)
        }
    }
}
*/