use parsing::ast::*;
use parsing::ast;
use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq)]
enum Type {
    Int,
    Function(Vec<Type>, Box<Type>),
    Struct(String),
    Unit,
    Bool,
}

type Environment = HashMap<String, Type>;

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
            &Declaration::StructDef(ref name, ref fields) => {
                hm.insert(name.clone(), fields.clone());
            },
            _ => panic!("")
        };
    };

    for (name, fields) in hm.iter() {
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

    for func in fhm.iter() {
        let mut arg_types = Vec::new();
        for arg in func.arg_types.iter() {
            let ty = type_checker.eval_type(arg);
            arg_types.push(ty);
        }
        let ret_type = type_checker.eval_type(&func.ret_type);
        type_checker.globals.insert(func.name.clone(), Type::Function(arg_types, Box::new(ret_type)));
    }
    type_checker.eval(&fhm);
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
    pub fn eval(&self, funcs: &Vec<FunctionDefinition>) {
        self.eval_functions(funcs)
    }

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

    fn function_call(&self, func: &Expr, args: &Vec<Expr>, env: &Environment) -> Type {
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
                        let s = &self.structs.get(name).unwrap().0;
                        let method = s.get(field);
                        match method {
                            Some(x) => return x.clone(),
                            None => panic!("")
                        }
                    }
                    _ => panic!("")
                }
            },
            _ => panic!("")
        }
    }

    fn eval_functions(&self, funcs: &Vec<FunctionDefinition>) {
        for func in funcs.iter() {
            let mut v = Vec::new();
            for arg in func.arg_types.iter() {
                v.push(self.eval_type(arg));
            }
            let ret = self.eval_type(&func.ret_type);
        }

        for func in funcs.iter() {
            let ty = expect_function(self.globals.get(&func.name).unwrap());
            for case in func.cases.iter() {
                let mut env:Environment = HashMap::new();
                for (n, case_arg) in case.matches.iter().enumerate() {
                    match case_arg {
                        &Match::WildCard(ref arg) => {
                            env.insert(arg.clone(), ty.0[n].clone());
                        },
                        _ => panic!("")
                    }
                };
                self.eval_body(func, &case.body, &mut env);
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
            }
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
                    }
                }
            },
            &Expr::LiteralUint(_) => Type::Int,
            &Expr::Var(ref n) => env.get(n).unwrap().clone(),
            &Expr::MethodCall(ref target, ref field, ref args) => self.eval_method_call(target, field, args, env),
            x => panic!("{:?} is not implemented", x)
        }
    }
}