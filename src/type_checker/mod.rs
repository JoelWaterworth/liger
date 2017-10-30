use parsing::ast::*;
use parsing::ast;
use std::collections::HashMap;

#[derive(Clone, Debug)]
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
        match type_checker.structs.insert(name.clone(), fds)  {
            Some(x) => panic!("{:?}", x),
            None => {}
        };
    };
    type_checker.eval(fhm);
}

#[derive(Clone, Debug)]
struct TypeChecker {
    globals: HashMap<String,Type>,
    structs: HashMap<String,HashMap<String, Type>>,
}

fn expect_function(typ: &Type) -> (Vec<Type>, Box<Type>) {
    match typ {
        &Type::Function(ref ty, ref ret) => (ty.clone(), ret.clone()),
        _ => panic!("")
    }
}

impl TypeChecker {
    pub fn eval(&mut self, funcs: Vec<FunctionDefinition>) {
        self.eval_functions(&funcs);
    }
    fn eval_functions(&mut self, funcs: &Vec<FunctionDefinition>) {
        for func in funcs.iter() {
            let mut v = Vec::new();
            for arg in func.arg_types.iter() {
                v.push(self.eval_type(arg));
            }
            let ret = self.eval_type(&func.ret_type);
            self.globals.insert(func.name.clone(), Type::Function(v, Box::new(ret)));
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
                for statement in case.body.iter() {
                    match statement {
                        &Statement::Let(ref expr) => {

                        }
                    }
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
            _ => panic!("")
        }
    }
    fn eval_expr(&self, env: &Environment, expr: &Expr) {
        match expr {
            &Expr::StructInit(ref ty, ref args) => {
                match ty {
                    &Type::Named(ref x) => {
                        let s = self.structs.get(x).unwrap();
                        let v = Vec::new();
                        for (arg_name, arg_expr) in args.iter() {

                        }
                    }
                }
            }
        }
    }
}