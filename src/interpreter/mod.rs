use parsing::ast::*;
use std::collections::HashMap;
use std::borrow::Borrow;

#[derive(Clone, Debug)]
enum Data {
    Int(i64),
    BuiltInFunc(String),
    Closure(FunctionDefinition),
    Method(Box<Data>, String),
    Unit,
    Bool(bool),
    StructVal(String, HashMap<String, Data>, HashMap<String, Data>) //name, fields, method table
}

#[derive(Clone, Debug)]
struct Environment {
    vars: HashMap<String, Data>,
}

pub fn interpret_source_file(sf: SourceFile) {
    let mut globals:HashMap<String, Data> = HashMap::new();
    globals.insert(String::from("print"), Data::BuiltInFunc(String::from("print")));
    for n in sf.decls.iter() {
        match n {
            &Declaration::FunctionDef(ref func) => {globals.insert(func.name.clone(), Data::Closure(func.clone()));},
            &Declaration::StructDef(ref name, ref fields, _) => {
                let mut args_data = HashMap::new();
                for f in fields.iter() {
                    args_data.insert(f.name.clone(), Data::Unit);
                };
                globals.insert(name.clone(), Data::StructVal(name.clone(),args_data, HashMap::new() ));
            }
        }
    }

    let main_func = globals.get("main").unwrap().clone();
    let exe = Executor{globals};
    exe.apply(&main_func, Vec::new());

}

#[derive(Clone, Debug)]
struct Executor {
    globals: HashMap<String,Data>,
}

impl Executor {
    fn apply(&self, func: &Data, args: Vec<Data>) -> Data {
        match func {
            &Data::Closure(ref func_def) => {
                let mut env = Environment{vars: HashMap::new() };
                for cases in func_def.cases.iter() {
                    let res = self.func_match(cases, &args, &mut env);
                    match res {
                        Some(func_case) => {
                            for statements in func_case.body.iter() {
                                match self.exec(&mut env, statements) {
                                    Some(x) => return x,
                                    None => {}
                                }
                            }
                            return Data::Unit
                        }
                        _ => {}
                    }
                }
                panic!("No matching function cases {:?}", func_def)
            },
            &Data::Method(ref var, ref ty) => {
                match var.borrow() {
                    &Data::StructVal(ref name, ref fields, ref funcs) => {
                        match fields.get(ty) {
                            Some(f) => return f.clone(),
                            None => panic!("field {:?}, fields{:?}", ty, fields)
                        }
                    }
                    _ => panic!("")
                }
            }
            &Data::BuiltInFunc(ref name) => {
                if *name == "print" {
                    println!("{:?}", args);
                    return Data::Unit
                } else {
                    panic!("Unknown builtin")
                }},
            _ => panic!("unexpected on apply")
        }
    }

    fn eval(&self, env: &Environment, expr: &Expr) -> Data {
        match expr {
            &Expr::BinaryExpr(ref op, ref left, ref right) => {
                let l = self.eval(env, left);
                let r = self.eval(env, right);
                match op {
                    &BinaryOperator::Add => {
                        match (l, r) {
                            (Data::Int(x), Data::Int(y)) => return Data::Int(x+y),
                            _ => panic!("type error")
                        }
                    }
                    &BinaryOperator::Sub => {
                        match (l, r) {
                            (Data::Int(x), Data::Int(y)) => return Data::Int(x-y),
                            _ => panic!("type error")
                        }
                    }
                    &BinaryOperator::IsEqualTo => {
                        match (l, r) {
                            (Data::Int(x), Data::Int(y)) => return Data::Bool(x == y),
                            _ => panic!("type error")
                        }
                    }
                    _ => panic!("not implemented {:?}", op)
                }
            }
            &Expr::Var(ref name) => {
                match env.vars.get(name) {
                    Some(x) => x.clone(),
                    None_ => {
                        match self.globals.get(name) {
                            Some(x) => x.clone(),
                            None => panic!("variable not in scope {:?}", &name)
                        }
                    }
                }
            }
            &Expr::LiteralUint(n) => Data::Int(n as i64),
            &Expr::App(ref func, ref args) => {
                let func_data = self.eval(env, func);
                let args_data = args.iter().map(|x|{
                    self.eval(env, x)
                }).collect();
                self.apply(&func_data, args_data)
            },
            &Expr::StructInit(ref ty, ref args) => {
                match ty {
                    &Type::Named(ref x) => {
                        let mut fields = HashMap::new();
                        match self.globals.get(x) {
                            Some(x) => {

                            },
                            _ => panic!("not in global")
                        }
                        for &(ref name, ref val) in args.iter() {
                            fields.insert(name.clone(), self.eval(env, &val));
                        };
                        println!("fields {:?}", fields);
                        Data::StructVal(x.clone(), fields, HashMap::new())
                    },
                    &Type::SelfT => panic!("")
                }
            },
            &Expr::MethodCall(ref con, ref field, ref args) => {
                let var = self.eval(env, con);
                let args_data = args.iter().map(|x|{
                    self.eval(env, x)
                }).collect();
                self.apply(&Data::Method(Box::new(var), field.clone()), args_data)
            }
            _ => panic!("not implemented {:?}", expr)
        }
    }

    fn exec(&self, env: &mut Environment, statement: &Statement) -> Option<Data> {
        match statement {
            &Statement::FunctionCall(ref func, ref args) => {
                let func_data = self.eval(env, func);
                let args_data = args.iter().map(|x|{
                   self.eval(env, x)
                }).collect();
                return Some(self.apply(&func_data, args_data))
            }
            &Statement::Assignment(ref name, ref expr) => {
                let x = self.eval(env, expr);
                env.vars.insert(name.clone(), x);
            }
            &Statement::Return(ref expr) => {
                let x = self.eval(env, expr);
                return Some(x)
            }
            &Statement::If(ref con, ref f, ref s) => {
                let x = self.eval(env, con);
                match x {
                    Data::Bool(true) => {
                        for statement in f.iter() {
                            match self.exec(env, statement) {
                                Some(x) => return Some(x),
                                None => {}
                            }
                        }
                    }
                    Data::Bool(false) => {
                        for statement in s.iter() {
                            match self.exec(env, statement) {
                                Some(x) => return Some(x),
                                None => {}
                            }
                        }
                    }
                    _ => panic!("should be bool")
                }
            }
            &Statement::Let(ref name, ref expr) => {
                let x = self.eval(env, expr);
                env.vars.insert(name.clone(), x);
            }
            &Statement::MethodCall(ref con, ref field, ref args) => {
                let var = self.eval(env, con);
                let args_data = args.iter().map(|x|{
                    self.eval(env, x)
                }).collect();
                self.apply(&Data::Method(Box::new(var), field.clone()), args_data);
            }
        }
        return None
    }

    fn func_match(&self, func_case: &FuncCase, args: &Vec<Data>, env: &mut Environment) -> Option<FuncCase> {
        if func_case.matches.is_empty() {
            return Some(func_case.clone())
        }
        for (case, arg) in func_case.matches.iter().zip(args.iter()) {
            if !self.func_match_arg_check(case, arg) {
                return None
            }
        }
        for (case, arg) in func_case.matches.iter().zip(args.iter()) {
            match case {
                &Match::WildCard(ref name) => env.vars.insert(name.clone(), arg.clone()),
                _ => panic!("")
            };
        };
        return Some(func_case.clone())
    }

    fn func_match_arg_check(&self, parameter: &Match, arg: &Data) -> bool{
        match parameter {
            &Match::WildCard(ref name) => true,
            &Match::Constructor(_,_) => false,
        }
    }

    fn func_match_arg_insert(&self, parameter: &Match, arg: Data, env: &mut Environment) {
        match parameter {
            &Match::WildCard(ref name) => {env.vars.insert(name.clone(), arg);},
            &Match::Constructor(_,_) => panic!("Constructor"),
        }
    }
}




