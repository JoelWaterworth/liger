use type_checker::typed_ast::*;
use parsing::ast::{BinaryOperator, Lit};
use std::collections::HashMap;
use std::borrow::Borrow;
use std::rc::Rc;
use std::cell::Cell;

type Ref<T>  = Rc<Cell<T>>;
type RData = Ref<RData>;

#[derive(Debug, Clone)]
enum Data {
    Int(u64),
    Closure(Function),
    Method(Box<Data>, String),
    Unit,
    Bool(bool),
    StructVal(String, HashMap<String, Data>, HashMap<String, Data>), //name, fields, method table
    Class(String, HashMap<String, Data>), //name, fields
}

#[derive(Clone, Debug)]
struct Environment {
    vars: HashMap<String, Data>,
}

pub fn interpret_source_file(tast: Globals) {
    let mut globals:HashMap<String, Data> = HashMap::new();
    let mut types:HashMap<String,HashMap<String,Data>> = HashMap::new();

    for s in tast.structs.into_iter() {
        let args_data: HashMap<String,Data> =  s.args.iter().map(|(name, _)| {
            (name.clone(), Data::Unit)
        }).collect();

        let functions:HashMap<String,Data> = s.methods.into_iter().map(|(name, f)| {
            (name, Data::Closure(f) )
        }).collect();

        types.insert(s.name.clone(), functions);
    }

    for f in tast.functions.iter() {
        globals.insert(f.name.clone(), Data::Closure(f.clone()));
    }

    let main_func = globals.get("main").unwrap().clone();
    let exe = Executor{globals, types};
    exe.apply(&main_func, Vec::new());
}

#[derive(Clone, Debug)]
struct Executor {
    globals: HashMap<String,Data>,
    types: HashMap<String,HashMap<String,Data>>,
}

impl Executor {
    fn apply(&self, func: &Data, args: Vec<Data>) -> Data {
        match func {
            &Data::Closure(ref func_def) => {
                match func_def.cases {
                    Body::BuiltInFunc(ref name) => {
                        if *name == "print" {
                            println!("{:?}", args);
                            return Data::Unit
                        } else {
                            panic!("Unknown builtin")
                        }
                    },
                    Body::Cases(ref cases) => {
                        for cases in cases.iter() {
                            let res = self.func_match(cases, &args);
                            match res {
                                Some(x) => {
                                    let (statements, mut env) = x;
                                    for statement in statements.iter() {
                                        match self.exec(&mut env, statement) {
                                            Some(x) => return x,
                                            None => {}
                                        }
                                    }
                                    return Data::Unit
                                },
                                None => {}
                            }
                        }
                    }
                }
                panic!("No matching function cases {:?}", func_def)
            },
            &Data::Method(ref var, ref ty) => {
                match var.borrow() {
                    &Data::Class(ref name, ref fields) => {
                        let funcs = self.types.get(name).unwrap();
                        match fields.get(ty) {
                            Some(f) => return f.clone(),
                            None => {
                                match funcs.get(ty) {
                                    Some(fun) => {
                                        self.apply( fun, args)
                                    },
                                    None => panic!("{:?} is unkown. {:?} {:?} could be used instead", ty, fields, funcs)
                                }
                            }
                        }
                    }
                    _ => panic!("{:?}", var)
                }
            },
            _ => panic!("")
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
                            (x, y) => panic!("type error with {:?}, {:?}", x, y)
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
                    None => {
                        match self.globals.get(name) {
                            Some(x) => x.clone(),
                            None => {
                                panic!("variable not in scope {:?}", &name)
                            }
                        }
                    }
                }
            }
            &Expr::Lit(Lit::Integral(n)) => Data::Int(n),
            &Expr::App(ref func, ref args) => {
                let func_data = self.eval(env, func);
                let args_data = args.iter().map(|x|{
                    self.eval(env, x)
                }).collect();
                self.apply(&func_data, args_data)
            },
            &Expr::StructInit(ref ty, ref args) => {
                match ty {
                    &Type::Struct(ref x) => {
                        let mut fields = HashMap::new();
                        for &(ref name, ref val) in args.iter() {
                            fields.insert(name.clone(), self.eval(env, &val));
                        };
                        Data::Class(x.clone(), fields)
                    },
                    _ => panic!("")
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
                self.apply(&func_data, args_data);
            }
            &Statement::Return{ref ty, ref expr} => {
                let x = self.eval(env, expr);
                return Some(x)
            }
            &Statement::If{ref condition, ref true_statements, ref false_statements} => {
                let x = self.eval(env, condition);
                match x {
                    Data::Bool(true) => {
                        for statement in true_statements.iter() {
                            match self.exec(env, statement) {
                                Some(x) => return Some(x),
                                None => {}
                            }
                        }
                    }
                    Data::Bool(false) => {
                        for statement in false_statements.iter() {
                            match self.exec(env, statement) {
                                Some(x) => return Some(x),
                                None => {}
                            }
                        }
                    }
                    _ => panic!("should be bool")
                }
            }
            &Statement::Let{ref ty, ref name, ref expr} => {
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
            &Statement::Assignment{ref l_expr, ref expr} => {
                let x = self.eval(env, expr);
                *self.eval_l_expr(l_expr, env) = x;
            }
            x => panic!("not implemented {:?}", x)
        }
        return None
    }

    fn eval_l_expr<'a>(&self, l_expr: &LExpr, env: &'a mut Environment) -> &'a mut Data {
        match l_expr {
            &LExpr::Var(ref name) => env.vars.get_mut(name).unwrap(),
            &LExpr::MethodCall(ref var, ref field, _) => {
                match self.eval_l_expr(var, env) {
                    &mut Data::Class(ref name, ref mut fields) => {
                        fields.get_mut(field).unwrap()
                    }
                    x => panic!("{:?}", x)
                }
            }
            _ => panic!("{:?}", l_expr)
        }
    }

    fn func_match<'a>(&self, case: &'a Case, args: &Vec<Data>) -> Option<(&'a Vec<Statement>, Environment)> {
        let mut env = Environment{vars: HashMap::new() };
        if case.args.is_empty() {
            return Some((&case.statements, env))
        }
        for (case, arg) in case.args.iter().zip(args.iter()) {
            if !self.func_match_arg_check(case, arg, &mut env) {
                return None
            }
        }
        return Some((&case.statements, env))
    }

    fn func_match_arg_check(&self, parameter: &Pattern, arg: &Data, env: &mut Environment) -> bool {
        match parameter {
            &Pattern::Binding{ref name, ref ty } => {
                env.vars.insert(name.clone(), arg.clone());
                return true
            },
            &Pattern::Constant {value: ConstVal{ref ty, val: Lit::Integral(ref n)}} => {
                match arg {
                    &Data::Int(ref m) => if n == m {
                        return true
                    } else {
                        return false
                    },
                    _ => panic!("")
                }
            }
            _ => false
        }
    }
}




