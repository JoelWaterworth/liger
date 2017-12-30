use type_checker::typed_ast::*;
use parsing::ast::{BinaryOperator, Lit};
use std::collections::HashMap;
use std::cell::RefCell;
use std::rc::Rc;
use std::ops::Deref;
use std::ops::DerefMut;

type RData = Rc<RefCell<Data>>;

fn rdata(data: Data) -> RData {
    Rc::new(RefCell::new(data))
}

#[derive(Debug, Clone)]
enum Data {
    Int(u64),
    Closure(Function),
    Method(RData, String),
    Unit,
    Bool(bool),
    Class(String, HashMap<String, RData>), //name, fields
}

#[derive(Clone, Debug)]
struct Environment {
    vars: HashMap<String, RData>,
}

pub fn interpret_source_file(tast: Globals) {
    let mut globals:HashMap<String, RData> = HashMap::new();
    let mut types:HashMap<String,HashMap<String,RData>> = HashMap::new();

    for s in tast.structs.into_iter() {
        let args_data: HashMap<String,RData> =  s.args.iter().map(|(name, _)| {
            (name.clone(), rdata(Data::Unit))
        }).collect();

        let functions:HashMap<String,RData> = s.methods.into_iter().map(|(name, f)| {
            (name, rdata(Data::Closure(f)) )
        }).collect();

        types.insert(s.name.clone(), functions);
    }

    for f in tast.functions.iter() {
        globals.insert(f.name.clone(), rdata(Data::Closure(f.clone())));
    }

    let main_func = globals.get("main").unwrap().clone();
    let exe = Executor{globals, types};
    exe.apply(&main_func, Vec::new());
}

#[derive(Clone, Debug)]
struct Executor {
    globals: HashMap<String,RData>,
    types: HashMap<String,HashMap<String,RData>>,
}

impl Executor {
    fn apply(&self, func: &RData, args: Vec<RData>) -> RData {
        match *(func).borrow() {
            Data::Closure(ref func_def) => {
                match func_def.cases {
                    Body::BuiltInFunc(ref name) => {
                        if *name == "print" {
                            println!("{:?}", args);
                            return rdata(Data::Unit)
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
                                    return rdata(Data::Unit)
                                },
                                None => {}
                            }
                        }
                    }
                }
                panic!("No matching function cases {:?}", func_def)
            },
            Data::Method(ref var, ref ty) => {
                match var.borrow().deref() {
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

    fn eval(&self, env: &Environment, expr: &Expr) -> RData {
        match expr {
            &Expr::BinaryExpr(ref op, ref left, ref right) => {
                let rl = self.eval(env, left);
                let rr = self.eval(env, right);
                let rrl = rl.borrow();
                let rrr = rr.borrow();
                let l = rrl.deref();
                let r = rrr.deref();
                match op {
                    &BinaryOperator::Add => {
                        match (l, r) {
                            (&Data::Int(x), &Data::Int(y)) => return rdata(Data::Int(x+y)),
                            (x, y) => panic!("type error with {:?}, {:?}", x, y)
                        }
                    }
                    &BinaryOperator::Sub => {
                        match (l, r) {
                            (&Data::Int(x), &Data::Int(y)) => return rdata(Data::Int(x-y)),
                            _ => panic!("type error")
                        }
                    }
                    &BinaryOperator::IsEqualTo => {
                        match (l, r) {
                            (&Data::Int(x), &Data::Int(y)) => return rdata(Data::Bool(x == y)),
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
            &Expr::Lit(Lit::Integral(n)) => rdata(Data::Int(n)),
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
                        rdata(Data::Class(x.clone(), fields))
                    },
                    _ => panic!("")
                }
            },
            &Expr::MethodCall(ref con, ref field, ref args) => {
                let var = self.eval(env, con);
                let args_data = args.iter().map(|x|{
                    self.eval(env, x)
                }).collect();
                self.apply(&rdata(Data::Method(var, field.clone())), args_data)
            }
            _ => panic!("not implemented {:?}", expr)
        }
    }

    fn exec(&self, env: &mut Environment, statement: &Statement) -> Option<RData> {
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
                let rx = x.borrow();
                match *rx {
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
                self.apply(&rdata(Data::Method(var, field.clone())), args_data);
            }
            &Statement::Assignment{ref l_expr, ref expr} => {
                let x = self.eval(env, expr);
                let y = self.eval_l_expr(l_expr, env);
                *y.borrow_mut() = x.borrow().deref().clone();
            }
            x => panic!("not implemented {:?}", x)
        }
        return None
    }

    fn eval_l_expr<'a>(&self, l_expr: &LExpr, env: &'a Environment) -> RData {
        match l_expr {
            &LExpr::Var(ref name) => env.vars.get(name).unwrap().clone(),
            &LExpr::MethodCall(ref var, ref field, _) => {
                let mut l = self.eval_l_expr(var, env).clone();
                let mut rl = l.borrow();
                let mut rrl = rl.deref();
                match rrl {
                    &Data::Class(ref name, ref fields) => {
                        fields.get(field).unwrap().clone()
                    }
                    x => panic!("{:?}", x)
                }
            }
            _ => panic!("{:?}", l_expr)
        }
    }

    fn func_match<'a>(&self, case: &'a Case, args: &Vec<RData>) -> Option<(&'a Vec<Statement>, Environment)> {
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

    fn func_match_arg_check(&self, parameter: &Pattern, arg: &RData, env: &mut Environment) -> bool {
        match parameter {
            &Pattern::Binding{ref name, ref ty } => {
                env.vars.insert(name.clone(), arg.clone());
                return true
            },
            &Pattern::Constant {value: ConstVal{ref ty, val: Lit::Integral(ref n)}} => {
                match arg.borrow().deref() {
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




