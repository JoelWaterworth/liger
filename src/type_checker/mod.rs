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
        let struct_type = Type::Struct((*name).clone());
        let method_type: FunctionTypes = funcs.iter().map(|func|{
            create_method_ty(&struct_type, func,&shm)
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
            },
            &ast::Type::Cell(ref x) => Type::Cell(Box::new(Type::from_with_map(x.as_ref(), map))),
            _ => panic!("{:?}", ty)
        }
    }

    pub fn method_type<T>(ty: &ast::Type, self_type: &Type, map: &HashMap<&String, T>) -> Type {
        match ty {
            &ast::Type::SelfT => self_type.clone(),
            _ => Type::from_with_map(ty, map)
        }
    }
    pub fn get_nested_cell_type(&self)-> Type {
        match self {
            &Type::Cell(ref x) => x.as_ref().clone(),
            _ => self.clone(),
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

fn create_method_ty<'a, T>(stru: &Type, func: &'a ast::FunctionDefinition, map: &HashMap<&String, T>) -> (&'a String, FunctionType) {
    let name = &func.name;
    let ret = Type::method_type( &func.ret_type, stru, map);
    let arg_types:Vec<Type> = func.arg_types.iter().map(|arg| {
        Type::method_type(arg,stru,map)
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
                    &ast::Type::SelfT => panic!("SelfT unimplemented"),
                    &ast::Type::Cell(ref ty) => panic!("Cell unimplemented"),
                }
            },
            &ast::Expr::Lit(Lit::Integral(n)) => (Expr::Lit(Lit::Integral(n.clone())), Type::Int),
            &ast::Expr::Var(ref n) => (Expr::Var(n.clone()), env.get(n).unwrap().clone()),
            &ast::Expr::MethodCall(ref target, ref field, ref args) => {
                let (eval_target, arg_expr, ret_type) = self.eval_method_call(&target, field, &args, env);
                (Expr::MethodCall(Box::new(eval_target.0), field.clone(), arg_expr), ret_type)
            },
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
                match (l.1.get_nested_cell_type(), r.1.get_nested_cell_type(),) {
                    (Type::Int, Type::Int) => Type::Int,
                    x => panic!("{:?} is not implemented", x)
                }
            },
            x => panic!("{:?} is not implemented", x)
        }
    }

    fn type_from_struct_call(&self, name: &String, field: &String, args: &Vec<(Expr, Type)>) -> Type {
        let s = self.structs.get(name).unwrap();
        match s.0.args.get(field) {
            Some(x) => return x.clone(),//field
            None => {
                match s.1.get(field) {
                    Some(&(ref arg_ty, ref ret_ty)) => {
                        arg_ty.iter().zip(args.iter()).for_each(|(x,&(ref expr, ref ty))|{
                            if *x != *ty {
                                panic!("wrong type supplied on field {}", field)
                            }
                        });
                        ret_ty.clone()
                    },//function
                    _ => panic!("field not found {:?}", field)
                }
            }
        }
    }

    fn eval_method_call(&self, target: &ast::Expr, field: &String, args: &Vec<ast::Expr>, env: &Environment) -> ((Expr, Type), Vec<Expr>, Type) {
        let eval_target = self.eval_expr(target, env);
        let mut eval_args: Vec<(Expr, Type)> = args.iter().map(|e| {
            self.eval_expr(e, env)
        }).collect();
        eval_args.insert(0, eval_target.clone());
        let (arg_expr, _): (Vec<_>, Vec<_>) = eval_args.iter().cloned().unzip();

        let ret_type = match eval_target.1 {
            Type::Struct(ref name) => self.type_from_struct_call(name, field, &eval_args),
            Type::Cell(box Type::Struct(ref name)) => self.type_from_struct_call(name, field, &eval_args),
            ref x => panic!("{:?} is not implermented", x),
        };
        (eval_target, arg_expr, ret_type)
    }

    fn eval_statements<'b>(&self, statements: &'b Vec<ast::Statement>, env: &mut Environment<'b> ,ret_type: &Type) -> Vec<Statement> {
        statements.iter().map(|statement: &'b ast::Statement| {
            match statement {
                &ast::Statement::Let(ref name, ref expr) => {
                    let (expr, ty) = self.eval_expr(expr, env);
                    let nty: Type = match ty {
                        Type::Cell(ref t) => t.as_ref().clone(),
                        x => x,
                    };
                    env.insert(name, nty.clone());
                    Statement::Let {name: name.clone(), ty: nty, expr: Box::new(expr)}
                },
                &ast::Statement::LetMut(ref name, ref expr) => {
                    let (expr, ty) = self.eval_expr(expr, env);
                    let nty: Type = match ty {
                        Type::Cell(ref t) => t.as_ref().clone(),
                        x => x,
                    };
                    env.insert(name, Type::Cell(Box::new(nty.clone())));
                    Statement::Let {name: name.clone(), ty: Type::Cell(Box::new(nty)), expr: Box::new(expr)}
                }
                &ast::Statement::Return(ref expr) => {
                    let ty = self.eval_expr(expr, env);
                    if ty.1 == *ret_type {
                        Statement::Return {ty: ty.1, expr: Box::new(ty.0)}
                    } else {
                        panic!("")
                    }
                },
                &ast::Statement::Assignment(ref l_expr, ref expr) => {
                    let (n_expr, ty) = self.eval_expr(expr, env);
                    match self.eval_l_expr(l_expr, env) {
                        (ref t_expr, Type::Cell(ref c_ty)) => {
                            if **c_ty == ty {
                                Statement::Assignment{l_expr: Box::new(t_expr.clone()), expr: Box::new(n_expr.clone())}
                            } else {
                                panic!("the expression yields the wrong type")
                            }
                        },
                        _ => panic!("")
                    }
                },
                &ast::Statement::FunctionCall(ref expr, ref args) => {
                    let (t, a, r) = self.function_call(expr,args,env);
                    Statement::FunctionCall(Box::new(t), a)
                },
                &ast::Statement::MethodCall(ref expr, ref field, ref args) => {
                    let (eval_target, arg_expr, ret_type) = self.eval_method_call(expr, field, &args, env);
                    Statement::MethodCall(Box::new(eval_target.0), field.clone(), arg_expr)
                },
                x => unimplemented!("{:?}", x)
            }
        }).collect()
    }

    pub fn eval_l_expr(&self, l_expr: &ast::LExpr, env: &Environment) -> (LExpr, Type) {
        match l_expr {
            &ast::LExpr::Var(ref name) => {
                let ty = env.get(name).unwrap().clone();
                return (LExpr::Var(name.clone()), ty)
            },
            &ast::LExpr::MethodCall(ref l_expr, ref field, ref arg) => {
                let (t_l_expr, ty) = self.eval_l_expr(l_expr, env);
                match ty {
                    Type::Cell(box Type::Struct(ref name)) => {
                        (LExpr::MethodCall(Box::new(t_l_expr), field.clone(), Vec::new()), self.type_from_struct_call(name, field, &Vec::new()))
                    }
                    _ => panic!("not implemented {:?}", ty)
                }
            }
            _ => panic!("not implemented {:?}", l_expr)
        }
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