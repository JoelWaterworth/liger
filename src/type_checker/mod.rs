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
    let mut enums = HashMap::new();
    let mut links = Vec::new();
    for decl in &sf.decls {
        match decl {
            &ast::Declaration::FunctionDef(ref x) => {functions.push(x);},
            &ast::Declaration::StructDef(ref name, ref fields, ref methods) => {shm.insert(name, (fields, methods));},
            &ast::Declaration::Enum(ref name, ref fields) => {
                enums.insert(name, fields);
            },
            &ast::Declaration::Link(ref n) => links.push(n.clone()),
            x => panic!("{:?}", x)
        };
    };
    let mut typed_enums = HashMap::new();
    for (name, fields) in enums.iter() {
        let mut members = Vec::new();
        for field in *fields {
            let mut args = Vec::new();
            for arg in field.body.iter() {
                args.push(Type::from_with_maps(&arg, &shm, &enums));
            }
            members.push((field.name.clone(), args))
        }
        typed_enums.insert(name.clone(), Enum{name: (*name).clone(), members});
    }
    let mut structs = HashMap::new();
    let struct_iter = shm.iter();
    let mut methods = HashMap::new();
    for (name, &(ref fields, ref funcs)) in struct_iter.clone() {
        let mut f = HashMap::new();
        for (i, field) in fields.iter().enumerate() {
            f.insert(field.name.clone(), (Type::from_with_maps(&field.ty, &shm, &enums), i as i32));
        };
        let struct_type = Type::Struct((*name).clone());
        let method_type: FunctionTypes = funcs.iter().map(|func|{
            create_method_ty(&struct_type, func,&shm, &enums)
        }).collect();
        structs.insert(*name, (Struct{name: (*name).clone(), args: f, methods: HashMap::new()}, method_type));
        methods.insert((*name).clone(), *funcs);
    };

    let function_types:FunctionTypes = functions.iter().map(|func|{
        create_func_ty(func, &shm, &enums)
    }).collect();
    let mut typed_functions = HashMap::new();
    typed_functions.insert(String::from("print"),
                           Function{
        name: String::from("print"),
        args_ty: vec![Type::Int],
        ret_ty: Box::new(Type::Unit),
        cases: Body::BuiltInFunc(String::from("print"))
    });

    let mut type_checker = TypeChecker{function_types, structs, typed_functions, enums: typed_enums};
    type_checker.parse_methods(methods);
    type_checker.parse_functions(functions);
    type_checker.global(links)
}

impl Type {
    pub fn from_with_maps<S,E>(ty: &ast::Type, struct_map: &HashMap<&String, S>, enum_map: &HashMap<&String, E>) -> Type {
        match ty {
            &ast::Type::Named(ref x) => match x.as_str() {
                "i64" => Type::Int,
                "IO" => Type::Unit,
                _ => {
                    match struct_map.get(x) {
                        Some(_) => Type::Struct(x.clone()),
                        None => match enum_map.get(x) {
                            Some(_) => Type::Enum(x.clone()),
                            None => panic!("{:?} is not expected", x)
                        }
                    }
                }
            },
            &ast::Type::Cell(ref x) => Type::Cell(Box::new(Type::from_with_maps(x.as_ref(), struct_map, enum_map))),
            _ => panic!("{:?}", ty)
        }
    }

    pub fn method_type<S,E>(ty: &ast::Type, self_type: &Type, struct_map: &HashMap<&String, S>, enum_map: &HashMap<&String, E>) -> Type {
        match ty {
            &ast::Type::SelfT => self_type.clone(),
            _ => Type::from_with_maps(ty, struct_map, enum_map)
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

fn create_func_ty<'a, T, E>(func: &'a ast::FunctionDefinition, map: &HashMap<&String, T>, enum_maps: &HashMap<&String, E>) -> (&'a String, FunctionType) {
    let name = &func.name;
    let ret = Type::from_with_maps( &func.ret_type, map, enum_maps);
    let arg_types:Vec<Type> = func.arg_types.iter().map(|arg| {
        Type::from_with_maps(arg, map, enum_maps)
    }).collect();
    (name, (arg_types, ret))
}

fn create_method_ty<'a, T, E>(stru: &Type, func: &'a ast::FunctionDefinition, map: &HashMap<&String, T>, enum_maps: &HashMap<&String, E>) -> (&'a String, FunctionType) {
    let name = &func.name;
    let ret = Type::method_type( &func.ret_type, stru, map, enum_maps);
    let arg_types:Vec<Type> = func.arg_types.iter().map(|arg| {
        Type::method_type(arg,stru,map, enum_maps)
    }).collect();
    (name, (arg_types, ret))
}

struct TypeChecker<'a> {
    function_types: FunctionTypes<'a>,
    structs: HashMap<&'a String, (Struct, FunctionTypes<'a>)>,
    enums: HashMap<&'a String, Enum>,
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
        if cases.is_empty() {
            return Body::Declare
        }
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
                            if arg_type.0 != expr_type {
                                panic!("{:?} should be {:?}, not {:?}", arg_name, arg_type, expr_type)
                            };
                            fields.push((arg_name.clone(), typed_expr));
                        }
                        (Expr::StructInit(Type::Struct(x.clone()), fields), Type::Struct(x.clone()))
                    },
                    &ast::Type::SelfT => panic!("SelfT unimplemented"),
                    &ast::Type::Cell(_) => panic!("Cell unimplemented"),
                }
            },
            &ast::Expr::Lit(Lit::Integral(n)) => (Expr::Lit(Lit::Integral(n.clone()), Type::Int), Type::Int),
            &ast::Expr::Var(ref n) => {
                let ty = env.get(n).unwrap().clone();
                (Expr::Var(n.clone(), ty.clone()), ty)
            },
            &ast::Expr::MethodCall(ref target, ref field, ref args) => {
                let ((eval_target, ty), arg_expr, ret_type) = self.eval_method_call(&target, field, &args, env);

                (Expr::MethodCall(Box::new(eval_target), ty, field.clone(), arg_expr, ret_type.clone()), ret_type)
            },
            &ast::Expr::App(ref target, ref args) => {
                let (t, a, r) = self.function_call(target, args, env);
                (Expr::App(Box::new(t), a, r.clone()), r)
            },
            &ast::Expr::BinaryExpr(ref op, ref left, ref right) => {
                let r = self.eval_operator(op, left, right, env);
                (Expr::BinaryExpr(op.clone(),
                                     Box::new(self.eval_expr(left, env).0),
                                     Box::new(self.eval_expr(right, env).0),
                                     r.clone()),
                    r
                )
            },
            &ast::Expr::EnumInit(ref ty, ref field, ref args) => {
                let enum_type = match ty {
                    &ast::Type::Named(ref name) => Type::Enum(name.clone()),
                    x => panic!("{:?}", x),
                };
                let mut v = Vec::new();
                for arg in args {
                    v.push(self.eval_expr(arg, env ).0);
                };
                (Expr::EnumInit(enum_type.clone(), field.clone(), v), enum_type)
            },
            &ast::Expr::SliceInit(ref args) => {
                let mut ty = Type::Unit;
                let mut a = Vec::new();
                match args.iter().next() {
                    Some(e) => {
                        let (val, t) = self.eval_expr(e, env);
                        ty = t;
                    },
                    None => {}
                };

                for arg in args {
                    let (val, t) = self.eval_expr(arg, env);
                    if t != ty {
                        panic!("inconsitent types in slice")
                    }
                    a.push(val);
                }
                println!("{:?}", a);
                let slice_type = Type::Array(box ty, args.len());
                (Expr::SliceInit(a, slice_type.clone()), slice_type)
            },
            &ast::Expr::Index(ref target, ref index) => {
                let (array, ty) = self.eval_expr(target, env);
                match ty {
                    Type::Array(box x, _) => {
                        let (index_exp, index_ty) = self.eval_expr(index, env);
                        if index_ty != Type::Int {
                            panic!("you should index with a integer type, not {:?}", index_ty)
                        }

                        (Expr::Index(box array, box index_exp, x.clone()), x)
                    },
                    x => panic!("Index on none Slice {:?}", x)
                }
            },
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
            Some(x) => return x.0.clone(),//field
            None => {
                match s.1.get(field) {
                    Some(&(ref arg_ty, ref ret_ty)) => {
                        arg_ty.iter().zip(args.iter()).for_each(|(x,&(ref _expr, ref ty))|{
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
                        panic!("return type should be {:?}, not {:?}", ret_type, ty.1)
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
                    let (t, a, ty) = self.function_call(expr,args,env);
                    Statement::FunctionCall(Box::new(t), a, ty)
                },
                &ast::Statement::MethodCall(ref expr, ref field, ref args) => {
                    let (eval_target, arg_expr, _) = self.eval_method_call(expr, field, &args, env);
                    Statement::MethodCall(Box::new(eval_target.0), field.clone(), arg_expr)
                },
                x => unimplemented!("{:?}", x)
            }
        }).collect()
    }

    #[allow(unreachable_patterns)]
    pub fn eval_l_expr(&self, l_expr: &ast::LExpr, env: &Environment) -> (LExpr, Type) {
        match l_expr {
            &ast::LExpr::Var(ref name) => {
                let ty = env.get(name).unwrap().clone();
                return (LExpr::Var(name.clone()), ty)
            },
            &ast::LExpr::MethodCall(ref l_expr, ref field, ref _arg) => {
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
                        return ( Expr::Var(name.clone(), ret_type.clone()), typed_args, ret_type.clone())
                    },
                    None => {
                        match self.typed_functions.get(name) {
                            Some(&Function{ref name, ref args_ty, ref ret_ty, cases: _}) => {
                                let mut typed_args = Vec::new();
                                for (arg, ty) in args.iter().zip(args_ty.iter()) {
                                    let eval_arg = self.eval_expr(arg, env);
                                    if ty.clone() != eval_arg.1 {
                                        panic!("expected {:?} not {:?}", ty, eval_arg)
                                    }
                                    typed_args.push(eval_arg.0);
                                };
                                return ( Expr::Var(name.clone(), *ret_ty.clone()), typed_args, *ret_ty.clone())
                            },
                            _ => panic!("")
                        }
                    }
                }
            },
            _ => panic!("")
        }
    }

    pub fn global(&self, links: Vec<String>) -> Globals {
        let structs:HashMap<String, Struct> = self.structs.iter().map(|(name, &(ref s, _))| {
            ((*name).clone(), s.clone())
        }).collect();
        let functions:HashSet<Function> = self.typed_functions.iter().map(|(_, ref s)| {
            (*s).clone()
        }).collect();
        let enums:HashMap<String, Enum> = self.enums.iter().map(|(name, e)| {
            ((*name).clone(), e.clone())
        }).collect();
        Globals { functions, structs, enums, links}
    }
}