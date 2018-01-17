use parsing::ast::{SourceFile, Lit};
use parsing::ast;
use std::collections::HashMap;
use type_checker::typed_ast::*;
use type_checker::enviroment::*;
pub mod typed_ast;
pub mod enviroment;

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
        structs.insert(*name, (Struct{
            var: Variable::init(Id::Name((*name).clone()),
                                 DeclaredIN::Global,
                                 Type::Struct((*name).clone())),
            args: f,
            methods: HashMap::new()
        }, method_type));
        methods.insert((*name).clone(), *funcs);
    };

    let function_types:FunctionTypes = functions.iter().map(|func|{
        create_func_ty(func, &shm, &enums)
    }).collect();

    let mut type_checker = TypeChecker{
        function_types,
        structs,
        env: Environment::new(),
        enums: typed_enums,
        functions: HashMap::new(),
    };
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
    env: Environment,
    functions: HashMap<String, Function>,
}

impl<'a> TypeChecker<'a> {
    pub fn parse_methods(&mut self, methods: HashMap<String, &Vec<ast::FunctionDefinition>>) {
        for (name, ms) in methods.iter() {
            for method in (*ms).iter() {
                let func_ty = self.structs.get(name).unwrap().1.get(&method.name).unwrap().clone();
                self.eval_func(method, &func_ty);
            }
        }
    }

    pub fn parse_functions(&mut self, funcs: Vec<&ast::FunctionDefinition>) {
        for func in funcs.iter() {
            let func_ty = self.function_types.get(&func.name).unwrap().clone();
            self.eval_func(*func, &func_ty);
        }
    }

    pub fn eval_func(&mut self, func: &ast::FunctionDefinition, func_ty: &FunctionType) {
        let args_ty = func_ty.0.clone();
        let cases = self.eval_func_cases(&func.cases, &args_ty, &func_ty.1);
        self.env.top_scope();
        let ty = Type::Function(box func_ty.clone());
        let var = Variable::init(Id::Name(func.name.clone()),
                               DeclaredIN::Global,
                               ty
        );
        self.env.insert(
            func.name.clone(),
            var.clone(),
        );
        self.functions.insert(func.name.clone(), Function{var, cases});
    }

    fn eval_func_cases(&mut self,
                           cases: &Vec<ast::FuncCase>,
                           arg_type: &Vec<Type>,
                           ret_type: &Type) -> Body {
        if cases.is_empty() {
            return Body::Declare
        }
        Body::Cases(
            cases.iter().map(|case: &ast::FuncCase| {
                let pattern: Vec<Pattern> =
                    case.matches.iter().zip(arg_type).map(|(ref m, ref ty)| {
                        self.env.new_scope();
                        match m {
                            &&ast::Match::WildCard(ref name) => {
                                let id = Id::Number(self.env.new_num());
                                self.env.insert(name.clone(), Variable::init(
                                    id,
                                    DeclaredIN::Argument,
                                    (*ty).clone())
                                );
                                Pattern::Binding { name: name.clone(), ty: (*ty).clone() }
                            },
                            &&ast::Match::Lit(ast::Lit::Integral(n)) => {
                                if **ty != Type::Int {
                                    panic!("")
                                }
                                Pattern::Constant {value: ConstVal{
                                    ty: (*ty).clone(),
                                    val: ast::Lit::Integral(n) } }
                            }
                            _ => unimplemented!()
                        }
                    }).collect();
                let statements = self.eval_statements(&case.body, ret_type);
                Case { args: pattern, statements }
            }).collect()
        )
    }

    fn eval_expr(&mut self, expr: &ast::Expr) -> (Expr, Type) {
        match expr {
            &ast::Expr::StructInit(ref ty, ref args) => {
                match ty {
                    &ast::Type::Named(ref x) => {
                        let s = self.structs.get(x).unwrap().0.clone();
                        let mut fields = Vec::new();
                        for &(ref arg_name, ref arg_expr) in args.into_iter() {
                            let arg_type = s.args.get(arg_name).unwrap();
                            let (typed_expr, expr_type) = self.eval_expr(arg_expr);
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
                let var = self.env.get(n).unwrap().clone();
                (Expr::Var(var.clone()), var.ty.as_ref().clone())
            },
            &ast::Expr::MethodCall(ref target, ref field, ref args) => {
                let ((eval_target, ty), arg_expr, ret_type) = self.eval_method_call(&target, field, &args);

                (Expr::MethodCall(Box::new(eval_target), ty, field.clone(), arg_expr, ret_type.clone()), ret_type)
            },
            &ast::Expr::App(ref target, ref args) => {
                let (t, a, r) = self.function_call(target, args);
                (Expr::App(Box::new(t), a, r.clone()), r)
            },
            &ast::Expr::BinaryExpr(ref op, ref left, ref right) => {
                let r = self.eval_operator(op, left, right);
                (Expr::BinaryExpr(op.clone(),
                                     Box::new(self.eval_expr(left).0),
                                     Box::new(self.eval_expr(right).0),
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
                    v.push(self.eval_expr(arg).0);
                };
                (Expr::EnumInit(enum_type.clone(), field.clone(), v), enum_type)
            },
            &ast::Expr::SliceInit(ref args) => {
                let mut ty = Type::Unit;
                let mut a = Vec::new();
                match args.iter().next() {
                    Some(e) => {
                        let (val, t) = self.eval_expr(e);
                        ty = t;
                    },
                    None => {}
                };

                for arg in args {
                    let (val, t) = self.eval_expr(arg);
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
                let (array, ty) = self.eval_expr(target);
                match ty {
                    Type::Array(box x, _) => {
                        let (index_exp, index_ty) = self.eval_expr(index);
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

    fn eval_operator(&mut self, op: &ast::BinaryOperator, left: &ast::Expr, right: &ast::Expr) -> Type {
        let l = self.eval_expr(left);
        let r = self.eval_expr(right);

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

    fn eval_method_call(&mut self, target: &ast::Expr, field: &String, args: &Vec<ast::Expr>) -> ((Expr, Type), Vec<Expr>, Type) {
        let eval_target = self.eval_expr(target);
        let mut eval_args: Vec<(Expr, Type)> = args.iter().map(|e| {
            self.eval_expr(e)
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

    fn eval_statements<'b>(&mut self, statements: &'b Vec<ast::Statement>,ret_type: &Type) -> Vec<Statement> {
        statements.iter().map(|statement: &'b ast::Statement| {
            match statement {
                &ast::Statement::Let(ref name, ref expr) => {
                    let (expr, ty) = self.eval_expr(expr);
                    let nty: Type = match ty {
                        Type::Cell(ref t) => t.as_ref().clone(),
                        x => x,
                    };
                    let var = Variable::init(
                        Id::Number(self.env.new_num()),
                        DeclaredIN::Local,
                        nty.clone()
                    );
                    self.env.insert(name.clone(), var.clone());
                    Statement::Let {var, expr: Box::new(expr)}
                },
                &ast::Statement::LetMut(ref name, ref expr) => {
                    let (expr, ty) = self.eval_expr(expr);
                    let nty: Type = match ty {
                        Type::Cell(ref t) => t.as_ref().clone(),
                        x => x,
                    };
                    let var = Variable::init(
                        Id::Number(self.env.new_num()),
                        DeclaredIN::Local,
                        Type::Cell(Box::new(nty.clone()))
                    );
                    self.env.insert(name.clone(), var.clone());
                    Statement::Let {var, expr: Box::new(expr)}
                }
                &ast::Statement::Return(ref expr) => {
                    let ty = self.eval_expr(expr);
                    if ty.1 == *ret_type {
                        Statement::Return {ty: ty.1, expr: Box::new(ty.0)}
                    } else {
                        panic!("return type should be {:?}, not {:?}", ret_type, ty.1)
                    }
                },
                &ast::Statement::Assignment(ref l_expr, ref expr) => {
                    let (n_expr, ty) = self.eval_expr(expr);
                    match self.eval_l_expr(l_expr) {
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
                    let (t, a, ty) = self.function_call(expr,args);
                    Statement::FunctionCall(Box::new(t), a, ty)
                },
                &ast::Statement::MethodCall(ref expr, ref field, ref args) => {
                    let (eval_target, arg_expr, _) = self.eval_method_call(expr, field, &args);
                    Statement::MethodCall(Box::new(eval_target.0), field.clone(), arg_expr)
                },
                x => unimplemented!("{:?}", x)
            }
        }).collect()
    }

    #[allow(unreachable_patterns)]
    pub fn eval_l_expr(&self, l_expr: &ast::LExpr) -> (LExpr, Type) {
        match l_expr {
            &ast::LExpr::Var(ref name) => {
                let var = self.env.get(name).unwrap().clone();
                return (LExpr::Var(var.clone()), var.ty.as_ref().clone())
            },
            &ast::LExpr::MethodCall(ref l_expr, ref field, ref _arg) => {
                let (t_l_expr, ty) = self.eval_l_expr(l_expr);
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

    pub fn function_call(&mut self, func: &ast::Expr, args: &Vec<ast::Expr>) -> (Expr, Vec<Expr>, Type) {
        match func {
            &ast::Expr::Var(ref name) => {
                let ty = self.function_types.get(name).cloned();
                match &ty {
                    &Some((ref args_ty, ref ret_type)) => {
                        let mut typed_args = Vec::new();
                        for (arg, ty) in args.iter().zip(args_ty.iter()) {
                            let eval_arg = self.eval_expr(arg);
                            if ty.clone() != eval_arg.1 {
                                panic!("expected {:?} not {:?}", ty, eval_arg)
                            }
                            typed_args.push(eval_arg.0);
                        };
                        return (
                            Expr::Var(Variable::init(
                                Id::Name(name.clone()),
                                DeclaredIN::Global,
                                Type::Function(box ty.clone().unwrap()))),
                            typed_args,
                            ret_type.clone())
                    },
                    &None => {
                        match self.env.get(name) {
                            Some(var) => {
                                let mut typed_args = Vec::new();
                                let (arg_ty, ret_ty) = match var.ty.clone() {
                                    box Type::Function(box func_ty) => func_ty,
                                    x => panic!("{:?}", x),
                                };
                                for (arg, ty) in args.iter().zip(arg_ty.iter()) {
                                    let eval_arg = self.eval_expr(arg);
                                    if ty.clone() != eval_arg.1 {
                                        panic!("expected {:?} not {:?}", ty, eval_arg)
                                    }
                                    typed_args.push(eval_arg.0);
                                };
                                return ( Expr::Var(var.clone()), typed_args, ret_ty.clone())
                            },
                            _ => panic!("")
                        }
                    }
                }
            },
            _ => panic!("")
        }
    }

    pub fn global(self, links: Vec<String>) -> Globals {
        let structs:HashMap<String, Struct> = self.structs.into_iter().map(|(name, (s, _))| {
            (name.clone(), s.clone())
        }).collect();
        let functions:HashMap<String, Function> = self.functions.into_iter().map(|(name, s)| {
            (name, s)
        }).collect();
        let enums:HashMap<String, Enum> = self.enums.into_iter().map(|(name, e)| {
            (name.clone(), e)
        }).collect();
        Globals { functions, structs, enums}
    }
}