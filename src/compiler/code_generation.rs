use super::llvm_node::LLVMNode;
use type_checker::typed_ast::*;
use parsing::ast::{BinaryOperator, Lit};
use std::collections::{HashMap, HashSet};
use compiler::llvm_node::FunctionDeclare;

struct FunctionWriter<'a> {
    function_name: String,
    code_generation: &'a CodeGeneration,
    block_number: u64,
    variable_number: u64,
    vars: HashMap<String, (String, String)>,
    globals: Vec<LLVMNode>,
    global_number: u64,
    to_declare_functions: HashSet<FunctionDeclare>,
}

impl<'a> FunctionWriter<'a> {
    fn generate_struct_constructor(&mut self, stru: &Struct) -> LLVMNode {
        let name = stru.name.clone();
        let mut return_type = stru.name.clone();
        return_type.insert(0, '%');
        let mut args = Vec::new();
        for val in stru.args.values() {
            args.push(self.code_generation.generate_type(&val.0))
        };
        let mut basic_block = self.new_basic_block();

        let ret = self.alloca(&mut basic_block, return_type.clone());
        for (i, ty) in stru.args.values().enumerate() {
            let ptr = self.getelementptr(
                &mut basic_block,
                return_type.clone(),
                ret.clone(),
                vec![0.to_string(), i.to_string()],
                String::from("i32")
            );
            self.store(&mut basic_block, self.code_generation.generate_type(&ty.0), ptr, format!("%{}", i as u8));
        };

        let var = self.load(&mut basic_block, return_type.clone(), ret.clone());
        self.ret(&mut basic_block, return_type.clone(), var);

        LLVMNode::Define {
            name,
            return_type,
            args,
            basic_blocks: vec![basic_block],
        }
    }

    fn generate_enum_constructors(&mut self, enu: &Enum) -> Vec<LLVMNode> {
        let mut v = Vec::new();
        let enum_name = enu.name.clone();
        for &(ref member_name, ref args) in &enu.members {
            let mut basic_block = self.new_basic_block();
            let name = format!("{}.{}", enum_name, member_name);
            let return_type = format!("%{}.{}", enum_name, member_name);
            let mut a = Vec::new();
            for val in args {
                a.push(self.code_generation.generate_type(&val))
            };

            let ret = self.alloca(&mut basic_block, return_type.clone());
            for (i, val) in args.iter().enumerate() {
                let ptr = self.getelementptr(
                    &mut basic_block,
                    return_type.clone(),
                    ret.clone(),
                    vec![0.to_string(), i.to_string()],
                    String::from("i32")
                );
                self.store(&mut basic_block, self.code_generation.generate_type(&val), ptr, format!("%{}", i as u8));
            }
            let var = self.load(&mut basic_block, return_type.clone(), ret.clone());
            self.ret(&mut basic_block, return_type.clone(), var);

            v.push(LLVMNode::Define {
                name,
                return_type,
                args: a,
                basic_blocks: vec![basic_block],
            })
        }
        v
    }

    fn new_basic_block(&mut self) -> LLVMNode {
        let label = format!("block.{}", self.block_number);
        self.block_number += 1;
        LLVMNode::BasicBlock {
            label,
            instructions: Vec::new(),
            terminator: Box::new(LLVMNode::Ret {
                ty: String::from("void"),
                val: String::new()
            })
        }
    }

    fn new_global(&mut self) -> String {
        let global = format!("@{}.{}", self.function_name, self.global_number);
        self.global_number += 1;
        global
    }

    fn global_array(&mut self, ty: String, array: Vec<String>) -> String {
        let name = self.new_global();
        let mut data = String::from("[");
        for (i, a) in array.iter().enumerate() {
            if i > 0 {
                data.push_str(", ");
            }
            data.push_str(a);
        };
        data.push(']');

        self.globals.push(LLVMNode::GlobalConstant{
            name: name.clone(),
            ty,
            data,
        });
        name
    }

    fn new_variable(&mut self) -> String {
        let variable = format!("%var.{}", self.variable_number);
        self.variable_number += 1;
        variable
    }

    fn generate_statements(&mut self, root: &mut LLVMNode, statements: &Vec<Statement>) {
        for statement in statements {
            self.generate_statement(root, statement.clone())
        }
    }

    fn ret(&self, root: &mut LLVMNode, ty: String, val: String) {
        root.basic_block_terminate(
            LLVMNode::Ret {
                ty,
                val,
            }
        );
    }

    fn generate_statement(&mut self, root: &mut LLVMNode, statement: Statement) {
        match statement {
            Statement::Return {ref ty, ref expr} => {
                let gty = self.code_generation.generate_type(&ty);
                let ptr = self.alloca(root, gty.clone());
                self.generate_expression(root, expr.as_ref(), ptr.clone());
                let val = self.load(root, gty.clone(), ptr);
                self.ret(root, gty, val)
            },
            Statement::Let {ty, name, expr} => {
                let ptr = self.new_variable();
                let gty = self.code_generation.generate_type(&ty);
                self.vars.insert(name, (ptr.clone(), gty.clone()));
                root.push_to_basic_block(
                    LLVMNode::Alloca {
                        ty: gty.clone(),
                        ptr: ptr.clone(),
                    }
                );
                self.generate_expression(root, expr.as_ref(), ptr)
            },
            Statement::FunctionCall(ref target, ref args, ref ret) => {
                self.function_call(root, target.as_ref(), args, ret);
            }
            x => panic!("{:?}", x)
        }
    }

    fn load(&mut self, root: &mut LLVMNode, ty: String, ptr: String) -> String {
        let dst = self.new_variable();
        root.push_to_basic_block(
            LLVMNode::Load {
                ty,
                dst: dst.clone(),
                ptr,
            }
        );
        dst
    }

    pub fn generate_expression(&mut self, root: &mut LLVMNode, expr: &Expr, ptr: String) {
        match expr {
            &Expr::Var(ref name, _) => return {
                let (v_ptr, ty) = self.vars.get(name).unwrap().clone();
                let temp = self.load(root, ty.clone(), v_ptr);
                self.store(root, ty, ptr, temp);
            },
            &Expr::BinaryExpr(ref op, ref l, ref r, ref ty) => {
                let gty = self.code_generation.generate_type(ty);
                let l_ptr = self.alloca(root, gty.clone());
                let r_ptr = self.alloca(root, gty.clone());
                self.generate_expression(root, l.as_ref(), l_ptr.clone());
                self.generate_expression(root, r.as_ref(), r_ptr.clone());
                let lir = self.load(root, gty.clone(), l_ptr);
                let rir = self.load(root, gty.clone(), r_ptr);
                let dst = self.new_variable();
                root.push_to_basic_block(
                    LLVMNode::BinaryOp {
                        op: op.clone(),
                        return_type: gty.clone(),
                        l: lir,
                        r: rir,
                        dst: dst.clone(),
                    }
                );
                self.store(root, gty, ptr, dst);
            },
            &Expr::SliceInit(ref args, ref ty) => {
                let mut data = Vec::new();
                let gty = self.code_generation.generate_type(&ty);
                for e in args {
                    match e {
                        &Expr::Lit(Lit::Integral(n), _) => {
                            data.push(format!("i64 {}", n))
                        }
                        x => panic!("{:?} not yet supported for slice Initialization")
                    }
                };
                let const_name = self.global_array(gty.clone(), data);
                let alloc_bitcast = self.bitcast(root, gty.clone(), ptr.clone(), String::from("i8*"));
                let const_bitcast = self.bitcast(root, gty.clone(), const_name, String::from("i8*"));
                self.call(root, String::from("void"), String::from("@llvm.memcpy.p0i8.p0i8.i64"),
                          vec![
                              (String::from("i8*"), alloc_bitcast),
                              (String::from("i8*"), const_bitcast),
                              (String::from("i32"), (args.len() * 8).to_string()),
                              (String::from("i32"), (args.len() * 8).to_string())
                          ]);
            },
            &Expr::Lit(ref val, ref ty) => {
                match val {
                    &Lit::Integral(v) => {
                        let number = format!("{}", v);
                        self.store(root, self.code_generation.generate_type(&ty), ptr, number)
                    }
                }
            },
            &Expr::MethodCall(ref target, ref ty, ref field, ref args, ref ret_ty) => {
                let gret_ty = self.code_generation.generate_type(ret_ty);
                let src_ty = self.code_generation.generate_type(ty);
                let src_ptr = self.alloca(root, src_ty.clone());
                self.generate_expression(root, target.as_ref(), src_ptr.clone());
                match ty {
                    &Type::Struct(ref name) => {
                        let stru = &self.code_generation.structs.get(name).unwrap().0;
                        match stru.args.get(field) {
                            Some(&(ref tye, offset)) => {
                                let dst = self.new_variable();
                                let src = self.load(root, src_ty, src_ptr);
                                root.push_to_basic_block(
                                    LLVMNode::ExtractValue {
                                        ty: String::from(ty),
                                        src,
                                        dst: dst.clone(),
                                        offset: offset as u8,
                                    }
                                );
                                self.store(root, gret_ty,ptr, dst);
                            },
                            None => panic!("")
                        };
                    },
                    x => panic!("{:?}", x)
                }
            },
//            &Expr::App(ref target, ref args, ref ret) => {
//                self.function_call(root, target.as_ref(), args, ret)
//            },
            &Expr::Index(ref target, ref index, ref ty) => {
                let gty = self.code_generation.generate_type(&ty);
                let src_ty = self.code_generation.generate_type(&target.get_type());
                let src_ptr = self.alloca(root, src_ty.clone());
                self.generate_expression(root, target.as_ref(), src_ptr.clone());
                let index_ty = self.code_generation.generate_type(&index.get_type());
                let index_ptr = self.alloca(root, index_ty.clone());
                self.generate_expression(root, index.as_ref(), index_ptr.clone());

                let index = self.load(root, index_ty.clone(), index_ptr);
                let element_ptr = self.getelementptr(
                    root,
                    self.code_generation.generate_type(&target.get_type()),
                    src_ptr,
                    vec![0.to_string(), index],
                    index_ty
                );
                let ld = self.load(root, gty.clone(), element_ptr);
                self.store(root, gty.clone(), ptr, ld);
            },
            &Expr::StructInit(ref ty, ref args) => {
                let mut struct_name = String::from(ty);
                struct_name.remove(0);
                let stru = &self.code_generation.structs.get(&struct_name).unwrap().0;
                let mut func = struct_name;
                func.insert(0, '@');
                let mut fields = Vec::new();
                for &(ref field, ref arg) in args {
                    let a_ty = self.code_generation.generate_type(&arg.get_type());
                    let fa = self.alloca(root, a_ty.clone());
                    self.generate_expression(root, arg, fa.clone());
                    let t = &stru.args.get(field).unwrap().0;
                    fields.push(( String::from(t),self.load(root, a_ty, fa)));
                };
                let res = self.call(root, String::from(ty), func, fields);
                self.store(root, self.code_generation.generate_type(ty), ptr, res);
            },
            x => panic!("{:?}", x)
        }
    }

//    pub fn generate_expression(&mut self, root: &mut LLVMNode, expr: &Expr) -> String {
//        match expr {
//            &Expr::Var(ref name, _) => return {
//                let (ptr, ty) = self.vars.get(name).unwrap().clone();
////                self.load(root, ty, ptr)
//                ptr
//            },
//            &Expr::BinaryExpr(ref op, ref l, ref r, ref ty) => {
//                let lir = self.generate_expression(root, l.as_ref());
//                let rir = self.generate_expression(root, r.as_ref());
//                let dst = self.new_variable();
//                root.push_to_basic_block(
//                    LLVMNode::BinaryOp {
//                        op: op.clone(),
//                        return_type: self.code_generation.generate_type(ty),
//                        l: lir,
//                        r: rir,
//                        dst: dst.clone(),
//                    }
//                );
//                return dst
//            },
//            &Expr::StructInit(ref ty, ref args) => {
//                let mut struct_name = String::from(ty);
//                struct_name.remove(0);
//                let stru = &self.code_generation.structs.get(&struct_name).unwrap().0;
//                let mut func = struct_name;
//                func.insert(0, '@');
//                let mut fields = Vec::new();
//                for &(ref field, ref arg) in args {
//                    let a = self.generate_expression(root, arg);
//                    let t = &stru.args.get(field).unwrap().0;
//                    fields.push(( String::from(t),a));
//                };
//                self.call(root, String::from(ty), func, fields)
//            },
//            &Expr::Lit(ref val, ref ty) => {
//                match val {
//                    &Lit::Integral(v) => {
//                        format!("{}", v)
//                    }
//                }
//            },
//            &Expr::MethodCall(ref target, ref ty, ref field, ref args, ref ret_ty) => {
//                let src = self.generate_expression(root, target.as_ref());
//                match ty {
//                    &Type::Struct(ref name) => {
//                        let stru = &self.code_generation.structs.get(name).unwrap().0;
//                        match stru.args.get(field) {
//                            Some(&(ref tye, offset)) => {
//                                let dst = self.new_variable();
//                                root.push_to_basic_block(
//                                    LLVMNode::ExtractValue {
//                                        ty: String::from(ty),
//                                        src,
//                                        dst: dst.clone(),
//                                        offset: offset as u8,
//                                    }
//                                );
//                                return dst;
//                            },
//                            None => panic!("")
//                        };
//                    },
//                    x => panic!("{:?}", x)
//                }
//            },
//            &Expr::App(ref target, ref args, ref ret) => {
//                self.function_call(root, target.as_ref(), args, ret)
//            },
//            &Expr::Index(ref target, ref index, ref ty) => {
//                let src = self.generate_expression(root, target.as_ref());
//                let i = self.generate_expression(root, index.as_ref());
//
//                self.code_generation.generate_type(&target.get_type());
//                self.getelementptr(root, self.code_generation.generate_type(&target.get_type()), src, vec![0.to_string(), i])
//            },
//            x => panic!("{:?}", x)
//        }
//    }

    fn function_call(&mut self, root: &mut LLVMNode, target: &Expr, args: &Vec<Expr>, ret: &Type) -> String {
        let dst = self.new_variable();
        let func = match target {
            &Expr::Var(ref name, ref ty) => format!("@{}", name.clone()),
            x => panic!("{:?}", x)
        };
        let mut a = Vec::new();
        for arg in args {
            let gty = self.code_generation.generate_type(&arg.get_type());
            let ptr = self.alloca(root, gty.clone());
            self.generate_expression(root, arg, ptr.clone());
            let l = self.load(root, gty.clone(), ptr);
            a.push((gty, l));
        }
        root.push_to_basic_block(
            LLVMNode::Call {
                dst: dst.clone(),
                ret_ty: String::from(ret),
                func,
                args: a,
            }
        );
        dst
    }

    fn call(&mut self, basic_block: &mut LLVMNode, ret_ty: String, func: String, args: Vec<(String, String)>) -> String {
        let dst = self.new_variable();
        basic_block.push_to_basic_block(
            LLVMNode::Call {
                dst: dst.clone(),
                ret_ty,
                func,
                args,
            }
        );
        dst
    }
    fn bitcast(&mut self, basic_block: &mut LLVMNode, src_ty: String, val: String, end_ty: String) -> String {
        let dst = self.new_variable();
        basic_block.push_to_basic_block(
            LLVMNode::BitCast {
                dst: dst.clone(),
                src_ty,
                end_ty,
                val,
            }
        );
        dst
    }

    fn unreachable(&self, basic_block: &mut LLVMNode) {
        basic_block.basic_block_terminate(LLVMNode::Unreachable)
    }

    pub fn generate_match(&mut self, root: &mut LLVMNode, cases: Vec<Case>) -> Vec<LLVMNode> {
        let mut blocks = Vec::new();
        let mut terminate = self.new_basic_block();
        self.unreachable(&mut terminate);
        blocks.push(terminate.clone());
        let mut next_block = terminate.get_label();
        for case in cases.iter().rev() {
            let mut pattern_block = self.new_basic_block();
            let mut o_cond = None;
            for (i, arg_pattern) in case.args.iter().enumerate() {
                let r = self.generate_pattern(&mut pattern_block, arg_pattern.clone(), format!("%{}", i));
                match o_cond.clone() {
                    Some(l) => {
                        o_cond = Some(self.generate_and(&mut pattern_block,l,r))
                    },
                    None => {
                        o_cond = Some(r)
                    }
                }
            };

            let mut pattern_body = self.new_basic_block();
            self.generate_statements(&mut pattern_body, &case.statements);
            match o_cond {
                Some(cond) => self.conditional_branch(&mut pattern_block, cond, pattern_body.get_label(), next_block.clone()),
                None => {
                    self.branch(&mut pattern_block, pattern_body.get_label())
                }
            };
            next_block = pattern_block.get_label();
            blocks.push(pattern_block);
            blocks.push(pattern_body);
        };
        self.branch(root, next_block);
        return blocks;

    }

    fn getelementptr(&mut self, root: &mut LLVMNode, ty: String, src: String, offset: Vec<String>, offset_ty: String) -> String {
        let dst = self.new_variable();
        root.push_to_basic_block(
            LLVMNode::GetElementptr {
                ty,
                src,
                dst: dst.clone(),
                offset,
                offset_ty,
            }
        );
        dst
    }

    fn branch(&mut self, root: &mut LLVMNode, branch: String) -> String {
        let dst = self.new_variable();
        root.basic_block_terminate(
            LLVMNode::Branch {
                branch,
            }
        );
        dst
    }

    fn conditional_branch(&mut self, root: &mut LLVMNode, condition: String, t: String, f: String) -> String {
        let dst = self.new_variable();
        root.push_to_basic_block(
            LLVMNode::ConditionalBranch {
                condition,
                t,
                f,
            }
        );
        dst
    }

    fn generate_and(&mut self, root: &mut LLVMNode, l: String, r: String) -> String {
        let dst = self.new_variable();
        root.push_to_basic_block(
            LLVMNode::BinaryOp {
                op: BinaryOperator::And,
                return_type: String::from("i1"),
                l,
                r,
                dst: dst.clone(),
            }
        );
        dst
    }

    pub fn generate_pattern(&mut self, root: &mut LLVMNode, pattern: Pattern, ptr: String) -> String {
        match pattern {
            Pattern::Binding { name, ty} => {
                let var = self.alloca(root, String::from(&ty));
                let llvm_type = self.code_generation.generate_type(&ty);
                self.store(root,llvm_type.clone(), var.clone(), ptr);

                self.vars.insert(name, (var, llvm_type));
                return String::from("1")
            },
            x => panic!("{:?}", x)
        }
    }

    pub fn store(&mut self, root: &mut LLVMNode,
                 ty: String, ptr: String, source_val: String) {
        root.push_to_basic_block(
            LLVMNode::Store {
                ty,
                ptr,
                source_val,
            }
        );
    }

    pub fn alloca(&mut self, root: &mut LLVMNode, ty: String) -> String {
        let var = self.new_variable();
        root.push_to_basic_block(
            LLVMNode::Alloca {
                ptr: var.clone(),
                ty,
            }
        );
        var
    }

    pub fn generate_function(&mut self, func: Function) -> Vec<LLVMNode> {
        self.function_name = func.name.clone();
        let return_type = self.code_generation.generate_type(func.ret_ty.as_ref());
        let mut args = Vec::new();
        for arg in func.args_ty {
            args.push(self.code_generation.generate_type(&arg));
        }
        let mut basic_block = self.new_basic_block();
        let mut basic_blocks = match func.cases {
            Body::Cases(cases) => {
                self.generate_match(&mut basic_block, cases)
            },
            _ => panic!("")
        };
        basic_blocks.insert(0,basic_block);
        let mut globals = self.globals.clone();
        globals.push(
            LLVMNode::Define {
                name: func.name,
                return_type,
                args,
                basic_blocks,
            }
        );
        globals
    }

    pub fn declare(&mut self, func: Function) -> LLVMNode {
        let return_type = self.code_generation.generate_type(func.ret_ty.as_ref());
        let mut args = Vec::new();
        for arg in func.args_ty {
            args.push(self.code_generation.generate_type(&arg));
        }

        LLVMNode::Declare {
            name: func.name,
            return_type,
            args,
        }
    }

    pub fn init(code_generation: &'a CodeGeneration) -> Self {
        Self {
            code_generation,
            block_number: 0,
            variable_number: 0,
            vars: HashMap::new(),
            to_declare_functions: HashSet::new(),
            globals: Vec::new(),
            function_name: String::new(),
            global_number: 0
        }
    }
}

pub struct CodeGeneration {
    structs: HashMap<String, (Struct, u64, u64)>,
    enums: HashMap<String, (Enum, u64, u64)>,
}

impl CodeGeneration {
    pub fn new(globals: Globals) -> Vec<LLVMNode> {
        let mut llvm_nodes = Vec::new();
        let structs = globals.structs.iter().map(|(ref n, ref stru)| {
            let name = (*n).clone();
            let (size, align) = CodeGeneration::size_n_align_of(&Type::Struct(name.clone()), &globals.enums, &globals.structs);
            (name.clone(), ((*stru).clone(), size, align))
        }).collect();
        let enums = globals.enums.iter().map(|(ref n, ref enu)| {
            let name = (*n).clone();
            let (size, align) = CodeGeneration::size_n_align_of(&Type::Enum(name.clone()), &globals.enums, &globals.structs);
            (name.clone(), ((*enu).clone(), size, align))
        }).collect();
        let code_generation = CodeGeneration{structs, enums};
        let mut function_writer = FunctionWriter::init(&code_generation);
        for (_, &(ref stru, _ ,_ )) in code_generation.structs.iter() {
            llvm_nodes.append(&mut code_generation.generate_struct(stru));
        }
        for (_, &(ref enu, size, _ )) in code_generation.enums.iter() {
            llvm_nodes.append(&mut code_generation.generate_enum(enu, size));
        }

        for func in globals.functions {
            match func.cases.clone() {
                Body::Cases(_) => llvm_nodes.append(&mut function_writer.generate_function(func)),
                Body::Declare => llvm_nodes.push(function_writer.declare(func)),
                _ => {},
            }
        }
        llvm_nodes.push(LLVMNode::Declare {
            name: String::from("llvm.memcpy.p0i8.p0i8.i64"),
            return_type: String::from("void"),
            args: vec![
                String::from("i8*"),
                String::from("i8*"),
                String::from("i32"),
                String::from("i32"),
            ],
        });
        llvm_nodes
    }

    pub fn generate_type(&self, ty: &Type) -> String {
        match ty {
            &Type::Int => String::from("i64"),
            &Type::Unit => String::from("void"),
            &Type::Struct(ref n) => format!("%{}", n),
            &Type::Enum(ref n) => format!("%{}", n),
            &Type::Array(ref ty, ref len) => format!("[{} x {}]", len, self.generate_type(ty)),
            x => panic!("{:?}", x),
        }
    }
    pub fn size_n_align_of(ty: &Type, enums: &HashMap<String, Enum>, structs: &HashMap<String, Struct>) -> (u64, u64) {
        fn align(size: &mut u64, align: u64) {
            if (*size % align) != 0 {
                *size = *size + (align - (*size % align));
            }
        }
        match ty {
            &Type::Int => (8, 8),
            &Type::Struct(ref n) => {
                let mut max_align = 0;
                let mut size = 0;
                let stru = structs.get(n).unwrap();
                for (_, &(ref field, _)) in stru.args.iter() {
                    let (field_size, field_align) = CodeGeneration::size_n_align_of(field, enums, structs);
                    align(&mut size, field_align);
                    size += field_size;
                    if field_align > max_align {
                        max_align = field_align;
                    }
                };
                align(&mut size, max_align);
                (size, max_align)
            },
            &Type::Enum(ref n) => {
                let mut max_size = 0;
                let mut max_align = 0;
                let enu = enums.get(n).unwrap();
                for members in enu.members.iter() {
                    let mut size = 0;
                    for arg in members.1.iter() {
                        let (field_size, field_align) = CodeGeneration::size_n_align_of(arg, enums, structs);
                        align(&mut size, field_align);
                        size += field_size;
                        if field_align > max_align {
                            max_align = field_align;
                        }
                    };
                    if size > max_size {
                        max_size = size;
                    };
                };
                (max_size + 1, max_align)
            },
            x => panic!("{:?}", x),
        }
    }

    pub fn generate_enum(&self, enu: &Enum, size: u64) -> Vec<LLVMNode> {
        let mut name = enu.name.clone();
        let types = vec![String::from("i8"), format!("[{} x i8]", size - 1)];
        name.insert(0, '%');

        let mut constructors = Vec::new();

        let mut nodes = Vec::new();
        for &(ref member_name, ref args) in &enu.members {
            let n = format!("{}.{}", name, member_name.clone());
            let mut function_writer = FunctionWriter::init(self);
            constructors = function_writer.generate_enum_constructors(&enu);
            let mut v = Vec::new();
            for arg in args {
                v.push(self.generate_type(&arg));
            }
            v.insert(0, String::from("i8"));
            nodes.push(
                LLVMNode::Type {
                    name: n,
                    types: v,
                }
            )
        }

        nodes.insert(0, LLVMNode::Type {
            name,
            types
        });

        nodes.append(&mut constructors);
        nodes
    }

    pub fn generate_struct(&self, stru: &Struct) -> Vec<LLVMNode> {
        let mut types = Vec::new();
        for val in stru.args.values() {
            types.push(self.generate_type(&val.0))
        };
        let mut struct_name = stru.name.clone();
        struct_name.insert(0, '%');

        let mut function_writer = FunctionWriter::init(self);
        let constructor = function_writer.generate_struct_constructor(&stru);
        vec![
            LLVMNode::Type {
                name: struct_name,
                types
            },
            constructor
        ]
    }
}