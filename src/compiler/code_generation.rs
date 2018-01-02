use super::llvm_node::LLVMNode;
use type_checker::typed_ast::*;
use parsing::ast::BinaryOperator;
use std::collections::HashMap;

struct FunctionWriter<'a> {
    code_generation: &'a CodeGeneration,
    block_number: u8,
    variable_number: u8,
    vars: HashMap<String, (String, String)>,
}

impl<'a> FunctionWriter<'a> {
    fn new_basic_block(&mut self) -> LLVMNode {
        let label = format!("block.{}", self.block_number);
        self.block_number += 1;
        LLVMNode::BasicBlock {label, instructions: Vec::new()}
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

    fn generate_statement(&mut self, root: &mut LLVMNode, statement: Statement) {
        match statement {
            Statement::Return {ty, expr} => {
                let gty = self.code_generation.generate_type(&ty);
                let val = self.generate_expression(root, expr.as_ref());
                root.push_to_basic_block(
                    LLVMNode::Ret {
                        ty: gty,
                        val,
                    }
                );
            },
            Statement::Let {ty, name, expr} => {
                let val = self.generate_expression(root, expr.as_ref());
                let ptr = self.new_variable();
                let gty = self.code_generation.generate_type(&ty);
                root.push_to_basic_block(
                    LLVMNode::Alloca {
                        ty: gty.clone(),
                        ptr: ptr.clone(),
                    }
                );
                self.vars.insert(name, (ptr.clone(), gty.clone()));
                root.push_to_basic_block(
                    LLVMNode::Store {
                        ty: gty,
                        ptr,
                        source_val: val,
                    }
                );
            }
            x => panic!("{:?}", x)
        }
    }

    pub fn generate_expression(&mut self, root: &mut LLVMNode , expr: &Expr) -> String {
        match expr {
            &Expr::Var(ref name, _) => return {
                let (ptr, ty) = self.vars.get(name).unwrap().clone();
                let dst = self.new_variable();
                root.push_to_basic_block(
                    LLVMNode::Load {
                        ty,
                        dst: dst.clone(),
                        ptr,
                    }
                );
                dst
            },
            &Expr::BinaryExpr(ref op, ref l, ref r, ref ty) => {
                let lir = self.generate_expression(root, l.as_ref());
                let rir = self.generate_expression(root, r.as_ref());
                let dst = self.new_variable();
                root.push_to_basic_block(
                    LLVMNode::BinaryOp {
                        op: op.clone(),
                        return_type: self.code_generation.generate_type(ty),
                        l: lir,
                        r: rir,
                        dst: dst.clone(),
                    }
                );
                return dst
            },
            x => panic!("")
        }
    }

    fn unreachable(&self, basic_block: &mut LLVMNode) {
        basic_block.push_to_basic_block(LLVMNode::Unreachable)
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

    fn branch(&mut self, root: &mut LLVMNode, branch: String) -> String {
        let dst = self.new_variable();
        root.push_to_basic_block(
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
                let var = self.alloca(root, &ty);
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

    pub fn alloca(&mut self, root: &mut LLVMNode, ty: &Type) -> String {
        let var = self.new_variable();
        root.push_to_basic_block(
            LLVMNode::Alloca {
                ptr: var.clone(),
                ty: self.code_generation.generate_type(ty),
            }
        );
        var
    }

    pub fn generate_function(&mut self, func: Function) -> LLVMNode {
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

        LLVMNode::Define {
            name: func.name,
            return_type,
            args,
            basic_blocks,
        }
    }

    pub fn init(code_generation: &'a CodeGeneration) -> Self {
        Self {code_generation, block_number: 0, variable_number: 0, vars: HashMap::new()}
    }
}

pub struct CodeGeneration {
}

impl CodeGeneration {
    pub fn new(globals: Globals) -> Vec<LLVMNode> {
        let mut llvm_nodes = Vec::new();
        let mut code_generation = CodeGeneration{};
        let mut function_writer = FunctionWriter::init(&code_generation);
        for stru in globals.structs {
            llvm_nodes.append(&mut code_generation.generate_struct(stru));
        }
        for func in globals.functions {
            match func.cases.clone() {
                Body::Cases(_) => llvm_nodes.push(function_writer.generate_function(func)),
                _ => {},
            }
        }
        llvm_nodes
    }

    pub fn generate_type(&self, ty: &Type) -> String {
        match ty {
            &Type::Int => String::from("i64"),
            &Type::Unit => String::from("void"),
            x => panic!("{:?}", x),
        }
    }

    pub fn generate_struct(&self, stru: Struct) -> Vec<LLVMNode> {
        let mut types = Vec::new();
        for val in stru.args.values() {
            types.push(self.generate_type(val))
        };
        let mut name = stru.name.clone();
        name.insert(0, '%');
        vec![
            LLVMNode::Type {
                name,
                types
            }
        ]
    }
}