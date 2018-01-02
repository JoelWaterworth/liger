use type_checker::typed_ast::*;
use parsing::ast::BinaryOperator;
use std::collections::HashMap;
use std::fs::File;
use std::io::Write;

use compiler::code_generation::CodeGeneration;

mod code_generation;
mod llvm_node;

use compiler::llvm_node::LLVMNode;

pub fn compile(globals: Globals) {
    let code = CodeGeneration::new(globals);
    let mut compiler = Compiler::new();
    compiler.write_top_levels(code);
}

struct Compiler {
    file: File,
    indent: u8,
    var: u8,
}

impl Compiler {
    fn new() -> Self {
        Compiler {
            file: File::create("foo.ll").unwrap(),
            indent: 0,
            var: 0
        }
    }

    pub fn write_top_levels(&mut self, nodes: Vec<LLVMNode>) {
        for node in nodes {
            self.write_top_level(node)
        }
    }

    fn write_top_level(&mut self, node: LLVMNode) {
        match node {
            LLVMNode::Define {name, return_type, args, basic_blocks} => {
                self.file.write(b"define ");
                self.file.write(return_type.as_bytes());
                self.file.write(b" @");
                self.file.write(name.as_bytes());
                self.file.write(b"(");
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        self.file.write(b", ");
                    }
                    self.file.write(arg.as_bytes());
                }
                self.file.write(b")  {\n");
                for basic_block in basic_blocks {
                    self.write_basic_block(basic_block);
                }
                self.file.write(b"}");
                self.indent = 0;
            },
            LLVMNode::Type {name, types} => {
                self.file.write(name.as_bytes());
                self.file.write(b" = type { ");
                for (i, ty) in types.iter().enumerate() {
                    if i != 0 {
                        self.file.write(b", ");
                    }
                    self.file.write(ty.as_bytes());
                }
                self.file.write(b"}");
            }
            x => panic!("{:?}", x)
        }
        self.file.write(b"\n\n");
    }

    fn write_basic_block(&mut self, node: LLVMNode) {
        self.file.write(node.get_label().as_bytes());
        self.file.write(b":\n");
        self.indent = 4;
        for instruction in node.get_instruction() {
            self.write_instruction(instruction);
        }
    }

    fn write_indent(&mut self) {
        let mut x = String::from("");
        for i in 0..self.indent {
            x.push(' ');
        }
        self.file.write(x.into_bytes().as_ref());
    }

    fn write_instruction(&mut self, instruction: LLVMNode) {
        self.write_indent();
        match instruction {
            LLVMNode::Ret {ty, val} => {
                self.file.write(b"ret ");
                self.file.write(ty.as_bytes());
                self.file.write(b" ");
                self.file.write(val.as_bytes());
            },
            LLVMNode::BinaryOp { op, return_type, l, r, dst} => {
                let ops = match op {
                    BinaryOperator::Add => {
                        b"add"
                    },
                    BinaryOperator::And => {
                        b"and"
                    },
                    x => panic!("{:?}", x)
                };

                self.file.write(dst.as_bytes());
                self.file.write(b" = ");
                self.file.write(ops);
                self.file.write(b" ");
                self.file.write(return_type.as_bytes());
                self.file.write(b" ");
                self.file.write(l.as_bytes());
                self.file.write(b", ");
                self.file.write(r.as_bytes());
            },
            LLVMNode::Branch {branch} => {
                self.file.write(b"br label %");
                self.file.write(branch.as_bytes());
            },
            LLVMNode::Unreachable => {
                self.file.write(b"unreachable");
            },
            LLVMNode::ConditionalBranch {condition, t, f} => {
                self.file.write(b"br i1 ");
                self.file.write(condition.as_bytes());
                self.file.write(b", label %");
                self.file.write(t.as_bytes());
                self.file.write(b", label %");
                self.file.write(f.as_bytes());
            },
            LLVMNode::Alloca {ty, ptr} => {
                self.file.write(ptr.as_bytes());
                self.file.write(b" = alloca ");
                self.file.write(ty.as_bytes());
            },
            LLVMNode::Store {ty, ptr, source_val} => {
                self.file.write(b"store ");
                self.file.write(ty.as_bytes());
                self.file.write(b" ");
                self.file.write(source_val.as_bytes());
                self.file.write(b", ");
                self.file.write(ty.as_bytes());
                self.file.write(b"* ");
                self.file.write(ptr.as_bytes());
            },
            LLVMNode::Load {ty, dst, ptr} => {
                self.file.write(dst.as_bytes());
                self.file.write(b" = load ");
                self.file.write(ty.as_bytes());
                self.file.write(b", ");
                self.file.write(ty.as_bytes());
                self.file.write(b"* ");
                self.file.write(ptr.as_bytes());
            },
            LLVMNode::GetElementptr { ty, src, dst, offset} => {
                self.file.write(dst.as_bytes());
                self.file.write(b" = getelementptr ");
                self.file.write(ty.as_bytes());
                self.file.write(b", ");
                self.file.write(ty.as_bytes());
                self.file.write(b"* ");
                self.file.write(src.as_bytes());
                for o in offset {
                    self.file.write(b", i32 ");
                    self.file.write(format!("{}", o).as_bytes());
                }
            },
            LLVMNode::Call {dst, ret_ty, func, args} => {
                self.file.write(dst.as_bytes());
                self.file.write(b" = call ");
                self.file.write(ret_ty.as_bytes());
                self.file.write(b" ");
                self.file.write(func.as_bytes());
                self.file.write(b"(");
                for (i, (ty, arg)) in args.into_iter().enumerate() {
                    if i != 0 {
                        self.file.write(b", ");
                    }
                    self.file.write(ty.as_bytes());
                    self.file.write(b" ");
                    self.file.write(arg.as_bytes());
                };
                self.file.write(b")");
            },
            x => panic!("{:?}", x)
        };
        self.file.write(b"\n");
    }
}

//impl Compiler {
//    fn new() -> Self {
//        Compiler {
//            file: File::create("foo.ll").unwrap(),
//            indent: 0,
//            var: 0,
//            vars: HashMap::new(),
//        }
//    }
//    fn write_funciton(&mut self, func: Function) {
//        match func.cases {
//            Body::Cases(cases) => {
//                self.file.write(b"define ");
//                self.write_type(func.ret_ty.as_ref());
//                self.file.write(b"@");
//                self.file.write(func.name.as_bytes());
//                self.file.write(b"(");
//                for (i, arg) in func.args_ty.iter().enumerate() {
//                    if i > 0 {
//                        self.file.write(b", ");
//                    }
//                    self.write_type(arg);
//                }
//                self.file.write(b")");
//                self.file.write(b"  {");
//                self.file.write(b"\n");
//                self.indent += 2;
//                let case = cases[0].clone();
//                for arg in case.args {
//                    let var = self.new_var();
//                    match arg {
//                        Pattern::Binding{name, ty} => self.vars.insert(name, var),
//                        x => panic!("{:?}", x)
//                    };
//                };
//                for statement in case.statements {
//                    self.write_statement(statement);
//                }
//                self.indent = 0;
//                self.file.write(b"}");
//            },
//            x => panic!("{:?}", x)
//        };
//    }
//
//    fn new_var(&mut self) -> String {
//        let x = format!("{}{}", '%', self.var);
//        self.var += 1;
//        x
//    }
//
//    fn write_type(&mut self, ty: &Type) {
//        match ty {
//            &Type::Int => self.file.write(b"i64"),
//            &Type::Unit => self.file.write(b"void"),
//            x => panic!("{:?}", x),
//        };
//    }
//
//    fn write_indent(&mut self) {
//        let mut x = String::from("");
//        for i in 0..self.indent {
//            x.push(' ');
//        }
//        self.file.write(x.into_bytes().as_ref());
//    }
//
//    fn write_statement(&mut self, statement: Statement) {
//        match statement {
//            Statement::Return{ty, expr} => {
//                let ret = self.write_expression(expr.as_ref());
//                self.write_indent();
//                self.file.write(b"ret ");
//                self.write_type(&ty);
//                self.file.write(b" ");
//                self.file.write(ret.as_bytes());
//            }
//            x => panic!("")
//        }
//        self.file.write(b"\n");
//    }
//
//    fn write_expression(&mut self, expr: &Expr) -> String {
//        match expr {
//            &Expr::Var(ref name, _) => return self.vars.get(name).unwrap().clone(),
//            &Expr::BinaryExpr(ref op, ref l, ref r, ref ty) => {
//                let lir = self.write_expression(l.as_ref());
//                let rir = self.write_expression(r.as_ref());
//
//                let ret = self.new_var();
//                self.write_indent();
//                self.file.write(ret.as_bytes());
//                self.file.write(b" = ");
//                match op {
//                    &BinaryOperator::Add => {
//                        self.file.write(b"add ");
//                        self.write_type(ty);
//                        self.file.write(b" ");
//                        self.file.write(lir.as_bytes());
//                        self.file.write(b", ");
//                        self.file.write(rir.as_bytes());
//                    },
//                    x => panic!("")
//                };
//                self.file.write(b"\n");
//                return ret
//
//            },
//            x => panic!("")
//        }
//    }
//}