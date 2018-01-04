use type_checker::typed_ast::*;
use parsing::ast::BinaryOperator;
use std::fs::File;
use std::io::Write;
use std::io;

use compiler::code_generation::CodeGeneration;

mod code_generation;
mod llvm_node;

use compiler::llvm_node::LLVMNode;

pub fn compile(globals: Globals) {
    let code = CodeGeneration::new(globals);
    let mut compiler = Compiler::new();
    match compiler.write_top_levels(code) {
        Ok(()) => {},
        Err(x) => println!("{}", x)
    };
}

struct Compiler {
    file: File,
    indent: u8,
}

impl Compiler {
    fn new() -> Self {
        Compiler {
            file: File::create("foo.ll").unwrap(),
            indent: 0,
        }
    }

    pub fn write_top_levels(&mut self, nodes: Vec<LLVMNode>) -> io::Result<()> {
        for node in nodes {
            self.write_top_level(node)?
        }
        Ok(())
    }

    fn write_top_level(&mut self, node: LLVMNode) -> io::Result<()> {
        match node {
            LLVMNode::Define {name, return_type, args, basic_blocks} => {
                self.file.write(b"define ")?;
                self.file.write(return_type.as_bytes())?;
                self.file.write(b" @")?;
                self.file.write(name.as_bytes())?;
                self.file.write(b"(")?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        self.file.write(b", ")?;
                    }
                    self.file.write(arg.as_bytes())?;
                }
                self.file.write(b")  {\n")?;
                for basic_block in basic_blocks {
                    self.write_basic_block(basic_block)?;
                }
                self.file.write(b"}")?;
                self.indent = 0;
            },
            LLVMNode::Type {name, types} => {
                self.file.write(name.as_bytes())?;
                self.file.write(b" = type { ")?;
                for (i, ty) in types.iter().enumerate() {
                    if i != 0 {
                        self.file.write(b", ")?;
                    }
                    self.file.write(ty.as_bytes())?;
                }
                self.file.write(b"}")?;
            }
            x => panic!("{:?}", x)
        }
        self.file.write(b"\n\n")?;
        Ok(())
    }

    fn write_basic_block(&mut self, node: LLVMNode) -> io::Result<()> {
        self.file.write(node.get_label().as_bytes())?;
        self.file.write(b":\n")?;
        self.indent = 4;
        for instruction in node.get_instruction() {
            self.write_instruction(instruction)?;
        }
        Ok(())
    }

    fn write_indent(&mut self) -> io::Result<()> {
        let mut x = String::from("");
        for _ in 0..self.indent {
            x.push(' ');
        }
        self.file.write(x.into_bytes().as_ref())?;
        Ok(())
    }

    fn write_instruction(&mut self, instruction: LLVMNode) -> io::Result<()> {
        self.write_indent()?;
        match instruction {
            LLVMNode::Ret {ty, val} => {
                self.file.write(b"ret ")?;
                self.file.write(ty.as_bytes())?;
                self.file.write(b" ")?;
                self.file.write(val.as_bytes())?;
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

                self.file.write(dst.as_bytes())?;
                self.file.write(b" = ")?;
                self.file.write(ops)?;
                self.file.write(b" ")?;
                self.file.write(return_type.as_bytes())?;
                self.file.write(b" ")?;
                self.file.write(l.as_bytes())?;
                self.file.write(b", ")?;
                self.file.write(r.as_bytes())?;
            },
            LLVMNode::Branch {branch} => {
                self.file.write(b"br label %")?;
                self.file.write(branch.as_bytes())?;
            },
            LLVMNode::Unreachable => {
                self.file.write(b"unreachable")?;
            },
            LLVMNode::ConditionalBranch {condition, t, f} => {
                self.file.write(b"br i1 ")?;
                self.file.write(condition.as_bytes())?;
                self.file.write(b", label %")?;
                self.file.write(t.as_bytes())?;
                self.file.write(b", label %")?;
                self.file.write(f.as_bytes())?;
            },
            LLVMNode::Alloca {ty, ptr} => {
                self.file.write(ptr.as_bytes())?;
                self.file.write(b" = alloca ")?;
                self.file.write(ty.as_bytes())?;
            },
            LLVMNode::Store {ty, ptr, source_val} => {
                self.file.write(b"store ")?;
                self.file.write(ty.as_bytes())?;
                self.file.write(b" ")?;
                self.file.write(source_val.as_bytes())?;
                self.file.write(b", ")?;
                self.file.write(ty.as_bytes())?;
                self.file.write(b"* ")?;
                self.file.write(ptr.as_bytes())?;
            },
            LLVMNode::Load {ty, dst, ptr} => {
                self.file.write(dst.as_bytes())?;
                self.file.write(b" = load ")?;
                self.file.write(ty.as_bytes())?;
                self.file.write(b", ")?;
                self.file.write(ty.as_bytes())?;
                self.file.write(b"* ")?;
                self.file.write(ptr.as_bytes())?;
            },
            LLVMNode::GetElementptr { ty, src, dst, offset} => {
                self.file.write(dst.as_bytes())?;
                self.file.write(b" = getelementptr ")?;
                self.file.write(ty.as_bytes())?;
                self.file.write(b", ")?;
                self.file.write(ty.as_bytes())?;
                self.file.write(b"* ")?;
                self.file.write(src.as_bytes())?;
                for o in offset {
                    self.file.write(b", i32 ")?;
                    self.file.write(format!("{}", o).as_bytes())?;
                }
            },
            LLVMNode::Call {dst, ret_ty, func, args} => {
                self.file.write(dst.as_bytes())?;
                self.file.write(b" = call ")?;
                self.file.write(ret_ty.as_bytes())?;
                self.file.write(b" ")?;
                self.file.write(func.as_bytes())?;
                self.file.write(b"(")?;
                for (i, (ty, arg)) in args.into_iter().enumerate() {
                    if i != 0 {
                        self.file.write(b", ")?;
                    }
                    self.file.write(ty.as_bytes())?;
                    self.file.write(b" ")?;
                    self.file.write(arg.as_bytes())?;
                };
                self.file.write(b")")?;
            },
            x => panic!("{:?}", x)
        };
        self.file.write(b"\n")?;
        Ok(())
    }
}