#![feature(box_patterns)]

#[macro_use]
extern crate nom;
extern crate libc;
/*
    TODO: create llvm backend
    TODO: create parser
*/
use std::fs::File;
use std::io::Read;
use std::path::Path;

mod parsing;
mod interpreter;
mod type_checker;
mod compiler;

use parsing::*;
use type_checker::type_check_ast;
use interpreter::interpret_source_file;
use compiler::compile;

fn main() {
    let path = Path::new("example/main.lig");
    let mut file = File::open(&path).unwrap();
    let mut s = String::new();
    file.read_to_string(&mut s).ok().unwrap();

    let sf = parse(s.as_ref());
    let ty = type_check_ast(&sf);
    println!("\n {:?}", ty);
    compile(ty);

    //interpret_source_file(ty);
}
