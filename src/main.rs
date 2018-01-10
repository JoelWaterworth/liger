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
use std::env;

mod parsing;
mod interpreter;
mod type_checker;
mod compiler;

use parsing::*;
use type_checker::type_check_ast;
use interpreter::interpret_source_file;
use compiler::compile;

fn main() {
    let mut args = env::args();
    args.next();
    let command = args.next();
    let display = args.next();
    let x = if display == Some(String::from("Debug")) {
        true
    } else {
        false
    };
    let path = Path::new("example/main.lig");
    let mut file = File::open(&path).unwrap();
    let mut s = String::new();
    file.read_to_string(&mut s).ok().unwrap();

    let sf = parse(s, x , x);
    let ty = type_check_ast(&sf);
    if x {
        println!("\n {:?}", ty);
    }

    if command == Some(String::from("compile")) {
        println!("compiling source code");
        compile(ty);
    } else if command == Some(String::from("interpret")) {
        println!("interpreting source code");
        interpret_source_file(ty);
    }
}
