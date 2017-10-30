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
use parsing::*;
use parsing::ast::SourceFile;
use interpreter::interpret_source_file;
use type_checker::type_check_source_file;

fn main() {
    let path = Path::new("example/main.lig");
    let mut file = File::open(&path).unwrap();
    let mut s = String::new();
    file.read_to_string(&mut s);

    let sf = parse(s.as_ref());
    type_check_source_file(&sf);
    interpret_source_file(sf)
}
