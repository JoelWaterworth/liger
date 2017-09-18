#[macro_use]
extern crate nom;
extern crate llvm;
extern crate libc;
/*
    TODO: create llvm backend
    TODO: create parser
*/
use std::fs::File;
use std::io::Read;
use std::path::Path;

mod parsing;
use parsing::*;

fn main() {
    let path = Path::new("example/main.li");
    let mut file = File::open(&path).unwrap();
    let mut s = String::new();
    file.read_to_string(&mut s);

    parse(s.as_ref());
}
