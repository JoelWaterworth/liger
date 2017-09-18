use llvm::*;
use nom::{digit, alpha, alphanumeric};
use nom;
use std::collections::HashMap;
use std::str::FromStr;
use std::str;

mod ast;
mod parser;
mod tokenizer;
use parsing::parser::parse_source_file;
use parsing::tokenizer::{lexer};

pub fn parse(slice: &[u8]) {
    let mut x = lexer(slice);
    println!("{:?}", x.clone());
    x.reverse();
    println!("{:?}", parse_source_file(&mut x));
}