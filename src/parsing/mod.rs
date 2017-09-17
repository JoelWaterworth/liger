use nom::{digit, alpha, alphanumeric};
use nom;
use std::str;
use std::str::FromStr;
use llvm::*;
use std::collections::HashMap;

mod ast;
mod tokenizer;
mod parser;
use parsing::tokenizer::{lexer};
use parsing::parser::parse_statement;

pub fn parse(slice: &[u8]) {
    let mut x = lexer(slice);
    println!("{:?}", x.clone());
    x.reverse();
    println!("{:?}", parse_statement(&mut x));
}