pub mod ast;
mod parser;
mod tokenizer;
use parsing::parser::parse_source_file;
use parsing::ast::SourceFile;
use parsing::tokenizer::{lexer};

pub fn parse(slice: &[u8]) -> SourceFile {
    let mut x = lexer(slice);
    println!("{:?}", x);
    x.reverse();
    let sf = parse_source_file(&mut x);
    println!("{:?}", sf);
    sf
}