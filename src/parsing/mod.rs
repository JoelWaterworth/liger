pub mod ast;
mod parser;
mod tokenizer;
use parsing::parser::parse_source_file;
use parsing::ast::SourceFile;
use parsing::tokenizer::{lexer};

pub fn parse(slice: &[u8], display_tokens: bool, display_ast: bool) -> SourceFile {
    let mut x = lexer(slice);
    if display_tokens {
        println!("{:?}", x);
    }
    x.reverse();
    let sf = parse_source_file(&mut x);
    if display_ast {
        println!("{:?}", sf);
    }
    sf
}