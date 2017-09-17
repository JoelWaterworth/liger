use nom::{digit, alpha, alphanumeric};
use nom;
use std::str;
use std::str::FromStr;
use llvm::*;
use std::collections::HashMap;

mod ast;
mod tokenizer;
use parsing::tokenizer::{lexer, Token, word, tokenizer, bracket};

#[derive(Clone, Debug)]
pub struct Func {
    name: String,
    arg_types: Vec<String>,
    ret_type: String,
    body: Entry
}

#[derive(Clone, Debug)]
pub struct Entry {
    arg_names: Vec<String>,
    body: Vec<Token>,
}

pub fn parse(slice: &[u8]) {
    let x = tokenizer(slice);
    println!("{:?}", x);
}

named!(pub entry<(String, Entry)>, do_parse!(
    name: word >>
    arg_names: many0!(word) >>
    ws!(tag!("=")) >>
    body: tokenizer >>
    ((name,Entry{arg_names, body}))
));

named!(function_type<(String, Vec<String>, String)>, do_parse!(
    name: word >>
    ws!(tag!(":")) >>
    types: delimited!(
            ws!(tag!("(")),
            many0!( do_parse!(
                    ty: word >>
                    ws!(alt!( tag!(",") | tag!("") )) >>
                    (ty)
                )),
            ws!(tag!(")"))
        ) >>
    ws!(tag!("->")) >>
    ret: word >>
    (name, types, ret)
));