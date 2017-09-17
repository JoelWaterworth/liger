use nom::{digit, alpha, alphanumeric};
use nom;
use std::str;
use std::str::FromStr;
use llvm::*;
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub enum Token {
    Let,
    Identifier(String),
    Add,
    Sub,
    Div,
    Mul,
    Integer(u64),
    Float(f64),
    OpenCurly,
    CloseCurly,
    OpenSquare,
    CloseSquare,
    OpenParen,
    CloseParen,
    SemiColon,
    Comma,
    Equal,
}

pub fn lexer(slice: &[u8]) -> Vec<Token> {
    let mut x = tokenizer(slice).unwrap().1;
    find_keywords(&mut x);
    x
}

fn find_keywords(v: &mut Vec<Token>) {
    for t in v.iter_mut() {
       match t.clone() {
            Token::Identifier(s) => {match s.as_str() {
                    "let" => *t = Token::Let,
                    _ => {}
                }},
            _ => {}
        };
    }
}

named!(pub tokenizer<Vec<Token>>, do_parse!(
    f: foo >>
    s: foo >>
    (f)
));

named!(foo<Vec<Token>>,
    alt!( bracket
        | many0!( alt!( operator
                |   punctuation
                |   Integer
                |   identifier)))
);

named!(pub word<String>, map_res!(
    map_res!(
        ws!(alphanumeric),
        str::from_utf8
    ),
    FromStr::from_str
));

named!(identifier<Token>, do_parse!(
    n: word >>
    (Token::Identifier(n))
));

named!(operator<Token>, do_parse!(
    p: ws!(alt!(
          tag!("*")
        | tag!("/")
        | tag!("+")
        | tag!("-")
    )) >>
    (match p[0] as char {
        '*' => Token::Mul,
        '/' => Token::Div,
        '+' => Token::Add,
        '-' => Token::Sub,
        _ => panic!("operator needs more cases")
    })
));

named!(pub bracket<Vec<Token>>, do_parse!(
    tokens: delimited!(
            ws!(tag!("{")),
            tokenizer,
            ws!(tag!("}"))
        ) >>
    ({
        let mut mv = tokens.clone();
        mv.push(Token::CloseCurly);
        mv.insert(0, Token::OpenCurly);
        mv
    })
));

named!(punctuation<Token>, do_parse!(
    p: ws!(alt!(
          tag!("(")
        | tag!(")")
        | tag!(";")
        | tag!(",")
        | tag!("=")
    )) >>
    (match p[0] as char {
        '(' => Token::OpenParen,
        ')' => Token::CloseParen,
        ',' => Token::Comma,
        ';' => Token::SemiColon,
        '=' => Token::Equal,
        _ => panic!("punctuation needs more cases")
    })
));

named!(Integer<Token>, do_parse!(
    n: map_res!(
          map_res!(
            ws!(digit),
            str::from_utf8
          ),
          FromStr::from_str
        ) >>
    (Token::Integer(n))
));