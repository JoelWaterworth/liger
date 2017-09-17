use nom::{digit, alpha, alphanumeric};
use nom;
use std::str;
use std::str::FromStr;

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Add,
    And,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    CloseCurly,
    CloseParen,
    CloseSquare,
    Colon,
    Comma,
    Div,
    Equal,
    Float(f64),
    Identifier(String),
    Integer(u64),
    Let,
    Mul,
    OpenCurly,
    OpenParen,
    OpenSquare,
    RightArrow,
    SemiColon,
    Sub,
    Or,
    GE,
    LE,
    GT,
    LT,
    Not,
    IsEqualTo,
    IsNotEqualTo,
    Struct,
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
                    "struct" => *t = Token::Struct,
                    _ => {}
                }},
            _ => {}
        };
    }
}

named!(tokenizer<Vec<Token>>, many0!(
    alt!(   multi_char_tokens
        |   char_tokens
        |   Integer
        |   identifier)
));

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

named!(multi_char_tokens<Token>, do_parse!(
    p: ws!(alt!(
            tag!("->")
        |   tag!("&&")
        |   tag!("||")
        |   tag!("<=")
        |   tag!(">=")
        |   tag!("==")
        |   tag!("!=")
        )) >>

    (match p {
        b"->" => Token::RightArrow,
        b"&&" => Token::And,
        b"||" => Token::Or,
        b"<=" => Token::LE,
        b">=" => Token::GE,
        b"==" => Token::IsEqualTo,
        b"!=" => Token::IsNotEqualTo,
        _ => panic!("multi_char_tokens needs more cases")
    })
));

named!(char_tokens<Token>, do_parse!(
    p: ws!(alt!(
          tag!("(")
        | tag!(")")
        | tag!(";")
        | tag!(",")
        | tag!("=")
        | tag!("{")
        | tag!("}")
        | tag!("*")
        | tag!("/")
        | tag!("+")
        | tag!("-")
        | tag!(":")
        | tag!("!")
        | tag!("<")
        | tag!(">")
        | tag!("&")
        | tag!("|")
        | tag!("^")
    )) >>
    (match p[0] as char {
        '(' => Token::OpenParen,
        ')' => Token::CloseParen,
        ',' => Token::Comma,
        ';' => Token::SemiColon,
        '=' => Token::Equal,
        '{' => Token::OpenCurly,
        '}' => Token::CloseCurly,
        '*' => Token::Mul,
        '/' => Token::Div,
        '+' => Token::Add,
        '-' => Token::Sub,
        ':' => Token::Colon,
        '!' => Token::Not,
        '<' => Token::LT,
        '>' => Token::GT,
        '&' => Token::BitwiseAnd,
        '|' => Token::BitwiseOr,
        '^' => Token::BitwiseXor,
        _ => panic!("char_tokens needs more cases")
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