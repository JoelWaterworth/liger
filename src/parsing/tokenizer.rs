use nom::{digit, alphanumeric};
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
    Dot,
    Div,
    Else,
    Equal,
    Float(f64),
    GE,
    GT,
    Identifier(String),
    If,
    Import,
    Integer(u64),
    IsEqualTo,
    IsNotEqualTo,
    LE,
    Let,
    LT,
    Mul,
    Not,
    OpenCurly,
    OpenParen,
    OpenSquare,
    Or,
    RightArrow,
    SemiColon,
    Struct,
    Sub,
    Return,
    Trait,
    Enum,
    SelfTok,
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
                    "let"       => *t = Token::Let,
                    "struct"    => *t = Token::Struct,
                    "if"        => *t = Token::If,
                    "import"    => *t = Token::Import,
                    "else"      => *t = Token::Else,
                    "return"    => *t = Token::Return,
                    "trait"     => *t = Token::Trait,
                    "enum"      => *t = Token::Enum,
                    "Self"      => *t = Token::SelfTok,
                    _ => {}
                }},
            _ => {}
        };
    }
}

named!(tokenizer<Vec<Token>>, many0!(
    alt!(   multi_char_tokens
        |   char_tokens
        |   integer
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
        | tag!(".")
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
        '.' => Token::Dot,
        _ => panic!("char_tokens needs more cases")
    })
));

named!(integer<Token>, do_parse!(
    n: map_res!(
          map_res!(
            ws!(digit),
            str::from_utf8
          ),
          FromStr::from_str
        ) >>
    (Token::Integer(n))
));