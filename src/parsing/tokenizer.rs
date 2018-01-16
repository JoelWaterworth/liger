use std::str::Chars;

#[allow(dead_code)]
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
    Delete,
    New,
    Or,
    RightArrow,
    SemiColon,
    Struct,
    Sub,
    Return,
    Trait,
    Enum,
    Mut,
    SelfTok,
    Link,
    Extern,
    Quote,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Pos {
    ln: u32,
    cl: u32,
}

pub type TokenPos = (Token, Pos);

impl Pos {
    pub fn new() -> Self {
        Self{ln: 1, cl: 1}
    }
}

pub fn lexer1(slice: &mut Chars) -> Vec<TokenPos> {
    let mut tokens = Vec::new();
    let mut pos = Pos::new();
    loop {
        skip_white_space(slice, &mut pos);
        let mut temp = slice.clone();
        match temp.next() {
            Some(c) => {
                match c {
                    '(' => {
                        pos.cl += 1;
                        slice.next();
                        tokens.push((Token::OpenParen, pos))
                    },
                    ')' => {
                        pos.cl += 1;
                        slice.next();
                        tokens.push((Token::CloseParen, pos))
                    },
                    '[' => {
                        pos.cl += 1;
                        slice.next();
                        tokens.push((Token::OpenSquare, pos))
                    },
                    ']' => {
                        pos.cl += 1;
                        slice.next();
                        tokens.push((Token::CloseSquare, pos))
                    },
                    ',' => {
                        pos.cl += 1;
                        slice.next();
                        tokens.push((Token::Comma, pos))
                    },
                    ';' => {
                        pos.cl += 1;
                        slice.next();
                        tokens.push((Token::SemiColon, pos))
                    },
                    '=' => {
                        pos.cl += 1;
                        slice.next();
                        match temp.next() {
                            Some('=') => {
                                slice.next();
                                tokens.push((Token::IsEqualTo, pos))
                            },
                            _ => tokens.push((Token::Equal, pos))
                        }
                    },
                    '{' => {
                        pos.cl += 1;
                        slice.next();
                        tokens.push((Token::OpenCurly, pos))
                    },
                    '}' => {
                        pos.cl += 1;
                        slice.next();
                        tokens.push((Token::CloseCurly, pos))
                    },
                    '*' => {
                        pos.cl += 1;
                        slice.next();
                        tokens.push((Token::Mul, pos))
                    },
                    '/' => {
                        pos.cl += 1;
                        slice.next();
                        tokens.push((Token::Div, pos))
                    },
                    '+' => {
                        pos.cl += 1;
                        slice.next();
                        tokens.push((Token::Add, pos))
                    },
                    '-' => {
                        pos.cl += 1;
                        slice.next();
                        match temp.next() {
                            Some('>') => {
                                pos.cl += 1;
                                slice.next();
                                tokens.push((Token::RightArrow, pos))
                            },
                            _ => {
                                tokens.push((Token::Sub, pos))
                            }
                        }
                    },
                    ':' => {
                        pos.cl += 1;
                        slice.next();
                        tokens.push((Token::Colon, pos))
                    },
                    '!' => {
                        pos.cl += 1;
                        slice.next();
                        match temp.next() {
                            Some('=') => {
                                pos.cl += 1;
                                slice.next();
                                tokens.push((Token::IsNotEqualTo, pos))
                            },
                            _ => tokens.push((Token::Not, pos))
                        }
                    },
                    '<' => {
                        pos.cl += 1;
                        slice.next();
                        match temp.next() {
                            Some('=') => {
                                pos.cl += 1;
                                slice.next();
                                tokens.push((Token::LE, pos))
                            },
                            _ => tokens.push((Token::LT, pos))
                        }
                    },
                    '>' => {
                        pos.cl += 1;
                        slice.next();
                        match temp.next() {
                            Some('=') => {
                                pos.cl += 1;
                                slice.next();
                                tokens.push((Token::GE, pos))
                            },
                            _ => tokens.push((Token::GT, pos))
                        }
                    },
                    '&' => {
                        pos.cl += 1;
                        slice.next();
                        tokens.push((Token::BitwiseAnd, pos))
                    },
                    '|' => {
                        pos.cl += 1;
                        slice.next();
                        tokens.push((Token::BitwiseOr, pos))
                    },
                    '^' => {
                        pos.cl += 1;
                        slice.next();
                        tokens.push((Token::BitwiseXor, pos))
                    },
                    '.' => {
                        pos.cl += 1;
                        slice.next();
                        tokens.push((Token::Dot, pos))
                    },
                    '"' => {
                        pos.cl += 1;
                        slice.next();
                        tokens.push((Token::Quote, pos))
                    },
                    '\n' => {
                        pos.cl = 0;
                        pos.ln += 1;
                        slice.next();
                    },
                    '\r' => {
                        pos.cl = 0;
                        pos.ln += 1;
                        slice.next();
                    },
                    ' ' => {
                        pos.ln += 1;
                        slice.next();
                    },
                    _ => {
                        if c.is_alphabetic() {
                            let (s, pos) = identifier1(slice, &mut pos);
                            tokens.push((Token::Identifier(s), pos))
                        } else if c.is_numeric() {
                            let (n, pos) = number1(slice, &mut pos);
                            tokens.push((Token::Integer(n), pos))
                        } else {
                            println!("{}", c);
                            break
                        }
                    },
                };
            },
            None => {
                println!("end");
                break
            }
        }
    }
    return tokens
}

fn skip_white_space(text:&mut Chars, pos: &mut Pos) {
    let mut slice = text.clone();
    loop {
        match slice.next() {
            Some(' ') => {
                pos.cl += 1;
                text.next();
            }
            Some('\n') => {
                pos.cl = 0;
                pos.ln += 1;
                text.next();
            }
            _ => break
        }
    }
}

fn identifier1(text:&mut Chars, pos: &mut Pos) -> (String, Pos) {
    let mut string = String::new();
    let mut slice = text.clone();
    loop {
        match slice.next() {
            Some(c) => {
                if c.is_alphanumeric() {
                    pos.cl += 1;
                    string.push(c);
                    text.next();
                } else {
                    break
                }
            }
            None => break
        }
    }
    (string, pos.clone())
}

fn number1(text:&mut Chars, pos: &mut Pos) -> (u64, Pos) {
    let mut number = 0;
    let mut slice = text.clone();
    loop {
        match slice.next() {
            Some(c) => {
                if c.is_numeric() {
                    pos.cl += 1;
                    number = (number * 10) + c.to_digit(10).unwrap() as u64;
                    text.next();
                } else {
                    break
                }
            }
            None => break
        }
    }
    (number, pos.clone())
}

pub fn lexer(slice: String) -> Vec<TokenPos> {
    let mut x = lexer1(&mut slice.chars());
    find_keywords(&mut x);
    println!("lexer finished");
    x
}

fn find_keywords(v: &mut Vec<TokenPos>) {
    for t in v.iter_mut() {
       match t.clone() {
           (Token::Identifier(s), _) => {match s.as_str() {
                    "let"       => t.0 = Token::Let,
                    "struct"    => t.0 = Token::Struct,
                    "if"        => t.0 = Token::If,
                    "import"    => t.0 = Token::Import,
                    "else"      => t.0 = Token::Else,
                    "return"    => t.0 = Token::Return,
                    "trait"     => t.0 = Token::Trait,
                    "enum"      => t.0 = Token::Enum,
                    "Self"      => t.0 = Token::SelfTok,
                    "mut"       => t.0 = Token::Mut,
                    "extern"    => t.0 = Token::Extern,
                    "link"      => t.0 = Token::Link,
                    "delete"    => t.0 = Token::Delete,
                    "new"       => t.0 = Token::New,
                    _ => {}
                }},
            _ => {}
        };
    }
}