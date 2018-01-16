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
pub fn lexer1(slice: &mut Chars) -> Vec<Token> {
    let mut tokens = Vec::new();
    loop {
        skip_white_space(slice);
        let mut temp = slice.clone();
        match temp.next() {
            Some(c) => {
                match c {
                    '(' => {
                        slice.next();
                        tokens.push(Token::OpenParen);
                    },
                    ')' => {
                        slice.next();
                        tokens.push(Token::CloseParen)
                    },
                    '[' => {
                        slice.next();
                        tokens.push(Token::OpenSquare);
                    },
                    ']' => {
                        slice.next();
                        tokens.push(Token::CloseSquare)
                    },
                    ',' => {
                        slice.next();
                        tokens.push(Token::Comma)
                    },
                    ';' => {
                        slice.next();
                        tokens.push(Token::SemiColon)
                    },
                    '=' => {
                        slice.next();
                        match temp.next() {
                            Some('=') => {
                                slice.next();
                                tokens.push(Token::IsEqualTo)
                            },
                            _ => tokens.push(Token::Equal)
                        }
                    },
                    '{' => {
                        slice.next();
                        tokens.push(Token::OpenCurly)
                    },
                    '}' => {
                        slice.next();
                        tokens.push(Token::CloseCurly)
                    },
                    '*' => {
                        slice.next();
                        tokens.push(Token::Mul)
                    },
                    '/' => {
                        slice.next();
                        tokens.push(Token::Div)
                    },
                    '+' => {
                        slice.next();
                        tokens.push(Token::Add)
                    },
                    '-' => {
                        slice.next();
                        match temp.next() {
                            Some('>') => {
                                slice.next();
                                tokens.push(Token::RightArrow)
                            },
                            _ => {
                                tokens.push(Token::Sub)
                            }
                        }
                    },
                    ':' => {
                        slice.next();
                        tokens.push(Token::Colon)
                    },
                    '!' => {
                        slice.next();
                        match temp.next() {
                            Some('=') => {
                                slice.next();
                                tokens.push(Token::IsNotEqualTo)
                            },
                            _ => tokens.push(Token::Not)
                        }
                    },
                    '<' => {
                        slice.next();
                        match temp.next() {
                            Some('=') => {
                                slice.next();
                                tokens.push(Token::LE)
                            },
                            _ => tokens.push(Token::LT)
                        }
                    },
                    '>' => {
                        slice.next();
                        match temp.next() {
                            Some('=') => {
                                slice.next();
                                tokens.push(Token::GE)
                            },
                            _ => tokens.push(Token::GT)
                        }
                    },
                    '&' => {
                        slice.next();
                        tokens.push(Token::BitwiseAnd)
                    },
                    '|' => {
                        slice.next();
                        tokens.push(Token::BitwiseOr)
                    },
                    '^' => {
                        slice.next();
                        tokens.push(Token::BitwiseXor)
                    },
                    '.' => {
                        slice.next();
                        tokens.push(Token::Dot)
                    },
                    '"' => {
                        slice.next();
                        tokens.push(Token::Quote)
                    },
                    '\n' => {
                        slice.next();
                    },
                    '\r' => {
                        slice.next();
                    },
                    ' ' => {
                        slice.next();
                    },
                    _ => {
                        if c.is_alphabetic() {
                            tokens.push(Token::Identifier(identifier1(slice)))
                        } else if c.is_numeric() {
                            tokens.push(Token::Integer(number1(slice)))
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

fn skip_white_space(text:&mut Chars) {
    let mut slice = text.clone();
    loop {
        match slice.next() {
            Some(' ') => {
                text.next();
            }
            Some('\n') => {
                text.next();
            }
            _ => break
        }
    }
}

fn identifier1(text:&mut Chars) -> String {
    let mut string = String::new();
    let mut slice = text.clone();
    loop {
        match slice.next() {
            Some(c) => {
                if c.is_alphanumeric() {
                    string.push(c);
                    text.next();
                } else {
                    break
                }
            }
            None => break
        }
    }
    string
}

fn number1(text:&mut Chars) -> u64 {
    let mut number = 0;
    let mut slice = text.clone();
    loop {
        match slice.next() {
            Some(c) => {
                if c.is_numeric() {
                    number = (number * 10) + c.to_digit(10).unwrap() as u64;
                    text.next();
                } else {
                    break
                }
            }
            None => break
        }
    }
    number
}

pub fn lexer(slice: String) -> Vec<Token> {
    let mut x = lexer1(&mut slice.chars());
    find_keywords(&mut x);
    println!("lexer finished");
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
                    "mut"       => *t = Token::Mut,
                    "extern"    => *t = Token::Extern,
                    "link"      => *t = Token::Link,
                    "delete"    => *t = Token::Delete,
                    "new"       => *t = Token::New,
                    _ => {}
                }},
            _ => {}
        };
    }
}