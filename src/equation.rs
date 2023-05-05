/*
 *   Copyright (c) 2022 Ludwig Bogsveen
 *   All rights reserved.

 *   Permission is hereby granted, free of charge, to any person obtaining a copy
 *   of this software and associated documentation files (the "Software"), to deal
 *   in the Software without restriction, including without limitation the rights
 *   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 *   copies of the Software, and to permit persons to whom the Software is
 *   furnished to do so, subject to the following conditions:
 
 *   The above copyright notice and this permission notice shall be included in all
 *   copies or substantial portions of the Software.
 
 *   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 *   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 *   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 *   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 *   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 *   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 *   SOFTWARE.
 */

use std::fmt::Display;

use pest::{iterators::Pair, Parser};

use crate::complex::Complex;

pub struct Equation {
    base: TokenNode
}

impl Equation {
    pub fn new(equation: &str) -> Equation {
        Equation {
            base: tokenize(equation),
        }
    }

    pub fn sum(&self) -> Complex {
        self.base.sum()
    }

    pub fn to_string(&self) -> String {
        self.base.to_string()
    }
}

struct TokenNode {
    token: Token,
    lhs: Option<Box<TokenNode>>,
    rhs: Option<Box<TokenNode>>,
}

impl TokenNode {
    fn with_token(token: Token) -> TokenNode {
        TokenNode {
            token,
            lhs: None,
            rhs: None,
        }
    }

    fn sum(&self) -> Complex {
        let token = self.token;
        match token {
            Token::Plus     => return self.lhs.as_ref().unwrap().sum() + self.rhs.as_ref().unwrap().sum(),
            Token::Minus    => return self.lhs.as_ref().unwrap().sum() - self.rhs.as_ref().unwrap().sum(),
            Token::Multiply => return self.lhs.as_ref().unwrap().sum() * self.rhs.as_ref().unwrap().sum(),
            Token::Divide   => return self.lhs.as_ref().unwrap().sum() / self.rhs.as_ref().unwrap().sum(),
            Token::Power    => return self.lhs.as_ref().unwrap().sum().powf(self.rhs.as_ref().unwrap().sum()),

            Token::Function(f) => return f.run(self.rhs.as_ref().unwrap().sum()),
    
            Token::Num(num) => return num,
            _ => {
                println!("Error while sumating the equation!!!");
                return Complex {real: 0.0, img: 0.0}
            }
        }
    }

    fn to_string(&self) -> String {
        let token = self.token;
        match token {
            Token::Plus     => format!("{} + {}", self.lhs.as_ref().unwrap().to_string(), self.rhs.as_ref().unwrap().to_string()),
            Token::Minus    => format!("{} - {}", self.lhs.as_ref().unwrap().to_string(), self.rhs.as_ref().unwrap().to_string()),
            Token::Multiply => format!("{} * {}", self.lhs.as_ref().unwrap().to_string(), self.rhs.as_ref().unwrap().to_string()),
            Token::Divide   => format!("{} / {}", self.lhs.as_ref().unwrap().to_string(), self.rhs.as_ref().unwrap().to_string()),
            Token::Power    => format!("{} ^ {}", self.lhs.as_ref().unwrap().to_string(), self.rhs.as_ref().unwrap().to_string()),

            Token::LeftParens    => format!("("),
            Token::RightParens   => format!(")"),

            Token::Function(f) => format!("{} {}", f, self.rhs.as_ref().unwrap().to_string()),
    
            Token::Num(num) => num.to_string(),
            _ => {
                println!("Error while crating the string!!!");
                return String::new()
            }
        }
    }
}

#[derive(Copy, Clone)]

enum Function {
    Sin,
    Cos,
    Tan,
}

impl Function {
    fn run(&self, x: Complex) -> Complex {
        match self {
            /*Self::Sin => x.sin(),
            Self::Cos => x.cos(),
            Self::Tan => x.tan(),*/
            _ => panic!(),
        }
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Sin => write!(f, "sin"),
            Self::Cos => write!(f, "cos"),
            Self::Tan => write!(f, "tan"),
        }
    }
}

#[derive(Copy, Clone)]
enum Token {
    None,

    Plus,
    Minus,
    Multiply,
    Divide,
    Power,

    LeftParens,
    RightParens,

    Function(Function),

    Equals,
    
    Num(Complex)
}

impl Token {
    fn print(&self) {
        match self {
            Token::None => print!("None!"),
    
            Token::Plus     => print!("+"),
            Token::Minus    => print!("-"),
            Token::Multiply => print!("*"),
            Token::Divide   => print!("/"),
            Token::Power    => print!("^"),
    
            Token::LeftParens  => print!("("),
            Token::RightParens => print!(")"),
    
            Token::Equals => print!("="),
    
            Token::Num(num) => print!("{}", num),

            Token::Function(f) => print!("{}", f),
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::None => write!(f, "None!"),
    
            Token::Plus     => write!(f, "+"),
            Token::Minus    => write!(f, "-"),
            Token::Multiply => write!(f, "*"),
            Token::Divide   => write!(f, "/"),
            Token::Power    => write!(f, "^"),
    
            Token::LeftParens  => write!(f, "("),
            Token::RightParens => write!(f, ")"),
    
            Token::Equals => write!(f, "="),
    
            Token::Num(num) => write!(f, "{}", num),

            Token::Function(func) => write!(f, "{}", func),
        }
    }
}

fn check_fn(chars: &[char], index: &mut usize) -> Token {
    const FUNCTIONS: [&str; 3] = ["sin", "cos", "tan"];
    
    let mut str = String::new();

    let mut fine = true;
    while fine {
        fine = false;
        
        str.push(chars[*index]);

        for f in FUNCTIONS.iter() {
            if f.starts_with(&str) {
                fine = true;
            }

            if f == &str {
                match *f {
                    "sin" => return Token::Function(Function::Sin),
                    "cos" => return Token::Function(Function::Cos),
                    "tan" => return Token::Function(Function::Tan),
                    _ => panic!(),
                }
            }
        }
        
        *index += 1;
    }

    Token::None
}

fn tokenize(part: &str) -> TokenNode {
    let mut pairs = match InternalParser::parse(Rule::Program, part) {
        Ok(pairs) => pairs,
        Err(error) => {
            panic!("{}", error);
        }
    };

    parse_expr(pairs.next().unwrap().into_inner().next().unwrap())
}

fn parse_expr(pair: Pair<Rule>) -> TokenNode {
    match pair.as_rule() {
        Rule::Complex => {
            let mut pairs = pair.into_inner();
            let pair = pairs.next().unwrap();
            
            let val = match pair.as_rule() {
                Rule::Real => {
                    let val = pair.as_str().trim().parse::<f64>().expect(&format!("Found an invalid number : [{}]", pair.as_str().trim()));

                    let img = match pairs.next() {
                        Some(pair) => match pair.as_rule() {
                            Rule::ImgUnit => {
                                1.0
                            },
                            _ => panic!(),
                        },
                        None => { 0.0 }
                    };

                    Complex {real: val * (1.0-img), img: val * img}
                },
                Rule::ImgUnit => {
                    Complex {real: 0.0, img: 1.0}
                },
                _ => panic!(),
            };

            TokenNode {
                token: Token::Num(val),
                lhs: None,
                rhs: None,
            }
        },
        Rule::Expr => {
            let mut pairs = pair.into_inner();
            let mut lhs = parse_expr(pairs.next().unwrap());
            let mut op;
            let mut rhs;

            while let Some(pair) = pairs.next() {
                match pair.as_rule() {
                    Rule::BinaryOperator => {
                        op = match pair.as_str().trim() {
                            "+" => Token::Plus,
                            "-" => Token::Minus,
                            "*" => Token::Multiply,
                            "/" => Token::Divide,
                            "^" => Token::Power,
                            op => panic!("invalid operator: {}", op)
                        };
                    },
                    _ => panic!(),
                }
                let pair = pairs.next().unwrap();
                rhs = parse_expr(pair);
                lhs = TokenNode { token: op, lhs: Some(Box::new(lhs)), rhs: Some(Box::new(rhs)) };
            }

            lhs
        }
        Rule::UnaryExpr => {
            let mut pairs = pair.into_inner();
            let pair = pairs.next().unwrap();
            let op;
            match pair.as_rule() {
                Rule::UnaryOperator => {
                    op = match pair.as_str().trim() {
                        "+" => Token::Plus,
                        "-" => Token::Minus,
                        op => panic!("invalid operator: {}", op)
                    };
                },
                _ => panic!(),
            }
            let pair = pairs.next().unwrap();
            let expr = parse_expr(pair);

            TokenNode {token: op, lhs: Some(Box::new(TokenNode {token: Token::Num(Complex {real: 0.0, img: 0.0}), lhs: None, rhs: None })), rhs: Some(Box::new(expr))}
        }
        Rule::NonBinaryExpr => {
            parse_expr(pair.into_inner().next().unwrap())
        }
        _ => panic!("{:?}", pair.as_rule()),
    }
}

#[derive(pest_derive::Parser)]
#[grammar = "grammar.pest"]
struct InternalParser;