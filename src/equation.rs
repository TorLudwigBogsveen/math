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

use core::panic;
use std::{fmt::Display, collections::HashMap, hash::Hash, f64::consts::{PI, TAU, E}};

use pest::{iterators::{Pair, Pairs}, Parser, pratt_parser::{PrattParser, Op, Assoc}};

use crate::{complex::Complex, functions::{factorial, differentiate, equal_f64}};

#[derive(Debug)]
pub enum Error {
    ParseError
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl std::error::Error for Error {}

#[derive(Debug)]
pub struct Equation {
    base: Node,
    pub vars: Vars,
}

impl Equation {
    pub fn new(equation: &str) -> Equation {
        Equation {
            base: tokenize(equation),
            vars: Vars::new(),
        }
    }

    pub fn sum(&self) -> Result<Node, Error> {
        self.base.sum(&self.vars)
    }
    
    pub fn call_on(&mut self, vars: &[(&str, f64)]) -> Node {
        for (var, val) in vars {
            self.vars.set_real(var, *val);
        }
        self.sum().unwrap() 
    }
}

#[derive(Debug)]
pub struct Vars {
    vars: HashMap<String, Node>   
}

impl Vars {
    fn new() -> Self {
        let mut s = Self {
            vars: HashMap::new()
        };

        s.set_real("PI", PI);
        s.set_real("TAU", TAU);
        s.set_real("e", E);
        s.set_real("phi", (1.0 + (5.0 as f64).sqrt()) / 2.0);

        s
    }

    pub fn set_real(&mut self, var: &str, val: f64) -> Option<Node> {
        self.vars.insert(var.to_lowercase(), Node::Real(val))
    }

    pub fn set_complex(&mut self, var: &str, val: Complex) -> Option<Node> {
        self.vars.insert(var.to_lowercase(), Node::Complex(val))
    }

    pub fn get(&self, var: &str) -> Option<Node> {
        self.vars.get(var).map(|node| node.clone())
    }

    pub fn get_real(&self, var: &str) -> Option<f64> {
        if let Some(node) = self.vars.get(&var.to_lowercase()) {
            match node {
                Node::Real(val) => Some(*val),
                _ => None,
            }
        } else {
            None
        }
    }

    pub fn get_complex(&self, var: &str) -> Option<Complex> {
        if let Some(node) = self.vars.get(&var.to_lowercase()) {
            match node {
                Node::Complex(val) => Some(*val),
                _ => None,
            }
        } else {
            None
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Function {
    Sin,
    Cos,
    Tan,
    ArcSin,
    ArcCos,
    ArcTan,
    Sqrt,
    Round,
    Floor,
    Ceil,
    Log,
    Log10,
    Log2,
    Ln,
    Min,
    Max,
    Abs,
    //Diff,
}

impl Function {
    fn eval(&self, vars: &Vars, args: &Vec<Node>) -> Result<Node, Error> {
        let val = match args[..] {
            [Node::Real(num)] => Node::Real(match self {
                Self::Sin => num.sin(),
                Self::Cos => num.cos(),
                Self::Tan => num.tan(),
                Self::ArcSin => num.asin(),
                Self::ArcCos => num.acos(),
                Self::ArcTan => num.atan(),
                Self::Sqrt => num.sqrt(),
                Self::Round => num.round(),
                Self::Floor => num.floor(),
                Self::Ceil => num.ceil(),
                Self::Log => num.log10(),
                Self::Log10 => num.log10(),
                Self::Log2 => num.log2(),
                Self::Ln => num.ln(),
                Self::Abs => num.abs(),
                //Self::Diff => differentiate(&mut vars.clone(), node, "x", num)?, //TODO: FIX
                Self::Min | Self::Max => Err(Error::ParseError)?,
            }),
            [Node::Complex(num)] => match self {
                Self::Sin => todo!(),
                Self::Cos => todo!(),
                Self::Tan => todo!(),
                Self::ArcSin => todo!(),
                Self::ArcCos => todo!(),
                Self::ArcTan => todo!(),
                Self::Sqrt => Node::Complex(num.sqrt()),
                Self::Round => Node::Complex(num.round()),
                Self::Floor => Node::Complex(num.floor()),
                Self::Ceil => Node::Complex(num.ceil()),
                Self::Abs => Node::Real(num.mag()),
                Self::Log => todo!(),
                Self::Log10 => todo!(),
                Self::Log2 => todo!(),
                Self::Ln => todo!(),
                Self::Min | Self::Max => Err(Error::ParseError)?,
                
            },
            [Node::Real(num), Node::Real(num2)] => Node::Real(match self {
                Self::Log => num2.log(num),
                Self::Min => num.min(num2),
                Self::Max => num.max(num2),
                _ => Err(Error::ParseError)?,
            }),
            [Node::Complex(num), Node::Complex(num2)] => match self {
                Self::Min => Node::Complex(if num.magsq() < num2.magsq() { num } else { num2 }),
                Self::Max => Node::Complex(if num.magsq() > num2.magsq() { num } else { num2 }),
                _ => Err(Error::ParseError)?,
            },
            _ => Err(Error::ParseError)?
        };

        Ok(val)
    }

    fn from_string(s: &str) -> Result<Self, Error> {
        Ok(match s {
            "sin" => Self::Sin,
            "cos" => Self::Cos,
            "tan" => Self::Tan,
            "arcsin" => Self::ArcSin,
            "arccos" => Self::ArcCos,
            "arctan" => Self::ArcTan,
            "sqrt" => Self::Sqrt,
            "round" => Self::Round,
            "floor" => Self::Floor,
            "ceil" => Self::Ceil,
            "log" => Self::Log,
            "log10" => Self::Log10,
            "log2" => Self::Log2,
            "ln" => Self::Ln,
            "min" => Self::Min,
            "max" => Self::Max,
            "abs" => Self::Abs,
            _ => Err(Error::ParseError)?
        })
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Sin => write!(f, "sin"),
            Self::Cos => write!(f, "cos"),
            Self::Tan => write!(f, "tan"),
            Self::ArcSin => write!(f, "arcsin"),
            Self::ArcCos => write!(f, "arccos"),
            Self::ArcTan => write!(f, "arctan"),
            Self::Sqrt => write!(f, "sqrt"),
            Self::Round => write!(f, "round"),
            Self::Floor => write!(f, "floor"),
            Self::Ceil => write!(f, "ceil"),
            Self::Log => write!(f, "log"),
            Self::Log10 => write!(f, "log10"),
            Self::Log2 => write!(f, "log2"),
            Self::Ln => write!(f, "ln"),
            Self::Min => write!(f, "min"),
            Self::Max => write!(f, "max"),
            Self::Abs => write!(f, "abs"),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum PrefixOperation {
    Plus,
    Neg,
}

impl PrefixOperation {
    fn eval(self, val: &Node) -> Result<Node, Error> {
        Ok(match val {
            Node::Real(val) => 
                match self {
                    Self::Plus => Node::Real(*val),
                    Self::Neg => Node::Real(-*val),
                }
            _ => Err(Error::ParseError)?,
        })
    }
}

#[derive(Debug, Copy, Clone)]
pub enum BinaryOperation {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    Mod,

    LessThan,
    GreaterThan,

    LessEqualTo,
    GreaterEqualTo,

    Equal,

    And,
    Nand,
    Or,
    Nor,
    Xor,
    Xnor,
}

impl BinaryOperation {
    fn eval(self, lhs: &Node, rhs: &Node) -> Result<Node, Error> {
        Ok(match (lhs, rhs) {
            (Node::Bool(lhs), Node::Bool(rhs)) => 
                Node::Bool(match self {
                    Self::And => *lhs & *rhs,
                    Self::Or => *lhs | *rhs,
                    Self::Xor => *lhs ^ *rhs,
                    Self::Nand => !(*lhs & *rhs),
                    Self::Nor => !(*lhs | *rhs),
                    Self::Xnor => !(*lhs ^ *rhs),
                    _ => Err(Error::ParseError)?,
                }),
            (Node::Real(lhs), Node::Real(rhs)) => 
                match self {
                    BinaryOperation::Add => Node::Real(lhs + rhs),
                    BinaryOperation::Sub => Node::Real(lhs - rhs),
                    BinaryOperation::Mul => Node::Real(lhs * rhs),
                    BinaryOperation::Div => Node::Real(lhs / rhs),
                    BinaryOperation::Mod => Node::Real(lhs.rem_euclid(*rhs)),
                    BinaryOperation::Pow => Node::Real(lhs.powf(*rhs)),
                    BinaryOperation::LessThan => Node::Bool(*lhs < *rhs),
                    BinaryOperation::GreaterThan => Node::Bool(*lhs > *rhs),
                    BinaryOperation::LessEqualTo => Node::Bool(*lhs <= *rhs),
                    BinaryOperation::GreaterEqualTo => Node::Bool(*lhs >= *rhs),
                    BinaryOperation::Equal => Node::Bool(equal_f64(*lhs, *rhs)),
                    _ => Err(Error::ParseError)?,
                },
            (Node::Complex(lhs), Node::Complex(rhs)) =>
                Node::Complex(match self {
                    BinaryOperation::Add => *lhs + *rhs,
                    BinaryOperation::Sub => *lhs - *rhs,
                    BinaryOperation::Mul => *lhs * *rhs,
                    BinaryOperation::Div => *lhs / *rhs,
                    BinaryOperation::Mod => Err(Error::ParseError)?,
                    BinaryOperation::Pow => lhs.powf(*rhs),
                    _ => Err(Error::ParseError)?,
                }),
            (Node::Real(lhs), Node::Complex(rhs)) =>
                Node::Complex(match self {
                    BinaryOperation::Add => Complex { real: *lhs, img: 0.0 } + *rhs,
                    BinaryOperation::Sub => Complex { real: *lhs, img: 0.0 } - *rhs,
                    BinaryOperation::Mul => Complex { real: *lhs, img: 0.0 } * *rhs,
                    BinaryOperation::Div => Complex { real: *lhs, img: 0.0 } / *rhs,
                    BinaryOperation::Mod => Err(Error::ParseError)?,
                    BinaryOperation::Pow => Complex { real: *lhs, img: 0.0 }.powf(*rhs),
                    _ => Err(Error::ParseError)?,
                }),
            (Node::Complex(lhs), Node::Real(rhs)) =>
                Node::Complex(match self {
                    BinaryOperation::Add => *lhs + Complex { real: *rhs, img: 0.0 },
                    BinaryOperation::Sub => *lhs - Complex { real: *rhs, img: 0.0 },
                    BinaryOperation::Mul => *lhs * Complex { real: *rhs, img: 0.0 },
                    BinaryOperation::Div => *lhs / Complex { real: *rhs, img: 0.0 },
                    BinaryOperation::Mod => Err(Error::ParseError)?,
                    BinaryOperation::Pow => lhs.powf(Complex { real: *rhs, img: 0.0 }),
                    _ => Err(Error::ParseError)?,
                }),
            _ => Err(Error::ParseError)?,
        })
    }
}

#[derive(Debug, Copy, Clone)]
pub enum PostfixOperation {
    Factorial
}

impl PostfixOperation {
    fn eval(self, val: &Node) -> Result<Node, Error> {
        Ok(match val {
            Node::Real(val) => 
                match self {
                    Self::Factorial => Node::Real(factorial(*val as usize) as f64),
                }
            _ => Err(Error::ParseError)?,
        })
    }
}

#[derive(Debug, Clone)]
pub enum Node {
    Bool(bool),
    Real(f64),
    Complex(Complex),
    PrefixOperation{op: PrefixOperation, rhs: Box<Node>},
    BinaryOperation{lhs: Box<Node>, op: BinaryOperation, rhs: Box<Node>},
    PostOperation{lhs: Box<Node>, op: PostfixOperation},
    Function(Function, Vec<Node>),
    Var(String),
    Equals,
}

impl Node {

    pub fn as_f64(&self) -> Option<f64> {
        match self {
            Node::Real(val) => Some(*val),
            _ => None,
        }
    }

    pub fn as_complex(&self) -> Option<Complex> {
        match self {
            Node::Complex(val) => Some(*val),
            _ => None,
        }
    }

    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Node::Bool(val) => Some(*val),
            _ => None,
        }
    }

    pub fn sum(&self, vars: &Vars) -> Result<Node, Error> {
        Ok(match self {
            Node::Bool(b) => Node::Bool(*b),
            Node::Real(val) => Node::Real(*val),
            Node::Complex(val) => Node::Complex(*val),
            Node::PrefixOperation { op, rhs } => op.eval(&rhs.sum(vars)?)?,
            Node::BinaryOperation { lhs, op, rhs } => op.eval(&lhs.sum(vars)?, &rhs.sum(vars)?)?,
            Node::PostOperation { lhs, op } => op.eval(&lhs.sum(vars)?)?,
            Node::Function(func, args) => func.eval(vars, &args.into_iter().map(|node| node.sum(vars)).collect::<Result<Vec<Node>, Error>>()?)?,
            Node::Var(var) => vars.get(var).ok_or(Error::ParseError)?,
            Node::Equals => todo!(),
        })
    }
}

impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Node::Bool(b) => f.write_fmt(format_args!("{}", b)),
            Node::Real(val) => f.write_fmt(format_args!("{}", val)),
            Node::Complex(val) => f.write_fmt(format_args!("{}", val)),
            Node::PrefixOperation { op, rhs } => todo!(),
            Node::BinaryOperation { lhs, op, rhs } => todo!(),
            Node::PostOperation { lhs, op } => todo!(),
            Node::Function(_, _) => todo!(),
            Node::Var(_) => todo!(),
            Node::Equals => todo!(),
        }
    }
}

fn tokenize(part: &str) -> Node {
    let parser = PrattParser::<Rule>::new()
    .op(
        Op::infix(Rule::Or, Assoc::Left) | Op::infix(Rule::Nor, Assoc::Left) |
        Op::infix(Rule::And, Assoc::Left) | Op::infix(Rule::Nand, Assoc::Left) |
        Op::infix(Rule::Xor, Assoc::Left) | Op::infix(Rule::Xnor, Assoc::Left))
    .op(
        Op::infix(Rule::LessThan, Assoc::Left) | Op::infix(Rule::GreaterThan, Assoc::Left) | 
        Op::infix(Rule::LessEqualTo, Assoc::Left) | Op::infix(Rule::GreaterEqualTo, Assoc::Left) | 
        Op::infix(Rule::Equal, Assoc::Left))
    .op(Op::infix(Rule::Add, Assoc::Left) | Op::infix(Rule::Sub, Assoc::Left))
    .op(Op::infix(Rule::Mul, Assoc::Left) | Op::infix(Rule::Div, Assoc::Left) | Op::infix(Rule::Mod, Assoc::Left))
    .op(Op::infix(Rule::Pow, Assoc::Left))
    .op(Op::prefix(Rule::Plus) | Op::prefix(Rule::Neg))
    .op(Op::postfix(Rule::Factorial));

    let mut pairs = match InternalParser::parse(Rule::Program, part) {
        Ok(pairs) => pairs,
        Err(error) => {
            panic!("{}", error);
        }
    };

    parse_expr(pairs.next().unwrap().into_inner(), &parser).unwrap()
}

fn parse_complex(mut pairs: Pairs<Rule>) -> Result<Node, Error> {
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
        _ => Err(Error::ParseError)?,
    };
    Ok(Node::Complex(val))
}

fn parse_real(pair: Pair<Rule>) -> Result<Node, Error> {
    let val = pair.as_str().trim().parse::<f64>().expect(&format!("Found an invalid number : [{}]", pair.as_str().trim()));
    Ok(Node::Real(val))
}


fn parse_prefix(pair: Pair<Rule>) -> Result<PrefixOperation, Error> {
    match pair.as_rule() {
        Rule::Plus => Ok(PrefixOperation::Plus),
        Rule::Neg => Ok(PrefixOperation::Neg),
        _ => panic!()
    }
}

fn parse_infix(pair: Pair<Rule>) -> Result<BinaryOperation, Error> {
    match pair.as_rule() {
        Rule::Add => Ok(BinaryOperation::Add),
        Rule::Sub => Ok(BinaryOperation::Sub),
        Rule::Mul => Ok(BinaryOperation::Mul),
        Rule::Div => Ok(BinaryOperation::Div),
        Rule::Mod => Ok(BinaryOperation::Mod),
        Rule::Pow => Ok(BinaryOperation::Pow),

        Rule::LessThan => Ok(BinaryOperation::LessThan),
        Rule::GreaterThan => Ok(BinaryOperation::GreaterThan),
        Rule::LessEqualTo => Ok(BinaryOperation::LessEqualTo),
        Rule::GreaterEqualTo => Ok(BinaryOperation::GreaterEqualTo),

        Rule::Equal => Ok(BinaryOperation::Equal),

        Rule::And => Ok(BinaryOperation::And),
        Rule::Nand => Ok(BinaryOperation::Nand),
        Rule::Or => Ok(BinaryOperation::Or),
        Rule::Nor => Ok(BinaryOperation::Nor),
        Rule::Xor => Ok(BinaryOperation::Xor),
        Rule::Xnor => Ok(BinaryOperation::Xnor),

        _ => panic!()
    }
}

fn parse_postfix(pair: Pair<Rule>) -> Result<PostfixOperation, Error> {
    match pair.as_rule() {
        Rule::Factorial => Ok(PostfixOperation::Factorial),
        _ => panic!()
    }
}

fn parse_function_call(mut pairs: Pairs<Rule>, parser: &PrattParser<Rule>) -> Result<Node, Error> {
    let function_name = pairs.next().unwrap().as_str();

    let mut args = Vec::new();
    
    for pair in pairs {
        args.push(parse_expr(pair.into_inner(), parser)?)
    }

    Ok(Node::Function(Function::from_string(function_name)?, args))
}

fn parse_var(pair: Pair<Rule>) -> Result<Node, Error> {
    Ok(Node::Var(pair.as_str().to_string()))
}

fn parse_expr(pairs: Pairs<Rule>, parser: &PrattParser<Rule>) -> Result<Node, Error> {
    parser
    .map_primary(|primary| {
        match primary.as_rule() {
            Rule::Complex => parse_complex(primary.into_inner()),
            Rule::Real => parse_real(primary),
            Rule::FunctionCall => parse_function_call(primary.into_inner(), parser),
            Rule::Var => parse_var(primary),
            Rule::Abs => Ok(Node::Function(Function::Abs, vec![parse_expr(primary.into_inner(), parser)?])),
            Rule::Expr => parse_expr(primary.into_inner(), parser),
            _ => panic!("Unexpected Node {:?}", primary.as_rule()),
    }})
    .map_prefix(|op, rhs| {
        let op = parse_prefix(op)?;
        Ok(Node::PrefixOperation { op, rhs: Box::new(rhs?) })
    })
    .map_infix(|lhs, op, rhs| {
        let op = parse_infix(op)?;
        Ok(Node::BinaryOperation { lhs: Box::new(lhs?), op, rhs: Box::new(rhs?) })
    })
    .map_postfix(|lhs, op| {
        let op = parse_postfix(op)?;
        Ok(Node::PostOperation { lhs: Box::new(lhs?), op})
    })
    .parse(pairs)
}

#[derive(pest_derive::Parser)]
#[grammar = "grammar.pest"]
struct InternalParser;