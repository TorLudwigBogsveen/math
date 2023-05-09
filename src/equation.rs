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
use std::{fmt::Display, collections::HashMap, hash::Hash};

use pest::{iterators::{Pair, Pairs}, Parser, pratt_parser::{PrattParser, Op, Assoc}};

use crate::{complex::Complex, functions::factorial};

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
    
    pub fn call_on(&mut self, vars: &[(&str, f64)]) -> f64 {
        for (var, val) in vars {
            self.vars.set_real(var.to_string(), *val);
        }
        match self.sum() {
            Ok(node) => {
                match node {
                    Node::Real(val) => val,
                    _ => panic!(),
                }
            },
            Err(err) => panic!("{:?}", err),
        }
    }
}

#[derive(Debug)]
pub struct Vars {
    vars: HashMap<String, Node>   
}

impl Vars {
    fn new() -> Self {
        Self {
            vars: HashMap::new()
        }
    }

    pub fn set_real(&mut self, var: String, val: f64) -> Option<Node> {
        self.vars.insert(var, Node::Real(val))
    }

    pub fn set_complex(&mut self, var: String, val: Complex) -> Option<Node> {
        self.vars.insert(var, Node::Complex(val))
    }

    pub fn get(&self, var: &str) -> Option<Node> {
        self.vars.get(var).map(|node| node.clone())
    }

    pub fn get_real(&self, var: &str) -> Option<f64> {
        if let Some(node) = self.vars.get(var) {
            match node {
                Node::Real(val) => Some(*val),
                _ => None,
            }
        } else {
            None
        }
    }

    pub fn get_complex(&self, var: &str) -> Option<Complex> {
        if let Some(node) = self.vars.get(var) {
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
}

impl Function {
    fn eval(&self, args: &Vec<Node>) -> Result<Node, Error> {
        let val = match args[..] {
            [Node::Real(num)] => Node::Real(match self {
                Self::Sin => num.sin(),
                Self::Cos => num.cos(),
                Self::Tan => num.tan(),
                Self::ArcSin => num.asin(),
                Self::ArcCos => num.acos(),
                Self::ArcTan => num.atan(),
            }),
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
}

impl BinaryOperation {
    fn eval(self, lhs: &Node, rhs: &Node) -> Result<Node, Error> {
        Ok(match (lhs, rhs) {
            (Node::Real(lhs), Node::Real(rhs)) => 
                Node::Real(match self {
                    BinaryOperation::Add => lhs + rhs,
                    BinaryOperation::Sub => lhs - rhs,
                    BinaryOperation::Mul => lhs * rhs,
                    BinaryOperation::Div => lhs / rhs,
                    BinaryOperation::Mod => lhs.rem_euclid(*rhs),
                    BinaryOperation::Pow => lhs.powf(*rhs),
                }),
            (Node::Complex(lhs), Node::Complex(rhs)) =>
                Node::Complex(match self {
                    BinaryOperation::Add => *lhs + *rhs,
                    BinaryOperation::Sub => *lhs - *rhs,
                    BinaryOperation::Mul => *lhs * *rhs,
                    BinaryOperation::Div => *lhs / *rhs,
                    BinaryOperation::Mod => Err(Error::ParseError)?,
                    BinaryOperation::Pow => lhs.powf(*rhs),
                }),
            (Node::Real(lhs), Node::Complex(rhs)) =>
                Node::Complex(match self {
                    BinaryOperation::Add => Complex { real: *lhs, img: 0.0 } + *rhs,
                    BinaryOperation::Sub => Complex { real: *lhs, img: 0.0 } - *rhs,
                    BinaryOperation::Mul => Complex { real: *lhs, img: 0.0 } * *rhs,
                    BinaryOperation::Div => Complex { real: *lhs, img: 0.0 } / *rhs,
                    BinaryOperation::Mod => Err(Error::ParseError)?,
                    BinaryOperation::Pow => Complex { real: *lhs, img: 0.0 }.powf(*rhs),
                }),
            (Node::Complex(lhs), Node::Real(rhs)) =>
                Node::Complex(match self {
                    BinaryOperation::Add => *lhs + Complex { real: *rhs, img: 0.0 },
                    BinaryOperation::Sub => *lhs - Complex { real: *rhs, img: 0.0 },
                    BinaryOperation::Mul => *lhs * Complex { real: *rhs, img: 0.0 },
                    BinaryOperation::Div => *lhs / Complex { real: *rhs, img: 0.0 },
                    BinaryOperation::Mod => Err(Error::ParseError)?,
                    BinaryOperation::Pow => lhs.powf(Complex { real: *rhs, img: 0.0 }),
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
    fn sum(&self, vars: &Vars) -> Result<Node, Error> {
        Ok(match self {
            Node::Real(val) => Node::Real(*val),
            Node::Complex(val) => Node::Complex(*val),
            Node::PrefixOperation { op, rhs } => op.eval(&rhs.sum(vars)?)?,
            Node::BinaryOperation { lhs, op, rhs } => op.eval(&lhs.sum(vars)?, &rhs.sum(vars)?)?,
            Node::PostOperation { lhs, op } => op.eval(&lhs.sum(vars)?)?,
            Node::Function(func, args) => func.eval(&args.into_iter().map(|node| node.sum(vars)).collect::<Result<Vec<Node>, Error>>()?)?,
            Node::Var(var) => vars.get(var).ok_or(Error::ParseError)?,
            Node::Equals => todo!(),
        })
    }
}

impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
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