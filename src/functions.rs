
/*
 *   Copyright (c) 2023 Ludwig Bogsveen
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

use crate::equation::{Vars, Node, Error};

pub fn factorial(nth: usize) -> usize {
    if nth == 0 { 1 }
    else { nth * factorial(nth-1) }
}

pub fn differentiate(vars: &mut Vars, node: &Node, var: &str, x: f64) -> Result<f64, Error> {
    vars.set_real(var, x);
    let a = match node.sum(vars)? {
        Node::Real(num) => num,
        _ => Err(Error::ParseError)?
    };
    vars.set_real(var, x+0.001);
    let b = match node.sum(vars)? {
        Node::Real(num) => num,
        _ => Err(Error::ParseError)?
    };
    Ok((b-a)/0.001)
}

pub fn equal_f64(a: f64, b: f64) -> bool {
    let offset = 0.003;
    let val = a - b;
    val < offset && val > -offset
}