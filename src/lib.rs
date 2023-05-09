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

#![feature(unboxed_closures)]
#![feature(fn_traits)]

use std::{time::SystemTime};

use graph::{Graph};
use minifb::{Window, WindowOptions, Key};

pub mod complex;
pub mod equation;
pub mod graph;
pub mod functions;

const WIDTH: usize = 600;
const HEIGHT: usize = 400;

#[cfg(test)]
mod tests {
    use crate::equation::Equation;

    #[test]
    fn math() {
        let s = "5 + 7 * 3";
        let eq = Equation::new(s);
        println!("{s} = {}", eq.sum().unwrap());
    }

    #[test]
    fn sin() {
        let s = "sin(0) + cos(3)";
        let eq = Equation::new(s);
        println!("{s} = {}", eq.sum().unwrap());
    }

    #[test]
    fn moodulo() {
        let s = "5 % 3";
        let eq = Equation::new(s);
        println!("{s} = {}", eq.sum().unwrap());
    }

    #[test]
    fn var() {
        let s = "1^2+4*x-8";
        let mut eq = Equation::new(s);
        for x in 0..10 {
            eq.vars.set_real(String::from("x"), x as f64);
            println!("{}", eq.sum().unwrap());
        }
    }
}