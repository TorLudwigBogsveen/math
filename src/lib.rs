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

pub mod complex;
pub mod equation;
pub mod functions;

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
            eq.vars.set_real("x", x as f64);
            println!("{}", eq.sum().unwrap());
        }
    }

    #[test]
    fn pi() {
        let s = "pi^(i*x)";
        let mut eq = Equation::new(s);
        for x in 0..10 {
            eq.vars.set_real("x", x as f64);
            println!("{}", eq.sum().unwrap());
        }
    }

    #[test]
    fn phi() {
        let s = "round((phi^x)/sqrt(5))";
        let mut eq = Equation::new(s);
        for x in 0..100 {
            eq.vars.set_real("x", x as f64);
            println!("{}", eq.sum().unwrap());
        }
    }

    #[test]
    fn log() {
        let s = "log(x, x^10)";
        let mut eq = Equation::new(s);
        for x in 0..100 {
            eq.vars.set_real("x", x as f64);
            println!("{}", eq.sum().unwrap());
        }
    }

    #[test]
    fn min() {
        let s = "min(3*x, 100)";
        let mut eq = Equation::new(s);
        for x in 0..100 {
            eq.vars.set_real("x", x as f64);
            println!("{}", eq.sum().unwrap());
        }
    }

    #[test]
    fn max() {
        let s = "max(3*x, 100)";
        let mut eq = Equation::new(s);
        for x in 0..100 {
            eq.vars.set_real("x", x as f64);
            println!("{}", eq.sum().unwrap());
        }
    }

    #[test]
    fn abs() {
        let s = "|i + x|";
        let mut eq = Equation::new(s);
        for x in -100..100 {
            eq.vars.set_real("x", x as f64);
            println!("{}", eq.sum().unwrap());
        }
    }

    #[test]
    fn sqrt() {
        let s = "sqrt(-1+0i)";
        let mut eq = Equation::new(s);
        for x in -100..100 {
            eq.vars.set_real("x", x as f64);
            println!("{}", eq.sum().unwrap());
        }
    }

    #[test]
    fn decimal() {
        let s = "1.3";
        let mut eq = Equation::new(s);
        println!("{}", eq.sum().unwrap());
    }

    #[test]
    fn equal() {
        let s = "0.5 * 2 = 1.0";
        let mut eq = Equation::new(s);
        println!("{}", eq.sum().unwrap());
    }

    #[test]
    fn and() {
        let s = "1 > 2 and 2 = 2";
        let mut eq = Equation::new(s);
        println!("{}", eq.sum().unwrap());
    }

    #[test]
    fn test() {
        let s = "a and b";
        let mut eq = Equation::new(s);
        println!("{:?}", eq);
    }
}