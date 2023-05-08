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

use std::{ops::{Mul, Div, Sub, Add, DivAssign, MulAssign, SubAssign, AddAssign, Deref}, fmt::Display, f64::consts::E};

#[derive(Debug, Copy, Clone)]
pub struct Complex {
    pub real: f64,
    pub img: f64,
}

impl Complex {
    pub fn con(&self) -> Self {
        Complex { real: self.real, img: -self.img }
    }

    pub fn abs(&self) -> f64 {
        (self.real*self.real+self.img*self.img).sqrt()
    }

    pub fn arg(&self) -> f64 {
        (self.img).atan2(self.real)
    }

    pub fn magsq(&self) -> f64 {
        self.real*self.real + self.img*self.img
    }

    pub fn mag(&self) -> f64 {
        self.magsq().sqrt()
    }

    pub fn powf(&self, rhs: Self) -> Complex {
        let a = self.abs().powf(rhs.real)/E.powf(self.arg()*rhs.img);
        let theta = self.arg()*rhs.real+self.abs().ln()*rhs.img;
        let c = Complex {real: theta.cos(), img: theta.sin()};
        c * Complex { real: a, img: 0.0 }
    }
}

impl Display for Complex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.real == 0.0 && self.img == 0.0 {
            write!(f, "0")
        } else if self.real != 0.0 && self.img == 0.0 {
            write!(f, "{}", self.real)
        } else if self.real == 0.0 && self.img != 0.0 {
            write!(f, "{}i", self.img)
        } else {
            let sign = match self.img >= 0.0 {
                true => '+',
                false => '-'
            };

            write!(f, "{} {} {}i", self.real, sign, self.img.abs())
        }
    }
}

impl AddAssign for Complex {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}

impl SubAssign for Complex {
    fn sub_assign(&mut self, rhs: Self) {
        *self = *self - rhs;
    }
}

impl MulAssign for Complex {
    fn mul_assign(&mut self, rhs: Self) {
        *self = *self * rhs;
    }
}

impl DivAssign for Complex {
    fn div_assign(&mut self, rhs: Self) {
        *self = *self / rhs;
    }
}

impl Add for Complex {
    type Output = Self;

    fn add(self, rhs: Self) -> Self {
        let real = self.real + rhs.real;
        let img = self.img + rhs.img;
        Complex { real, img }
    }
}

impl Sub for Complex {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self {
        let real = self.real - rhs.real;
        let img = self.img - rhs.img;
        Complex { real, img }
    }
}

impl Mul for Complex {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self {
        let real = self.real * rhs.real - self.img * rhs.img;
        let img = self.img * rhs.real + self.real * rhs.img;
        Complex { real, img }
    }
}

impl Div for Complex {
    type Output = Self;

    fn div(self, rhs: Self) -> Self {
        let con = rhs.con();
        let a = self * rhs;
        let b = rhs * con;
        let real = if b.real == 0.0 {
            0.0
        } else {
            a.real / b.real
        };
        let img = if b.img == 0.0 {
            0.0
        } else {
            a.img / b.img
        };

        Complex { real: real, img: img }
    }
}
