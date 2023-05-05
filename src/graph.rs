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

use crate::{WIDTH, HEIGHT, complex::Complex};

const FACTOR : usize = 100;

pub struct Graph {
    pixels: Vec<u32>,
    width: usize,
    height: usize,
    origin: (f64, f64),
    pub view: (f64, f64),
}

impl Graph {
    pub fn new(width: usize, height: usize) -> Graph {
        Graph { pixels: vec![0; width*height], width, height, origin: (width as f64 / 2.0, height as f64 / 2.0), view: (2.0, 2.0) }
    }

    pub fn set_origin(&mut self, x: f64, y: f64) {
        self.origin = (self.width as f64 / 2.0+x, self.height as f64 / 2.0+y*self.height as f64);
    }

    pub fn set_view(&mut self, width: f64, height: f64) {
        self.view = (width, height);
    }

    pub fn clear(&mut self) {
        self.pixels.fill(0);
    }

    pub fn draw_x(&mut self, f: &dyn Fn(f64) -> f64) {
        for x in 0..(self.width*FACTOR) {
            let transformed_x = (x as f64 / FACTOR as f64 - self.origin.0) / WIDTH as f64 * self.view.0;
            let y = (f)(transformed_x) / self.view.1 * HEIGHT as f64 + self.origin.1;
            self.plot((x / FACTOR) as isize, y as isize, 0xFF_FF_FF_FF);
        }
    }

    pub fn draw_y(&mut self, f: &dyn Fn(f64) -> f64) {
        for y in 0..(self.height*FACTOR) {
            let x = (f)(y as f64 / FACTOR as f64);
            self.plot(x as isize, (y / FACTOR) as isize, 0xFF_FF_FF_FF);
        }
    }

    pub fn draw(&mut self, f: &dyn Fn(f64, f64) -> bool) {
        for y in 0..self.height {
            for x in 0..self.width {
                if f(x as f64, y as f64) {
                    self.plot(x as isize, y as isize, 0xFF_FF_FF_FF);
                }
            }
        }
    }

    pub fn draw_argb(&mut self, f: &dyn Fn(f64, f64) -> u32) {
        for y in 0..self.height {
            for x in 0..self.width {
                self.plot(x as isize, y as isize, f(x as f64, y as f64));
            }
        }
    }

    pub fn plot(&mut self, x: isize, y: isize, color: u32) {
        if x >= self.width as isize || y >= self.height as isize || x < 0 || y < 0 {
            //println!("Tried to plot outside canvas: {}, {}", x, y);
            return;
        }

        self.pixels[x as usize + (self.height - 1 - y as usize) * self.width] = color;
    }

    pub fn pixels(&self) -> &[u32] {
        &self.pixels[..]
    }
}

pub fn mandelbrot_color(x: f64, y: f64) -> u32 {
    let iter = mandelbrot(x, y) as f64;
    let a = 0.01;

    let r = (((iter * a + 0.0).sin() + 1.0) / 2.0) * 255.0;
    let g = (((iter * a + 0.4).sin() + 1.0) / 2.0) * 255.0;
    let b = (((iter * a + 1.0).sin() + 1.0) / 2.0) * 255.0;

    //println!("{}", r);

	let color = ((r as u32) << 16) + ((g as u32) << 8) + (b as u32);
    color
}

pub fn mandelbrot(x: f64, y: f64) -> i32 {
    let xoff: f64 = -2.5;
    let yoff: f64 = -1.0;
    let width: f64 = 3.5;
    let height: f64 = 2.0;
    
    let pos = Complex {real: x, img: y};

    let x0 = (pos.real / WIDTH as f64) * width + xoff;
    let y0 = (1.0 - (pos.img / HEIGHT as f64)) * height + yoff;
    let mut iteration = 0;
    let max_iteration = 100;

    let mut z = Complex { real: 0.0, img: 0.0 };
    let c = Complex {real: x0, img: y0};

    while z.magsq() <= 2.0*2.0 && iteration < max_iteration {
        z *= z;
        z += c;

        iteration = iteration + 1;
    }

    iteration
}

