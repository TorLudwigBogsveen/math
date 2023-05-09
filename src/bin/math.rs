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

use std::{time::SystemTime, ops::{Mul, Div}};
use math::{graph::Graph, equation::Equation};
use minifb::{Window, WindowOptions, Key};
 

 
const WIDTH: usize = 600;
const HEIGHT: usize = 400;

fn diff<F: Fn(f64) -> f64 + 'static>(fun: F) -> Box<dyn Fn(f64) -> f64> {
    let diff = move |x| -> f64 {
        let a = fun(x);
        let b = fun(x+0.001);
        (b-a)/0.001
    };

    Box::new(diff)
}

fn main() {
    //println!("{}", Equation::new("2^(1/2)").sum());
    let time = SystemTime::now();

    let mut window = Window::new(
        "Test - ESC to exit",
        WIDTH,
        HEIGHT,
        WindowOptions::default(),
    )
    .unwrap_or_else(|e| {
        panic!("{}", e);
    });
    
    // Limit to max ~60 fps update rate
    window.limit_update_rate(Some(std::time::Duration::from_micros(16600)));

    

    let mut graph = Graph::new(WIDTH, HEIGHT);
    
    let s = "sin(x)";
    let mut eq = Equation::new(s);

    while window.is_open() && !window.is_key_down(Key::Escape) {
        graph.set_view(20.0, 20.0);
        graph.set_origin(0.0, 0.0);
        if window.is_key_down(Key::D) {
            graph.view.0 *= 1.01;
        }
        if window.is_key_down(Key::A) {
            graph.view.0 *= 1.0/1.01;
        }
        if window.is_key_down(Key::W) {
            graph.view.1 *= 1.01;
        }
        if window.is_key_down(Key::S) {
            graph.view.1 *= 1.0/1.01;
        }
        graph.clear();
        
        //graph.draw_x(&mut eq);
        //graph.draw_x(&mut |x|x*x + 4.0*x - 8.0);
        //graph.draw_x(&|x|x*x);
        //graph.draw_argb(&|x, y| mandelbrot_color(x, y));
        //graph.draw_x(&|x| (x / ((time.elapsed().unwrap().as_secs_f64()/1.0).sin()*10.0+15.0)).sin() * 100.0 + 200.0);
        //graph.draw_y(&|x| (x / ((time.elapsed().unwrap().as_secs_f64()/1.0).sin()*10.0+15.0)).sin() * 100.0 + 200.0);
        //graph.draw_x(&|x| (x/100.0).sin()*100.0 + 150.0);
        //graph.draw_x(&|x| (x).cos());
        /*graph.draw_x(&|x| (x).sin());
        graph.draw_x(&|x| diff(|x| (x).sin())(x));
        graph.draw_x(&|x| diff(diff(|x| (x).sin()))(x));
        graph.draw_x(&|x| diff(diff(diff(|x| (x).sin())))(x));
        graph.draw_x(&|x| diff(diff(diff(diff(|x| (x).sin()))))(x));*/
        /*graph.draw_x(&|x| 3.0*x*x-4.0*x*x*x+x-1.0);
        graph.draw_x(&|x| diff(|x| 3.0*x*x-4.0*x*x*x+x-1.0)(x));
        graph.draw_x(&|x| diff(diff(|x| 3.0*x*x-4.0*x*x*x+x-1.0))(x));
        graph.draw_x(&|x| diff(diff(diff(|x| 3.0*x*x-4.0*x*x*x+x-1.0)))(x));*/
        //graph.draw(&|x, y| ((x / ((time.elapsed().unwrap().as_secs_f64()/1.0).sin()*10.0+15.0)).sin() * 100.0 + 200.0 - y).abs() < 2.0);
        graph.draw(&mut |x, y| (x-100.0).powi(2) + (y-100.0).powi(2) < 2000.0);
        //graph.draw(&|x, y| mandelbrot(x, y) == 100);
        window
        .update_with_buffer(&graph.pixels(), WIDTH, HEIGHT)
        .unwrap();
        
    }
}

