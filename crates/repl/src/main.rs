use std::io;

use core::parser::parse;
use core::vm::VM;

fn main() {
    let mut vm = VM::new();
    let mut buffer = String::new();

    for line in io::stdin().lines() {
        buffer.push_str(&line.unwrap());
        if parse(&buffer).is_err() {
            continue;
        }

        let result = vm.eval(&buffer);
        buffer.clear();

        println!(">> {:?}", result);
    }
}
