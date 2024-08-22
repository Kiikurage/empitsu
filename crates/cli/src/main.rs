// use clap::Parser;
// use core::vm::VM;
// use std::path::PathBuf;
// 
// #[derive(Parser)]
// struct Cli {
//     /// The path to the file to run
//     path: PathBuf,
// }
// 
// fn main() {
//     let args = Cli::parse();
//     let source = match std::fs::read_to_string(&args.path) {
//         Ok(source) => source,
//         Err(err) => {
//             eprintln!("Error reading file: {}", err);
//             return;
//         }
//     };
// 
//     VM::new().eval(&source);
//     // let mut vm = VM::new();
//     // let mut buffer = String::new();
//     //
//     // for line in io::stdin().lines() {
//     //     buffer.push_str(&line.unwrap());
//     //     if parse(&buffer).is_err() {
//     //         continue;
//     //     }
//     //
//     //     let result = vm.eval(&buffer);
//     //     buffer.clear();
//     //
//     //     println!(">> {:?}", result);
//     // }
// }
