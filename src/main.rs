mod ast;
mod parser;
mod stdlib;

use std::io::{self, Write};
use std::fs;
use std::env;
use std::process;
use ast::{Ast, Value};

fn run_file(path: &str, arguments: Vec<Value>) {
    let source = fs::read_to_string(path).unwrap_or_else(|_| {
        eprintln!("lust: could not read file '{}'", path);
        process::exit(1);
    });

    let mut ast = Ast::new();
    stdlib::load(&mut ast, arguments);

    let tokens = parser::tokenize(source);

    match tokens {
        Ok(tokens) => if let Some(_) = ast.run(tokens) { return },
        Err(message) => eprintln!("error: {}", message)
    }

    process::exit(1)
}

fn repl() {
    let mut buffer;
    let mut ast = Ast::new();

    stdlib::load(&mut ast, vec![]);

    loop {
        print!("> ");
        buffer = String::new();

        io::stdout().flush().expect("failed to flush stdout");
        io::stdin().read_line(&mut buffer).expect("failed to read from stdin");

        match buffer.len() {
            0 => break,
            1 => continue,
            _ => {
                let tokens = parser::tokenize(buffer);

                match tokens {
                    Ok(tokens) => match ast.run(tokens) {
                        Some(Value::String(string)) => println!("\"{}\"", string),
                        Some(value) => println!("{}", value),
                        None => {}
                    },
                    Err(message) => eprintln!("error: {}", message)

                }
            }
        }
    }

    println!();
}

fn main() {
    let arguments = env::args().collect::<Vec<String>>();

    if arguments.len() == 1 {
        repl();
    } else {
        let path = &arguments[1];
        let args = arguments[2..]
            .iter()
            .map(|s| Value::String(s.to_string()))
            .collect();

        run_file(path, args);
    }
}
