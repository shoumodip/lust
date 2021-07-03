mod ast;
mod parser;

use std::io::{self, Write};
use std::fs;
use std::env;
use std::process;

fn print(arguments: Vec<ast::Value>) -> ast::Result {
    for argument in arguments {
        println!("{}", argument);
    }
    Ok(ast::Value::Nil)
}

fn run_file(file_path: &str) {
    let source = fs::read_to_string(file_path).unwrap_or_else(|error| {
        eprintln!("lust: could not read file '{}': {}", file_path, error);
        process::exit(1);
    });

    let mut ast = ast::Ast::new();
    ast.define("print".to_string(), ast::Value::Native(print));

    let tokens = parser::tokenize(source);
    match ast.run(tokens) {
        Some(_) => {},
        None => process::exit(1)
    }
}

fn repl() {
    let mut buffer;
    let mut ast = ast::Ast::new();

    ast.define("print".to_string(), ast::Value::Native(print));

    loop {
        print!("> ");
        buffer = String::new();

        io::stdout().flush().expect("failed to flush stdout");
        io::stdin().read_line(&mut buffer).expect("failed to read from stdin");

        match buffer.len() {
            0 => break,
            1 => continue,
            _ => match ast.run(parser::tokenize(buffer)) {
                Some(value) => println!("{}", value),
                None => {}
            },
        }
    }

    println!();
}

fn main() {
    let mut files = 0;

    for (index, file_path) in env::args().enumerate() {
        if index == 0 { continue; }
        run_file(&file_path);
        files += 1;
    }

    if files == 0 {
        repl();
    }
}
