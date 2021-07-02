mod ast;
mod parser;

use std::fs;
use std::env;
use std::process;

fn print(arguments: Vec<ast::Value>) -> ast::Result {
    for argument in arguments {
        println!("{}", argument);
    }
    Ok(ast::Value::Nil)
}

fn main() {
    let mut files = 0;

    for (index, file_path) in env::args().enumerate() {
        if index == 0 { continue; }

        let source = fs::read_to_string(&file_path).unwrap_or_else(|error| {
            eprintln!("lust: could not read file '{}': {}", file_path, error);
            process::exit(1);
        });

        let tokens = parser::tokenize(source);

        let mut ast = ast::Ast::new(tokens);

        ast.define("print".to_string(), ast::Value::Native(print));
        ast.run();

        files += 1;
    }

    if files == 0 {
        eprintln!("error: No input files were provided");
        eprintln!("Usage: lust [FILE-1] [...]");
        process::exit(1);
    }
}
