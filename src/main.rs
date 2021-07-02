mod ast;
mod lexer;

use std::fs;

use ast::{Value, Ast};
use lexer::Lexer;

fn greet(name: Vec<Value>) -> ast::Result {
    println!("Hello, {}!", name[0]);
    Ok(Value::Nil)
}

fn main() {
    let source = fs::read_to_string("lexer.scm").unwrap();
    let tokens = Lexer::tokenize(source);

    let mut ast = Ast::new(tokens);

    ast.define("greet".to_string(), Value::Native(greet));
    ast.run();
}
