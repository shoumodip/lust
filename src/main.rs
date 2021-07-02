mod ast;
mod parser;

use std::fs;

fn print(arguments: Vec<ast::Value>) -> ast::Result {
    for argument in arguments {
        println!("{}", argument);
    }
    Ok(ast::Value::Nil)
}

fn main() {
    let source = fs::read_to_string("lexer.scm").unwrap();
    let tokens = parser::tokenize(source);

    let mut ast = ast::Ast::new(tokens);

    ast.define("print".to_string(), ast::Value::Native(print));
    ast.run();
}
