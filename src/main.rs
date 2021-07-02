mod ast;

use ast::Value;
use ast::Ast;

#[allow(dead_code)]
fn greet(name: Vec<Value>) -> ast::Result {
    println!("Hello, {}!", name[0]);
    Ok(Value::Nil)
}

fn main() {
    let source = vec![
        Value::List(vec![
            Value::Lambda(vec!["name".to_string()], vec![
                Value::List(vec![
                    Value::Symbol("greet".to_string()),
                    Value::Symbol("name".to_string())
                ])
            ]),
            Value::String("world".to_string()),
        ])
    ];

    let mut ast = Ast::new(source);
    ast.define("greet".to_string(), Value::Native(greet));

    ast.run();
}
