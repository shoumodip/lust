use crate::ast::{Ast, Result, Value};

macro_rules! arith_condition {
    ($predicate: expr) => {{
        |arguments: Vec<Value>| -> Result {
            use Value::*;

            let mut previous = 0.0;
            let mut previous_set = false;

            for argument in arguments {
                match argument {
                    Number(n) => {
                        if previous_set {
                            if !$predicate(previous, n) { return Ok(Boolean(false)) }
                        } else {
                            previous_set = true;
                        }  
                        
                        previous = n;
                    },
                    invalid => return Err(format!("invalid number '{}'", invalid))
                }
            }

            Ok(Boolean(true))
        }
    }};
}

macro_rules! arith_operation {
    ($initial: expr, $operator: expr) => {{
        |arguments: Vec<Value>| -> Result {
            use Value::*;

            let mut result = 0.0;
            let mut result_set = false;

            for argument in &arguments {
                match argument {
                    Number(n) => if result_set {
                        result = $operator(result, n);
                    } else {
                        result = *n;
                        result_set = true;
                    },
                    invalid => return Err(format!("invalid number '{}'", invalid))
                }
            }

            if arguments.len() == 1 {
                result = $operator($initial, result);
            }

            Ok(Number(result))
        }
    }};
}

fn print(arguments: Vec<Value>) -> Result {
    for argument in arguments {
        println!("{}", argument);
    }
    Ok(Value::Nil)
}

pub fn load(ast: &mut Ast) {
    ast.define("print".to_string(), Value::Native(print));
    
    ast.define("<".to_string(), Value::Native(arith_condition!(|a, b| a < b)));
    ast.define("<=".to_string(), Value::Native(arith_condition!(|a, b| a <= b)));
    ast.define(">".to_string(), Value::Native(arith_condition!(|a, b| a > b)));
    ast.define(">=".to_string(), Value::Native(arith_condition!(|a, b| a >= b)));
    ast.define("=".to_string(), Value::Native(arith_condition!(|a, b| a == b)));
    ast.define("!=".to_string(), Value::Native(arith_condition!(|a, b| a != b)));

    ast.define("+".to_string(), Value::Native(arith_operation!(0.0, |a, b| a + b)));
    ast.define("-".to_string(), Value::Native(arith_operation!(0.0, |a, b| a - b)));
    ast.define("*".to_string(), Value::Native(arith_operation!(1.0, |a, b| a * b)));
    ast.define("/".to_string(), Value::Native(arith_operation!(1.0, |a, b| a / b)));
}
