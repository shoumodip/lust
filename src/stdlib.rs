use crate::ast::{self, Ast, Result, Value};
use crate::parser;

macro_rules! equality {
    ($predicate: expr) => {{
        |arguments: Vec<Value>| -> Result {
            let mut previous = Nil;
            let mut previous_set = false;

            for value in arguments {
                if previous_set {
                    if !$predicate(previous, value.clone()) { return Ok(Boolean(false)); }
                } else {
                    previous_set = true;
                }

                previous = value;
            }

            Ok(Boolean(true))
        }
    }};
}

macro_rules! bool_unary {
    ($name: expr, $predicate: expr) => {{
        |arguments: Vec<Value>| -> Result {
            if arguments.len() == 1 {
                Ok(Boolean($predicate(arguments[0].clone())))
            } else {
                Err(format!("function '{}' takes 1 parameter(s), found {} instead",
                            $name,
                            arguments.len()))
            }
        }
    }};
}

macro_rules! bool_condition {
    ($short_circuit: expr, $predicate: expr) => {{
        |arguments: Vec<Value>| -> Result {
            let mut previous = true;
            let mut previous_set = false;

            for value in arguments {
                let new = ast::is_true(&value);
                if previous_set {
                    if $predicate(previous, new) == $short_circuit { return Ok(Boolean($short_circuit)); }
                } else {
                    previous_set = true;
                }
                previous = new;
            }

            Ok(Boolean(true))
        }
    }};
}

macro_rules! arith_condition {
    ($predicate: expr) => {{
        |arguments: Vec<Value>| -> Result {
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
    ($ast: expr, $name: expr, $initial: expr, $operator: expr) => {{
        $ast.define(&format!("{}", $name), Native(
            |arguments: Vec<Value>| -> Result {
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
        ));

        let mutate = $ast.run(parser::tokenize(format!(
        "(macro (symbol value)
          (eval
            `(set ,symbol ({} ,symbol ,value))))", $name)));

        $ast.define(&format!("{}=", $name), mutate.expect("100% rust bug not mine"));
    }};
}

fn print(arguments: Vec<Value>) -> Result {
    for argument in arguments {
        println!("{}", argument);
    }
    Ok(Value::Nil)
}

fn cons(arguments: Vec<Value>) -> Result {
    let mut result = vec![];

    use Value::*;
    for value in arguments {
        match value {
            List(l) => result.extend(l),
            value => result.push(value)
        }
    }

    Ok(List(result))
}

fn car(arguments: Vec<Value>) -> Result {
    use Value::*;
    match &arguments[0] {
        List(l) => Ok(l[0].clone()),
        Nil => Ok(Nil),
        invalid => Err(format!("invalid list '{}'", invalid))
    }
}

fn cdr(arguments: Vec<Value>) -> Result {
    use Value::*;
    match &arguments[0] {
        List(l) => if l.len() > 1 {
            Ok(List(l[1..].to_vec()))
        } else {
            Ok(Nil)
        },
        Nil => Ok(Nil),
        invalid => Err(format!("invalid list '{}'", invalid))
    }
}

fn nth(arguments: Vec<Value>) -> Result {
    use Value::*;

    if arguments.len() == 2 {
        match &arguments[1] {
            List(l) => match &arguments[0] {
                Number(i) if *i >= 0.0 && *i as usize <= l.len() => Ok(l[*i as usize].clone()),
                invalid => Err(format!("invalid index '{}'", invalid))
            },

            String(s) => match &arguments[0] {
                Number(i) if *i >= 0.0 && *i as usize <= s.len() => Ok(String(s
                                                                              .chars()
                                                                              .nth(*i as usize)
                                                                              .unwrap()
                                                                              .to_string())),
                invalid => Err(format!("invalid index '{}'", invalid))
            },
            invalid => Err(format!("invalid list '{}'", invalid))
        }
    } else {
        Err(format!("function 'nth' takes 2 parameter(s), found {} instead", arguments.len()))
    }
}

fn slice(arguments: Vec<Value>) -> Result {
    use Value::*;

    let length = arguments.len();

    if length < 1 || length > 3 {
        Err(format!("function 'slice' takes 1, 2 or 3 parameters, found {} instead", length))
    } else {
        let mut low = 0;
        let mut high;

        match &arguments[0] {
            String(s) => high = s.len(),
            List(l) => high = l.len(),
            invalid => return Err(format!("invalid sequence '{}'", invalid))
        }

        if arguments.len() > 1 {
            match &arguments[1] {
                Number(n) if *n as usize <= high => {low = *n as usize;}
                invalid => return Err(format!("invalid index '{}'", invalid))
            }
        }

        if arguments.len() > 2 {
            match &arguments[2] {
                Number(n) if *n as usize <= high => {high = *n as usize;}
                invalid => return Err(format!("invalid index '{}'", invalid))
            }
        }

        match &arguments[0] {
            String(s) => Ok(String(s[low..high].to_string())),
            List(l) => Ok(List(l[low..high].to_vec())),
            invalid => Err(format!("invalid sequence '{}'", invalid))
        }
    }
}

fn length(arguments: Vec<Value>) -> Result {
    use Value::*;
    match &arguments[0] {
        String(s) => Ok(Number(s.len() as f64)),
        List(l) => Ok(Number(l.len() as f64)),
        _ => Ok(Nil)
    }
}

fn concat(arguments: Vec<Value>) -> Result {
    let mut result = String::new();

    for value in arguments {
        result.push_str(&format!("{}", value));
    }

    Ok(Value::String(result))
}

fn range(arguments: Vec<Value>) -> Result {
    use Value::*;

    let mut low = Number(0.0);
    let high;

    match arguments.len() {
        0 => return Err("atleast upper limit must be provided to function 'range'".to_string()),
        1 => high = arguments[0].clone(),
        2 => {
            low = arguments[0].clone();
            high = arguments[1].clone();
        }
        n => return Err(format!("function 'range' takes 1 or 2 parameters, found '{}' instead", n))
    }

    match (&low, &high) {
        (Number(low), Number(high)) => {
            let result = (*low as i64..*high as i64).map(|n| Number(n as f64)).collect();
            Ok(List(result))
        }
        _ => Err(format!("invalid limits '{}' and '{}' provided to function 'range'",
                         low,
                         high))
    }
}

pub fn load(ast: &mut Ast) {
    use Value::*;

    // Ad-hoc primitives
    ast.define("car", Native(car));
    ast.define("cdr", Native(cdr));
    ast.define("nth", Native(nth));
    ast.define("cons", Native(cons));
    ast.define("append", Native(cons));
    ast.define("print", Native(print));
    ast.define("slice", Native(slice));

    // Boolean conditions
    ast.define("and", Native(bool_condition!(false, |a, b| a && b)));
    ast.define("or", Native(bool_condition!(true, |a, b| a || b)));
    ast.define("not", Native(bool_unary!("not", |v| !ast::is_true(&v))));

    // Equality
    ast.define("=", Native(equality!(|a, b| a == b)));
    ast.define("!=", Native(equality!(|a, b| a != b)));

    // Types
    ast.define("nil?", Native(bool_unary!("nil?", |v| ast::is_nil(&v))));
    ast.define("symbol?", Native(bool_unary!("symbol?", |v| if let Symbol(_) = v {true} else {false})));
    ast.define("string?", Native(bool_unary!("string?", |v| if let String(_) = v {true} else {false})));
    ast.define("number?", Native(bool_unary!("number?", |v| if let Number(_) = v {true} else {false})));
    ast.define("bool?", Native(bool_unary!("bool?", |v| if let Boolean(_) = v {true} else {false})));
    ast.define("list?", Native(bool_unary!("list?", |v| if let List(_) = v {true} else {false})));
    
    // Arithmetic conditions
    ast.define("<", Native(arith_condition!(|a, b| a < b)));
    ast.define("<=", Native(arith_condition!(|a, b| a <= b)));
    ast.define(">", Native(arith_condition!(|a, b| a > b)));
    ast.define(">=", Native(arith_condition!(|a, b| a >= b)));

    // Arithmetic operations
    arith_operation!(ast, "+", 0.0, |a, b| a + b);
    arith_operation!(ast, "-", 0.0, |a, b| a - b);
    arith_operation!(ast, "*", 1.0, |a, b| a * b);
    arith_operation!(ast, "/", 1.0, |a, b| a / b);
    arith_operation!(ast, "%", 0.0, |a, b| a % b);

    // QoL functions
    ast.define("length", Native(length));
    ast.define("concat", Native(concat));
    ast.define("range", Native(range));

    ast.run(parser::tokenize("
(let even
  (lambda (number)
    (= (% number 2) 0)))

(let even
  (lambda (number)
    (= (% number 2) 1)))

(let map
  (lambda (function list)
    (let ((result '()))
        (dolist (i list)
          (set result (cons result (function i))))
        result)))

(let filter
  (lambda (predicate list)
    (let ((result '()))
      (dolist (i list)
        (when (predicate i)
          (set result (cons result i))))
      result)))

(let set-nth
  (macro
     (list index value)
     (eval
      `(set ,list (cons
                   (slice ,list 0 ,index)
                   ,value
                   (slice ,list ,(+ index 1)))))))

".to_string())).expect("100% rust bug not mine");
}
