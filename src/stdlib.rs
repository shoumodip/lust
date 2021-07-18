use crate::ast::{self, Ast, Result, Value};
use crate::parser;
use std::io::{self, Write};
use std::fs;

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
            `(set ,symbol ({} ,symbol ,value))))", $name)).expect("100% rust bug not mine"));

        $ast.define(&format!("{}=", $name), mutate.expect("100% rust bug not mine"));
    }};
}

fn print(arguments: Vec<Value>) -> Result {
    for argument in arguments {
        println!("{}", argument);
    }
    Ok(Value::Nil)
}

fn read(arguments: Vec<Value>) -> Result {
    if arguments.len() > 0 {
        print!("{}", arguments[0]);
    }

    let mut input = String::new();

    io::stdout().flush().expect("failed to flush stdout");
    io::stdin().read_line(&mut input).expect("failed to read from stdin");

    input.pop();
    Ok(Value::String(input))
}

fn open(arguments: Vec<Value>) -> Result {
    use Value::*;

    if arguments.len() == 1 {
        match &arguments[0] {
            String(path) | Symbol(path) => match fs::read_to_string(path) {
                Ok(contents) => Ok(String(contents)),
                Err(message) => Err(format!("could not read file '{}': {}", path, message))
            },
            invalid => Err(format!("invalid file path '{}'", invalid))
        }
    } else {
        Err(format!("function 'open' takes 1 parameter(s), found {} instead", arguments.len()))
    }
}

fn write(arguments: Vec<Value>) -> Result {
    use Value::*;

    if arguments.len() == 2 {
        match &arguments[1] {
            String(path) | Symbol(path) => {
                match fs::write(path, arguments[0].to_string()) {
                    Ok(_) => Ok(Nil),
                    Err(message) => Err(format!("could not write to file '{}': {}", path, message))
                }
            },
            invalid => Err(format!("invalid file path '{}'", invalid))
        }
    } else {
        Err(format!("function 'write' takes 2 parameter(s), found {} instead", arguments.len()))
    }
}

fn cons(arguments: Vec<Value>) -> Result {
    use Value::*;

    let mut result = vec![];
    for value in arguments {
        match value {
            List(l) => result.extend(l),
            value => result.push(value)
        }
    }

    Ok(List(result))
}

fn cons_bang(arguments: Vec<Value>) -> Result {
    if arguments.len() < 1 {
        Err("variadic lambda '{}' takes at least 1 parameter(s), found 0 instead".to_string())
    } else {
        use Value::*;

        let mut result = vec![];
        let mut index = 0;
        let push_index;

        match &arguments[0] {
            Number(n) if n >= &0.0 => push_index = *n as usize,
            invalid => return Err(format!("invalid index '{}'", invalid))
        }

        for value in &arguments[1..] {
            match value {
                List(l) if index != push_index => if l.len() != 0 { result.extend(l.clone()) },
                value => result.push(value.clone())
            }

            index += 1;
        }

        Ok(List(result))
    }
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
        let mut tree = vec![];

        match &arguments[1] {
            List(list) => match &arguments[0] {
                List(l) => for node in l {
                    match node {
                        Number(n) if *n >= 0.0 && (*n as usize) < list.len() => tree.push(*n as usize),
                        invalid => return Err(format!("invalid list position '{}'", invalid)),
                    }
                },
                Number(n) if *n >= 0.0 && (*n as usize) < list.len() => tree.push(*n as usize),
                invalid => return Err(format!("invalid index '{}'", invalid))
            },
            invalid => return Err(format!("invalid list '{}'", invalid))
        }

        let mut target = &arguments[1];

        for i in 0..tree.len() {
            match target {
                List(list) => {
                    let index = tree[i];
                    if index < list.len() {
                        target = &list[index];
                    } else {
                        return Err(format!("invalid index '{}'", index));
                    }
                },
                invalid => return Err(format!("invalid list '{}'", invalid)),
            }
        }

        Ok(target.clone())
    } else {
        Err(format!("function 'elt' takes 2 parameter(s), found {} instead", arguments.len()))
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
            String(s) | Symbol(s) => high = s.len(),
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
            String(s) | Symbol(s) => Ok(String(s[low..high].to_string())),
            List(l) => Ok(List(l[low..high].to_vec())),
            invalid => Err(format!("invalid sequence '{}'", invalid))
        }
    }
}

fn length(arguments: Vec<Value>) -> Result {
    use Value::*;
    match &arguments[0] {
        String(s) | Symbol(s) => Ok(Number(s.len() as f64)),
        List(l) => Ok(Number(l.len() as f64)),
        _ => Ok(Nil)
    }
}

fn reverse(arguments: Vec<Value>) -> Result {
    use Value::*;
    match &arguments[0] {
        String(s) => Ok(String(s.chars().rev().collect())),
        Symbol(s) => Ok(Symbol(s.chars().rev().collect())),
        List(l) => Ok(List(l.clone().into_iter().rev().collect())),
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

fn string2symbol(arguments: Vec<Value>) -> Result {
    use Value::*;

    if arguments.len() == 1 {
        match &arguments[0] {
            String(s) => if s.chars().any(|c| matches!(c, ' ' | '(' | ')')) {
                Err(format!("invalid symbol '{}'", s))
            } else {
                Ok(Symbol(s.clone()))
            },
            invalid => Err(format!("invalid string '{}'", invalid))
        }
    } else {
        Err(format!("function 'string->symbol' takes 1 parameter(s), found {} instead",
                    arguments.len()))
    }
}

fn symbol2string(arguments: Vec<Value>) -> Result {
    use Value::*;

    if arguments.len() == 1 {
        match &arguments[0] {
            Symbol(s) => Ok(String(s.clone())),
            invalid => Err(format!("invalid symbol '{}'", invalid))
        }
    } else {
        Err(format!("function 'symbol->string' takes 1 parameter(s), found {} instead",
                    arguments.len()))
    }
}

fn string2boolean(arguments: Vec<Value>) -> Result {
    use Value::*;

    if arguments.len() == 1 {
        match &arguments[0] {
            String(b) if b == "true" => Ok(Boolean(true)),
            String(b) if b == "false" => Ok(Boolean(false)),
            String(invalid) => Err(format!("invalid boolean '{}'", invalid)),
            invalid => Err(format!("invalid string '{}'", invalid))
        }
    } else {
        Err(format!("function 'string->boolean' takes 1 parameter(s), found {} instead",
                    arguments.len()))
    }
}

fn boolean2string(arguments: Vec<Value>) -> Result {
    use Value::*;

    if arguments.len() == 1 {
        match &arguments[0] {
            Boolean(b) => Ok(String(b.to_string())),
            invalid => Err(format!("invalid boolean '{}'", invalid))
        }
    } else {
        Err(format!("function 'boolean->string' takes 1 parameter(s), found {} instead",
                    arguments.len()))
    }
}

fn string2number(arguments: Vec<Value>) -> Result {
    use Value::*;

    if arguments.len() == 1 {
        match &arguments[0] {
            String(s) => match s.parse::<f64>() {
                Ok(number) => Ok(Number(number)),
                Err(_) => Err(format!("invalid number '{}'", s))
            },
            invalid => Err(format!("invalid string '{}'", invalid))
        }
    } else {
        Err(format!("function 'string->number' takes 1 parameter(s), found {} instead",
                    arguments.len()))
    }
}

fn number2string(arguments: Vec<Value>) -> Result {
    use Value::*;

    if arguments.len() == 1 {
        match &arguments[0] {
            Number(n) => Ok(String(n.to_string())),
            invalid => Err(format!("invalid number '{}'", invalid))
        }
    } else {
        Err(format!("function 'number->string' takes 1 parameter(s), found {} instead",
                    arguments.len()))
    }
}

fn function2list(arguments: Vec<Value>) -> Result {
    use Value::*;
    if arguments.len() == 1 {
        match &arguments[0] {
            Lambda(is_macro, is_variadic, parameters, body) => {
                let form_name = if *is_macro {"macro"} else {"lambda"};
                let mut result = vec![Symbol(form_name.to_string())];
                let mut parameters = parameters
                    .iter()
                    .map(|p| Symbol(p.clone()))
                    .collect::<Vec<Value>>();

                if *is_variadic {
                    let parameters_length = parameters.len();

                    if parameters_length == 0 {
                        return Err(format!("unnamed variadic parameter in {}", form_name));
                    }

                    let variadic_parameter = parameters[parameters_length - 1].clone();
                    parameters[parameters_length - 1] = Symbol(":rest".to_string());
                    parameters.push(variadic_parameter);
                }

                result.push(List(parameters));
                result.extend(body.clone());

                Ok(List(result))
            },
            invalid => Err(format!("invalid lambda '{}'", invalid))
        }
    } else {
        Err(format!("function 'function->list' takes 1 parameter(s), found {} instead",
                    arguments.len()))
    }
}

fn range(arguments: Vec<Value>) -> Result {
    use Value::*;

    let mut a = Number(0.0);
    let mut step = Number(0.0);
    let b;

    match arguments.len() {
        0 => return Err("atleast upper limit must be provided to function 'range'".to_string()),
        1 => b = arguments[0].clone(),
        2 | 3 => {
            a = arguments[0].clone();
            b = arguments[1].clone();

            if arguments.len() == 3 {
                step = arguments[2].clone();
            }
        }
        n => return Err(format!("function 'range' takes 1 or 2 parameters, found '{}' instead", n))
    }

    match (&a, &b, &step) {
        (Number(a), Number(b), Number(step)) => {
            let mut list = vec![];
            let mut step = *step;

            if step == 0.0 { step = if a > b { -1.0 } else { 1.0 }; }

            let mut a = *a;
            let b = *b;

            while (step > 0.0 && a < b) || (step < 0.0 && a > b) {
                list.push(Number(a));
                a += step;
            }

            Ok(List(list))
        }
        (Number(_), Number(_), invalid) =>
            Err(format!("invalid step '{}' provided to function 'range'", invalid)),
        (Number(_), invalid, _) | (invalid, _, _) =>
            Err(format!("invalid limit '{}' provided to function 'range'", invalid)),
    }
}

pub fn load(ast: &mut Ast) {
    use Value::*;

    // Ad-hoc primitives
    ast.define("car", Native(car));
    ast.define("cdr", Native(cdr));
    ast.define("nth", Native(nth));
    ast.define("cons", Native(cons));
    ast.define("cons!", Native(cons_bang));
    ast.define("print", Native(print));
    ast.define("read", Native(read));
    ast.define("open", Native(open));
    ast.define("write", Native(write));
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

    ast.define("string->symbol", Native(string2symbol));
    ast.define("symbol->string", Native(symbol2string));
    ast.define("boolean->string", Native(boolean2string));
    ast.define("string->boolean", Native(string2boolean));
    ast.define("number->string", Native(number2string));
    ast.define("string->number", Native(string2number));
    ast.define("function->list", Native(function2list));

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
    ast.define("reverse", Native(reverse));
    ast.define("range", Native(range));

    ast.run(parser::tokenize("
(let defun (macro
            (name arguments :rest body)
            `(let ,name (lambda ,arguments
                          ,@body)))

     defmacro (macro
               (name arguments :rest body)
               `(let ,name (macro ,arguments
                                  ,@body)))

     defvar (macro
             (name value)
             `(let ,name ,value)))

(defun even? (number)
  (= (% number 2) 0))

(defun odd? (number)
  (= (% number 2) 1))

(defun map (function list)
  (let ((result '()))
    (dolist (i list)
      (set result (cons result (function i))))
    result))

(defun filter (predicate list)
  (let ((result '()))
    (dolist (i list)
      (when (predicate i)
        (set result (cons result i))))
    result))

(defun foldl (function value sequence)
  (dolist (i sequence)
    (set value (function value i)))
  value)

(defun foldr (function value sequence)
  (dolist (i (reverse sequence))
    (set value (function value i)))
  value)

(defmacro ns (name :rest bindings)
  (eval `(let ,name '()))
  (dolist (binding bindings)
    (set (binding 1)
         (string->symbol
          (concat name \"/\" (nth 1 binding))))
    (eval `(let ,name (cons ,name (nth 1 binding))))
    (eval binding)))

(defmacro use (namespace)
  (dolist (symbol (eval 'namespace))
    (eval
     `(let ,(string->symbol
             (slice symbol (+ (length namespace) 1)))
        ,symbol))))

(defun zipwith (function a b)
  (let ((result '()))
    (while (and a b)
      (set result (cons result (function (car a) (car b)))
           a (cdr a)
           b (cdr b)))
    result))

(defun zip (a b)
  (let ((result '()))
    (while (and a b)
      (set result (cons! 1 result `(,(car a) ,(car b)))
           a (cdr a)
           b (cdr b)))
    result))

(defun empty? (sequence)
  (= (length sequence) 0))

(defmacro loop (:rest body)
  `(while true
     ,@body))

(defun load (path)
  (eval (open path)))
".to_string()).expect("100% rust bug not mine")).expect("100% rust bug not mine");
}
