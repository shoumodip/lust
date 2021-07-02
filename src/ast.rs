use std::fmt;
use std::result;
use std::process;
use std::collections::HashMap;

pub type Env = HashMap<String, Value>;
pub type Function = fn(Vec<Value>) -> Result;
pub type Result = result::Result<Value, String>;

#[derive(Clone)]
#[allow(dead_code)]
pub enum Value {
    Boolean(bool),
    Number(f64),
    Symbol(String),
    String(String),
    Native(Function),
    Lambda(Vec<String>, Vec<Value>),
    List(Vec<Value>),
    Nil,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Value::*;
        match self {
            Boolean(b) => write!(f, "{}", b),
            Number(n) => write!(f, "{}", n),
            Symbol(s) => write!(f, "{}", s),
            String(s) => write!(f, "{}", s),
            Native(_) => write!(f, "#<function>"),
            Lambda(_, _) => write!(f, "#<function>"),
            List(list) => {
                write!(f, "(")?;
                for (index, value) in list.iter().enumerate() {
                    if index != 0 { write!(f, " ")?; }
                    match value {
                        String(s) => write!(f, "\"{}\"", s),
                        value => write!(f, "{}", value)
                    }?;
                }
                write!(f, ")")
            }
            Nil => write!(f, "nil"),
        }
    }
}

pub struct Ast {
    pub env: Vec<Env>,
    pub calls: Vec<String>,
    pub source: Vec<Value>,
}

impl Ast {
    pub fn new(source: Vec<Value>) -> Self {
        Self {
            env: vec![Env::new()],
            calls: vec![],
            source,
        }
    }

    pub fn define(&mut self, symbol: String, value: Value) {
        self.env[0].insert(symbol, value);
    }

    pub fn lookup(&self, symbol: String) -> Result {
        if self.calls.len() == 0 {
            for env in self.env.iter().rev() {
                if let Some(value) = env.get(&symbol) {
                    return Ok(value.clone());
                }
            }
        } else {
            let last = self.env.len() - 1;

            if let Some(value) = self.env[0].get(&symbol) {
                return Ok(value.clone());
            } else if let Some(value) = self.env[last].get(&symbol) {
                return Ok(value.clone());
            }
        }

        Err(format!("undefined symbol '{}'", symbol))
    }

    pub fn eval_native(&mut self,
                       name: String,
                       function: Function,
                       arguments: &[Value]) -> Result
    {
        let mut parameters: Vec<Value> = vec![];
        
        for argument in arguments {
            match self.eval(argument.clone()) {
                Ok(value) => parameters.push(value),
                error => return error,
            }
        }

        self.calls.push(name);
        let result = function(parameters);
        self.calls.pop();

        result
    }

    pub fn eval_lambda(&mut self,
                       name: String,
                       parameters: Vec<String>,
                       arguments: &[Value],
                       body: Vec<Value>) -> Result
    {
        use Value::Nil;

        if parameters.len() != arguments.len() {
            Err(format!("expected {} parameters, found {} instead",
                        parameters.len(),
                        arguments.len()))
        } else {
            let mut env = Env::new();

            for (index, argument) in arguments.iter().enumerate() {
                match self.eval(argument.clone()) {
                    Ok(value) => {
                        env.insert(parameters[index].clone(), value);
                    }
                    error => return error,
                }
            }

            self.env.push(env);
            self.calls.push(name);

            let mut result = Nil;
            for expression in body {
                match self.eval(expression) {
                    Ok(value) => result = value,
                    error => return error,
                }
            }

            self.env.pop();
            self.calls.pop();
            Ok(result)
        }
    }

    pub fn eval_define(&mut self, arguments: &[Value]) -> Result {
        use Value::*;
        if arguments.len() < 2 {
            Err(format!("expected variable name and value, instead received {} arguments",
                        arguments.len()))
        } else {
            match arguments[0].clone() {
                Symbol(variable) => match self.eval(arguments[1].clone()) {
                    Ok(value) => {
                        self.define(variable, value);
                        Ok(Nil)
                    },
                    error => error
                },
                invalid => Err(format!("invalid variable '{}'", invalid))
            }
        }
    }

    pub fn eval_call(&mut self, name: String, arguments: &[Value]) -> Result {
        use Value::*;

        match &name[..] {
            "define" => self.eval_define(arguments),
            "quote" => Ok(arguments[0].clone()),

            function => match self.lookup(function.to_string()) {
                Ok(value) => match value {
                    Native(f) => self.eval_native(name, f, arguments),
                    Lambda(parameters, body) => self.eval_lambda(name, parameters, arguments, body),
                    invalid => Err(format!("invalid function '{}'", invalid))
                },
                error => error
            }
        }
    }

    pub fn eval_list(&mut self, list: Vec<Value>) -> Result {
        use Value::*;
        match list.len() {
            0 => Err("cannot evaluate empty expression".to_string()),

            _ => match list[0].clone() {
                Symbol(s) => self.eval_call(s, &list[1..]),
                Lambda(parameters, body) => self.eval_lambda("#<lambda>".to_string(),
                                                             parameters,
                                                             &list[1..],
                                                             body),

                invalid => Err(format!("invalid function '{}'", invalid))
            }
        }
    }

    pub fn eval(&mut self, expression: Value) -> Result {
        use Value::*;
        match expression {
            Symbol(s) => self.lookup(s),
            List(list) => self.eval_list(list),
            _ => Ok(expression)
        }
    }

    pub fn run(&mut self) {
        for expression in self.source.clone() {
            self.eval(expression).unwrap_or_else(|message| {
                eprintln!("lust: {}", message);

                if self.calls.len() > 0 {
                    eprintln!("stack: ");

                    for call in &self.calls {
                        eprintln!("{}", call);
                    }
                }

                process::exit(1);
            });
        }
    }
}
