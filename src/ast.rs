use std::fmt;
use std::result;
use std::collections::HashMap;

pub type Env = HashMap<String, Value>;
pub type Function = fn(Vec<Value>) -> Result;
pub type Result = result::Result<Value, String>;

#[derive(Clone)]
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

pub struct Scope {
    list: Vec<Env>
}

impl Scope {
    pub fn new() -> Self {
        Self {
            list: vec![Env::new()]
        }
    }

    pub fn from(env: Env) -> Self {
        Self {
            list: vec![env]
        }
    }

    pub fn first(&self) -> &Env {
        &self.list[0]
    }

    pub fn define(&mut self, symbol: String, value: Value) {
        self.list
            .first_mut()
            .expect("100% rust bug not mine")
            .insert(symbol, value);
    }

    pub fn lookup(&self, symbol: String) -> Option<Value> {
        for env in self.list.iter().rev() {
            if let Some(value) = env.get(&symbol) {
                return Some(value.clone());
            }
        }

        None
    }

    pub fn push(&mut self, env: Env) {
        self.list.push(env);
    }

    pub fn pop(&mut self) {
        self.list.pop();
    }
}

pub struct Ast {
    pub calls: Vec<String>,
    pub scopes: Vec<Scope>,
}

impl Ast {
    pub fn new() -> Self {
        Self {
            calls: vec![],
            scopes: vec![Scope::new()],
        }
    }

    pub fn define(&mut self, symbol: String, value: Value) {
        self.scopes
            .first_mut()
            .expect("100% rust bug not mine")
            .define(symbol, value);
    }

    pub fn last_scope(&mut self) -> &mut Scope {
        self.scopes.last_mut()
            .expect("100% rust bug not mine")
    }

    pub fn push_scope(&mut self, scope: Scope) {
        self.scopes.push(scope);
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn lookup(&self, symbol: String) -> Result {
        if self.calls.len() == 0 {
            for scope in self.scopes.iter().rev() {
                if let Some(value) = scope.lookup(symbol.clone()) {
                    return Ok(value.clone());
                }
            }
        } else {
            let last = self.scopes.len() - 1;

            if let Some(value) = self.scopes[last].lookup(symbol.clone()) {
                return Ok(value.clone());
            }

            if let Some(value) = self.scopes[0].first().get(&symbol) {
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
            Err(format!("{}() takes {} parameter(s), found {} instead",
                        name,
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

            self.push_scope(Scope::from(env));
            self.calls.push(name);

            let mut result = Nil;
            for expression in body {
                match self.eval(expression) {
                    Ok(value) => result = value,
                    error => return error,
                }
            }

            self.pop_scope();
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

    pub fn eval_do(&mut self, body: &[Value]) -> Result {
        use Value::*;

        let mut result = Nil;
        for expression in body {
            match self.eval(expression.clone()) {
                Ok(value) => result = value,
                error => return error,
            }
        }

        Ok(result)
    }

    pub fn eval_let(&mut self, arguments: &[Value]) -> Result {
        use Value::*;

        if arguments.len() == 0 {
            Ok(Nil)
        } else {
            match &arguments[0] {
                List(scope) => {
                    let mut env = Env::new();

                    for binding in scope.iter() {
                        match binding {
                            List(binding) => match &binding[0] {
                                Symbol(symbol) => match self.eval(binding[1].clone()) {
                                    Ok(value) => { env.insert(symbol.to_string(), value); }
                                    error => return error,
                                },
                                invalid => return Err(format!("invalid variable '{}'", invalid))
                            },
                            invalid => return Err(format!("invalid binding in scope '{}'", invalid))
                        }
                    }

                    self.last_scope().push(env);

                    let mut result = Nil;
                    for expression in &arguments[1..] {
                        match self.eval(expression.clone()) {
                            Ok(value) => result = value,
                            error => return error,
                        }
                    }

                    self.last_scope().pop();
                    Ok(result)
                },
                _ => Err("invalid form of scope".to_string())
            }
        }
    }

    pub fn eval_call(&mut self, name: String, arguments: &[Value]) -> Result {
        use Value::*;

        match &name[..] {
            "define" => self.eval_define(arguments),
            "quote" => Ok(arguments[0].clone()),
            "let" => self.eval_let(arguments),
            "do" => self.eval_do(arguments),
            "lambda" => {
                if arguments.len() == 0 {
                    Ok(Lambda(vec![], vec![]))
                } else {
                    match &arguments[0] {
                        List(lambda_parameters) => {
                            let mut parameters = vec![];

                            for parameter in lambda_parameters {
                                match parameter {
                                    Symbol(s) => parameters.push(s.clone()),
                                    invalid => return Err(format!("invalid parameter '{}'", invalid))
                                }
                            }

                            Ok(Lambda(parameters, arguments[1..].to_vec()))
                        },
                        invalid => return Err(format!("invalid parameter list '{}'", invalid))
                    }
                }
            }

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

    pub fn run(&mut self, source: Vec<Value>) -> Option<Value> {
        let mut result = Value::Nil;

        for expression in source.clone() {
            match self.eval(expression) {
                Ok(value) => result = value,
                Err(message) => {
                    eprintln!("lust: {}", message);

                    if self.calls.len() > 0 {
                        for call in self.calls.iter().rev() {
                            eprintln!("In {}()", call);
                        }
                    }

                    return None
                }
            }
        }

        Some(result)
    }
}
