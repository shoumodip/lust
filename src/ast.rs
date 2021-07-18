use std::fmt;
use std::result;
use std::collections::HashMap;

use crate::parser;

type Env = HashMap<String, Value>;
type Function = fn(Vec<Value>) -> Result;
pub type Result = result::Result<Value, String>;

#[derive(PartialEq, Clone)]
pub enum Value {
    Boolean(bool),
    Number(f64),
    Symbol(String),
    String(String),
    Native(Function),
    Lambda(bool, bool, Vec<String>, Vec<Value>),
    List(Vec<Value>),
    Nil,
}

pub fn is_true(value: &Value) -> bool {
    use Value::*;
    match value {
        Number(n) if *n == 0.0 => false,
        List(l) if l.len() == 0 => false,
        Boolean(false) => false,
        Nil => false,
        _ => true
    }
}

pub fn is_nil(value: &Value) -> bool {
    use Value::*;
    match value {
        Nil => true,
        List(l) if l.len() == 0 => true,
        _ => false
    }
}

fn eval_lambda_form(arguments: &[Value], is_macro: bool) -> Result {
    use Value::*;

    if arguments.len() == 0 {
        Ok(Lambda(is_macro, false, vec![], vec![]))
    } else {
        match &arguments[0] {
            List(lambda_parameters) => {
                let mut parameters = vec![];
                let mut variadic = false;
                let mut variadic_unnamed = true;

                for parameter in lambda_parameters {
                    match parameter {
                        Symbol(s) => if s == ":rest" {
                            variadic = true;
                        } else {
                            parameters.push(s.clone());
                            if variadic {
                                variadic_unnamed = false;
                                break;
                            }
                        }

                        invalid => return Err(format!("invalid parameter '{}' in {}",
                                                      invalid,
                                                      if is_macro {"macro"} else {"lambda"}))
                    }
                }

                if variadic && variadic_unnamed {
                    Err(format!("name of variadic argument is not given in {}",
                                if is_macro {"macro"} else {"lambda"}))
                } else {
                    Ok(Lambda(is_macro, variadic, parameters, arguments[1..].to_vec()))
                }

            },
            invalid => return Err(format!("invalid parameter list '{}' in {}",
                                          invalid,
                                          if is_macro {"macro"} else {"lambda"}))
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Value::*;
        match self {
            Boolean(b) => write!(f, "{}", b),
            Number(n) => write!(f, "{}", n),
            Symbol(s) => write!(f, "{}", s),
            String(s) => write!(f, "{}", s),
            Native(_) => write!(f, "#<native>"),
            Lambda(false, _, _, _) => write!(f, "#<lambda>"),
            Lambda(true, _, _, _) => write!(f, "#<macro>"),
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

struct Scope {
    list: Vec<Env>
}

impl Scope {
    fn new() -> Self {
        Self {
            list: vec![Env::new()]
        }
    }

    fn from(env: Env) -> Self {
        Self {
            list: vec![env]
        }
    }

    fn first(&self) -> &Env {
        &self.list[0]
    }

    fn first_mut(&mut self) -> &mut Env {
        &mut self.list[0]
    }

    fn define(&mut self, symbol: String, value: Value) {
        self.list
            .first_mut()
            .expect("100% rust bug not mine")
            .insert(symbol, value);
    }

    fn contains(&self, symbol: &String) -> bool {
        for env in self.list.iter().rev() {
            if env.contains_key(symbol) {
                return true;
            }
        }
        false
    }

    fn lookup(&self, symbol: &String) -> Option<Value> {
        for env in self.list.iter().rev() {
            if let Some(value) = env.get(symbol) {
                return Some(value.clone());
            }
        }
        None
    }

    fn lookup_mut(&mut self, symbol: &String) -> Option<&mut Value> {
        for env in self.list.iter_mut().rev() {
            if let Some(value) = env.get_mut(symbol) {
                return Some(value);
            }
        }

        None
    }

    fn push(&mut self, env: Env) {
        self.list.push(env);
    }

    fn pop(&mut self) {
        self.list.pop();
    }
}

pub struct Ast {
    calls: Vec<String>,
    scopes: Vec<Scope>,
}

impl Ast {
    pub fn new() -> Self {
        Self {
            calls: vec![],
            scopes: vec![Scope::new()],
        }
    }

    pub fn define(&mut self, symbol: &str, value: Value) {
        self.scopes
            .first_mut()
            .expect("100% rust bug not mine")
            .define(symbol.to_string(), value);
    }

    fn last_scope(&mut self) -> &mut Scope {
        self.scopes.last_mut()
            .expect("100% rust bug not mine")
    }

    fn push_scope(&mut self, scope: Scope) {
        self.scopes.push(scope);
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn lookup(&self, symbol: &str) -> Result {
        let symbol = &symbol.to_string();

        if self.calls.len() == 0 {
            for scope in self.scopes.iter().rev() {
                if let Some(value) = scope.lookup(symbol) {
                    return Ok(value.clone());
                }
            }
        } else {
            let last = self.scopes.len() - 1;

            if let Some(value) = self.scopes[last].lookup(symbol) {
                return Ok(value.clone());
            }

            if let Some(value) = self.scopes[0].first().get(symbol) {
                return Ok(value.clone());
            }
        }

        Err(format!("undefined symbol '{}'", symbol))
    }

    fn scope_set(&mut self, symbol: &String, value: Value) -> Result {
        use Value::*;
        if self.calls.len() == 0 {
            for scope in self.scopes.iter_mut().rev() {
                if let Some(symbol) = scope.lookup_mut(symbol) {
                    *symbol = value;
                    return Ok(Nil);
                }
            }
        } else {
            let last = self.scopes.len() - 1;

            if let Some(symbol) = self.scopes[last].lookup_mut(symbol) {
                *symbol = value;
                return Ok(Nil);
            } else if let Some(symbol) = self.scopes[0].first_mut().get_mut(symbol) {
               *symbol = value;
                return Ok(Nil);
            }
        }

        Err(format!("undefined symbol '{}'", symbol))
    }

    fn scope_get(&mut self, symbol: &String) -> result::Result<&mut Value, String> {
        if self.calls.len() == 0 {
            for scope in self.scopes.iter_mut().rev() {
                if let Some(symbol) = scope.lookup_mut(symbol) {
                    return Ok(symbol);
                }
            }
        } else {
            let last = self.scopes.len() - 1;

            if self.scopes[last].contains(symbol) {
                return Ok(
                    self.scopes[last]
                        .lookup_mut(symbol)
                        .expect("100% rust bug not mine")
                );
            } else if let Some(symbol) = self.scopes[0].first_mut().get_mut(symbol) {
                return Ok(symbol);
            }
        }

        Err(format!("undefined symbol '{}'", symbol))
    }

    fn eval_native(&mut self,
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

    fn eval_lambda(&mut self,
                       name: String,
                       parameters: Vec<String>,
                       arguments: &[Value],
                       variadic: bool,
                       is_macro: bool,
                       body: Vec<Value>) -> Result
    {
        let parameters_length = parameters.len();
        let arguments_length = arguments.len();

        if variadic {
            if arguments_length < parameters_length {
                return Err(format!("variadic {} '{}' takes at least {} parameter(s), found {} instead",
                                   if is_macro {"macro"} else {"function"},
                                   name,
                                   parameters_length,
                                   arguments_length));
            }
        } else {
            if arguments_length != parameters_length {
                return Err(format!("{} '{}' takes {} parameter(s), found {} instead",
                                   if is_macro {"macro"} else {"function"},
                                   name,
                                   parameters_length,
                                   arguments_length));
            }
        }

        let mut env = Env::new();

        for i in 0..parameters_length {
            if is_macro {
                env.insert(parameters[i].clone(), arguments[i].clone());
            } else {
                match self.eval(arguments[i].clone()) {
                    Ok(value) => {
                        env.insert(parameters[i].clone(), value);
                    },
                    error => return error
                }
            }
        }

        if variadic && arguments_length >= parameters_length {
            let mut variadic = vec![];

            for i in parameters_length - 1..arguments_length {
                if is_macro {
                    variadic.push(arguments[i].clone());
                } else {
                    match self.eval(arguments[i].clone()) {
                        Ok(value) => variadic.push(value),
                        error => return error
                    }
                }
            }

            env.insert(parameters[parameters_length - 1].clone(), Value::List(variadic));
        }

        self.push_scope(Scope::from(env));
        self.calls.push(name);

        let mut result = self.eval_do(&body);
        self.pop_scope();

        if let Ok(value) = &result {
            if is_macro { result = self.eval(value.clone()); }
            self.calls.pop();
        }

        result
    }

    fn eval_set(&mut self, arguments: &[Value]) -> Result {
        use Value::*;

        if arguments.len() < 1 {
            Err("improper form of set".to_string())
        } else {
            let mut i = 0;

            while i < arguments.len() {
                match arguments[i].clone() {
                    Symbol(symbol) => {
                        let mut value = Nil;

                        if arguments.len() - 1 > i {
                            i += 1;

                            match self.eval(arguments[i].clone()) {
                                Ok(v) => value = v,
                                error => return error
                            }
                        }

                        match self.scope_set(&symbol, value) {
                            Ok(_) => {},
                            error => return error
                        }
                    },
                    List(list) if list.len() > 1 => {
                        let mut value = Nil;
                        if arguments.len() - 1 > i {
                            i += 1;

                            match self.eval(arguments[i].clone()) {
                                Ok(v) => value = v,
                                error => return error
                            }
                        }

                        let mut tree = vec![];
                        for node in &list[1..] {
                            match self.eval(node.clone()) {
                                Ok(Number(n)) => tree.push(n as usize),
                                Ok(invalid) => return Err(format!("invalid list position '{}'", invalid)),
                                error => return error
                            }
                        }

                        match &list[0] {
                            Symbol(s) => match self.scope_get(s) {
                                Ok(mut target) => match target {
                                    List(_) => {
                                        for i in 0..tree.len() {
                                            match target {
                                                List(list) => {
                                                    let index = tree[i];
                                                    if index < list.len() {
                                                        target = &mut list[index];
                                                    } else {
                                                        return Err(format!("invalid index '{}'", index));
                                                    }
                                                },
                                                invalid => return Err(format!("invalid list '{}'", invalid)),
                                            }
                                        }
                                        *target = value;
                                    },
                                    invalid => return Err(format!("invalid list '{}'", invalid)),
                                }
                                Err(message) => return Err(message)
                            }
                            _ => return Err(format!("invalid binding '{}' in set", List(list))),
                        }
                    }
                    invalid => return Err(format!("invalid binding '{}' in set", invalid))
                }

                i += 1;
            }

            Ok(Nil)
        }
    }

    fn eval_let(&mut self, arguments: &[Value]) -> Result {
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
                                Symbol(symbol) => if binding.len() == 2 {
                                    match self.eval(binding[1].clone()) {
                                        Ok(value) => { env.insert(symbol.to_string(), value); }
                                        error => return error,
                                    }
                                } else {
                                    env.insert(symbol.to_string(), Nil);
                                },
                                invalid => return Err(format!("invalid variable '{}' in let", invalid))
                            },
                            invalid => return Err(format!("invalid binding '{}' in let", invalid))
                        }
                    }

                    self.last_scope().push(env);
                    let result = self.eval_do(&arguments[1..]);
                    self.last_scope().pop();
                    result
                },
                _ => {
                    let mut i = 0;
                    while i < arguments.len() {
                        match &arguments[i] {
                            Symbol(symbol) => if arguments.len() - 1 > i {
                                i += 1;

                                match self.eval(arguments[i].clone()) {
                                    Ok(value) => self.define(&symbol, value),
                                    error => return error
                                }
                            } else {
                                self.define(&symbol, Nil);
                            }
                            invalid => return Err(format!("invalid global binding '{}'", invalid))
                        }

                        i += 1;
                    }
                    Ok(Nil)
                }
            }
        }
    }

    fn eval_do(&mut self, body: &[Value]) -> Result {
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

    fn eval_if(&mut self, arguments: &[Value]) -> Result {
        use Value::*;

        if arguments.len() == 0 {
            Ok(Nil)
        } else {
            let branch;

            match self.eval(arguments[0].clone()) {
                Ok(value) => branch = if is_true(&value) {1} else {2},
                error => return error
            }

            if arguments.len() <= branch {
                Ok(Nil)
            } else {
                self.eval(arguments[branch].clone())
            }
        }
    }

    fn eval_do_condition(&mut self, arguments: &[Value], condition: bool) -> Result {
        use Value::*;

        if arguments.len() == 0 {
            Ok(Nil)
        } else {
            match self.eval(arguments[0].clone()) {
                Ok(value) => if is_true(&value) == condition {
                    self.eval_do(&arguments[1..])
                } else {
                    Ok(Nil)
                },
                error => error
            }
        }
    }

    fn eval_while(&mut self, arguments: &[Value]) -> Result {
        use Value::*;

        if arguments.len() < 1 {
            Ok(Nil)
        } else {
            let mut result = Nil;

            loop {
                match self.eval(arguments[0].clone()) {
                    Ok(value) if !is_true(&value) => break,
                    Ok(_) => match self.eval_do(&arguments[1..]) {
                        Ok(value) => result = value,
                        error => return error
                    },
                    error => return error
                }
            }

            Ok(result)
        }
    }

    fn eval_dolist(&mut self, arguments: &[Value]) -> Result {
        use Value::*;

        if arguments.len() < 1 {
            Ok(Nil)
        } else {
            match arguments[0].clone() {
                List(declaration) => if declaration.len() == 2 {
                    match &declaration[0] {
                        Symbol(symbol) => match self.eval(declaration[1].clone()) {
                            Ok(List(list)) => {
                                if list.len() > 0 {
                                    let mut env = Env::new();
                                    env.insert(symbol.clone(), Nil);
                                    self.last_scope().push(env);

                                    for value in list {
                                        match self.scope_set(symbol, value) {
                                            Ok(_) => {},
                                            error => return error
                                        }

                                        match self.eval_do(&arguments[1..]) {
                                            Ok(_) => {},
                                            error => return error
                                        }
                                    }

                                    self.last_scope().pop();
                                }

                                Ok(Nil)
                            },
                            Ok(invalid) => Err(format!("invalid list '{}' in dolist", invalid)),
                            error => error
                        }
                        invalid => Err(format!("invalid iterator '{}' in dolist", invalid))
                    }
                } else {
                    Ok(Nil)
                },
                invalid => Err(format!("invalid binding '{}' in dolist", invalid))
            }
        }
    }

    fn eval_eval(&mut self, arguments: &[Value]) -> Result {
        use Value::*;

        if arguments.len() < 1 {
            Ok(Nil)
        } else {
            match self.eval(arguments[0].clone()) {
                Ok(String(s)) => {
                    self.calls.push("eval".to_string());

                    match parser::tokenize(s) {
                        Ok(tokens) => {
                            self.calls.pop();
                            match self.run(tokens) {
                                Some(value) => Ok(value),
                                None => Ok(Nil)
                            }
                        },
                        Err(message) => Err(message)
                    }
                },
                Ok(sexp) => match self.eval(sexp) {
                    Ok(value) => self.eval(value),
                    error => error
                },
                error => error
            }
        }
    }

    fn eval_quasiquote(&mut self, value: Value) -> Result {
        use Value::*;

        match value {
            List(list) => {
                let mut result = vec![];

                for value in list {
                    match value.clone() {
                        List(list) if list.len() > 0 => match list[0].clone() {
                            Symbol(s) if s == "unquote" => match self.eval(list[1].clone()) {
                                Ok(value) => result.push(value),
                                error => return error
                            },

                            Symbol(s) if s == "unquote-splice" => {
                                match list[1].clone() {
                                    List(list) => result.extend(list),
                                    symbol => match self.eval(symbol) {
                                        Ok(List(list)) => result.extend(list),
                                        Ok(value) => result.push(value),
                                        error => return error
                                    }
                                }
                            },

                            _ => match self.eval_quasiquote(List(list)) {
                                Ok(value) => result.push(value),
                                error => return error
                            }
                        },
                        value => result.push(value)
                    }
                }

                Ok(List(result))
            },
            atom => Ok(atom)
        }
    }


    fn eval_call(&mut self, name: String, arguments: &[Value]) -> Result {
        use Value::*;

        match self.lookup(&name) {
            Ok(value) => match value {
                Native(f) => self.eval_native(name, f, arguments),
                Lambda(is_macro, variadic, parameters, body) => self.eval_lambda(name,
                                                                                 parameters,
                                                                                 arguments,
                                                                                 variadic,
                                                                                 is_macro,
                                                                                 body),
                invalid => Err(format!("invalid function '{}'", invalid))
            },
            error => match &name[..] {
                "let" => self.eval_let(arguments),
                "set" => self.eval_set(arguments),
                "do" => self.eval_do(arguments),
                "if" => self.eval_if(arguments),
                "when" => self.eval_do_condition(arguments, true),
                "unless" => self.eval_do_condition(arguments, false),
                "dolist" => self.eval_dolist(arguments),
                "while" => self.eval_while(arguments),
                "lambda" => eval_lambda_form(arguments, false),
                "macro" => eval_lambda_form(arguments, true),
                "quote" => if arguments.len() > 0 {
                    Ok(arguments[0].clone())
                } else {
                    Ok(Nil)
                },
                "quasiquote" => self.eval_quasiquote(arguments[0].clone()),
                "eval" => self.eval_eval(arguments),
                _ => error
            }
        }
    }

    fn eval_list(&mut self, list: Vec<Value>) -> Result {
        use Value::*;
        match list.len() {
            0 => Err("cannot evaluate empty expression".to_string()),

            _ => match list[0].clone() {
                Symbol(s) => self.eval_call(s, &list[1..]),
                Lambda(is_macro, variadic, parameters, body) => self.eval_lambda("#<lambda>".to_string(),
                                                                                 parameters,
                                                                                 &list[1..],
                                                                                 variadic,
                                                                                 is_macro,
                                                                                 body),

                List(l) => match self.eval_list(l) {
                    Ok(function) => {
                        let mut expression = vec![function];
                        expression.extend(list[1..].to_vec());
                        self.eval_list(expression)
                    }
                    error => error
                },
                invalid => Err(format!("invalid function '{}'", invalid))
            }
        }
    }

    fn eval(&mut self, expression: Value) -> Result {
        use Value::*;
        match expression {
            Symbol(s) => self.lookup(&s),
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
                    eprintln!("error: {}", message);

                    if self.calls.len() > 0 {
                        let mut i = self.calls.len();

                        while i > 0 {
                            i -= 1;
                            if i > 0 && self.calls[i] == self.calls[i - 1] { continue; }
                            eprintln!("In ({})", self.calls[i]);
                        }
                    }

                    self.calls.clear();
                    return None
                }
            }
        }

        Some(result)
    }
}
