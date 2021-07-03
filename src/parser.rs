use std::process;
use crate::ast::{Value, Result};

pub struct Parser {
    length: usize,
    source: Vec<char>,
    position: usize,
}

impl Parser {
    pub fn new(source: String) -> Self {
        Self {
            length: source.len(),
            source: source.chars().collect(),
            position: 0,
        }
    }

    pub fn is_at_end(&self) -> bool {
        self.position >= self.length
    }

    pub fn get_char(&self) -> Option<char> {
        if self.is_at_end() {
            None
        } else {
            Some(self.source[self.position])
        }
    }

    pub fn advance(&mut self) {
        if !self.is_at_end() {
            self.position += 1;
        }
    }

    pub fn advance_till(&mut self, targets: &[char], consume: &[char]) {
        while let Some(character) = self.get_char() {
            for target in targets {
                if character == *target {
                    return;
                }
            }

            for target in consume {
                if character == *target {
                    self.advance();
                    return;
                }
            }

            self.advance();
        }
    }

    pub fn parse_atom(&mut self, atom: &str) -> Value {
        match atom {
            "true" => Value::Boolean(true),
            "false" => Value::Boolean(false),
            "nil" => Value::Nil,
            _ => match atom.parse::<f64>() {
                Ok(number) => Value::Number(number),
                Err(_) => Value::Symbol(atom.to_string())
            }
        }
    }

    pub fn read_atom(&mut self) -> Result {
        let position = self.position;

        self.advance_till(&['(', ')', ';', ' ', '\n'], &[]);

        let atom = &self.source[position..self.position]
            .iter()
            .collect::<String>();

        let atom = self.parse_atom(atom);

        self.skip_whitespace();
        Ok(atom)
    }

    pub fn read_string(&mut self) -> Result {
        self.advance();
        let position = self.position;

        self.advance_till(&['"'], &['\n']);

        if self.is_at_end() {
            Err("unterminated string".to_string())
        } else {
            let string = self.source[position..self.position]
                .iter()
                .collect();

            self.advance();
            self.skip_whitespace();
            Ok(Value::String(string))
        }
    }

    pub fn parse_list(&mut self, list: &mut Vec<Value>) {
        use Value::*;

        if list.len() == 0 {
            return;
        }

        if let Symbol(s) = &list[0] {
            match &s[..] {
                "let" => match &list[1] {
                    Symbol(_) => list[0] = Symbol("define".to_string()),
                    _ => {}
                },
                _ => {}
            }
        }
    }

    pub fn read_list(&mut self) -> Result {
        let mut list = vec![];
        self.advance();

        while !self.is_at_end() && self.get_char() != Some(')') {
            match self.read_token() {
                Ok(token) => list.push(token),
                error => return error
            }
        }

        if self.is_at_end() {
            Err("unterminated parenthesis".to_string())
        } else {
            self.advance();
            self.skip_whitespace();
            self.parse_list(&mut list);
            Ok(Value::List(list))
        }
    }

    pub fn skip_whitespace(&mut self) {
        while let Some(character) = self.get_char() {
            match character {
                '\n' | '\t' | '\r' | ' ' => self.advance(),
                ';' => self.advance_till(&[], &['\n']),
                _ => return
            }
        }
    }

    pub fn read_token(&mut self) -> Result {
        use Value::*;
        self.skip_whitespace();

        if let Some(character) = self.get_char() {
            match character {
                '(' => self.read_list(),
                ')' => Err("unbalanced parenthesis".to_string()),
                '"' => self.read_string(),

                '\'' => {
                    self.advance();
                    match self.read_token() {
                        Ok(token) => Ok(List(vec![Symbol("quote".to_string()), token])),
                        error => error
                    }
                },

                _ => self.read_atom(),
            }
        } else {
            Ok(Nil)
        }
    }
}

pub fn tokenize(source: String) -> Vec<Value> {
    let mut parser = Parser::new(source);
    let mut tokens = vec![];

    while parser.position < parser.length {
        match parser.read_token() {
            Ok(token) => tokens.push(token),
            Err(message) => {
                eprintln!("lust: {}", message);
                process::exit(1);
            }
        }
    }

    tokens
}
