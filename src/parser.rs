use std::result;
use crate::ast::{Value, Result};

struct Parser {
    length: usize,
    source: Vec<char>,
    position: usize,
    line: usize
}

impl Parser {
    fn new(source: String) -> Self {
        Self {
            length: source.len(),
            source: source.chars().collect(),
            position: 0,
            line: 1
        }
    }

    fn is_at_end(&self) -> bool {
        self.position >= self.length
    }

    fn get_char(&self) -> Option<char> {
        if self.is_at_end() {
            None
        } else {
            Some(self.source[self.position])
        }
    }

    fn consume(&mut self, character: char) -> bool {
        if !self.is_at_end() && self.source[self.position + 1] == character {
            self.advance();
            true
        } else {
            false
        }
    }

    fn advance(&mut self) {
        if !self.is_at_end() {
            if self.get_char() == Some('\n') {
                self.line += 1;
            }
            self.position += 1;
        }
    }

    fn advance_till(&mut self, targets: &[char], consume: &[char]) {
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

    fn parse_atom(&mut self, atom: &str) -> Value {
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

    fn read_atom(&mut self) -> Result {
        let position = self.position;

        self.advance_till(&['(', ')', ';', ' ', '\n'], &[]);

        let atom = &self.source[position..self.position]
            .iter()
            .collect::<String>();

        let atom = self.parse_atom(atom);

        self.skip_whitespace();
        Ok(atom)
    }

    fn read_string(&mut self) -> Result {
        self.advance();
        let mut position = self.position;
        let mut string = String::new();

        while let Some(character) = self.get_char() {
            if character == '"' {
                break
            } else if character == '\n' {
                self.advance();
                break;
            } else {
                if character == '\\' {
                    self.advance();
                    let character;

                    match self.get_char() {
                        Some('n')  => character = '\n',
                        Some('r')  => character = '\r',
                        Some('t')  => character = '\t',
                        Some('0')  => character = '\0',
                        Some('"')  => character = '\"',
                        Some('\\') => character = '\\',
                        Some(c)    => return Err(format!("invalid escape character '{}'", c)),
                        None       => return Err("incomplete escape sequence".to_string())
                    }

                    string.push_str(
                        &self.source[position..self.position - 1]
                            .iter()
                            .collect::<String>()
                    );

                    position = self.position + 1;
                    string.push(character);
                }
                self.advance();
            }
        }

        if self.is_at_end() {
            Err("unterminated string".to_string())
        } else {
            if self.position > position {
                string.push_str(
                    &self.source[position..self.position]
                        .iter()
                        .collect::<String>()
                );
            }

            self.advance();
            self.skip_whitespace();
            Ok(Value::String(string))
        }
    }

    fn read_list(&mut self) -> Result {
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
            Ok(Value::List(list))
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(character) = self.get_char() {
            match character {
                '\n' | '\t' | '\r' | ' ' => self.advance(),
                ';' => self.advance_till(&[], &['\n']),
                _ => return
            }
        }
    }

    fn read_quote(&mut self, kind: &str) -> Result {
        use Value::*;

        self.advance();
        match self.read_token() {
            Ok(token) => Ok(List(vec![Symbol(kind.to_string()), token])),
            error => error
        }
    }

    fn read_token(&mut self) -> Result {
        self.skip_whitespace();

        if let Some(character) = self.get_char() {
            match character {
                '(' => self.read_list(),
                ')' => Err("unbalanced parenthesis".to_string()),
                '"' => self.read_string(),

                '\'' => self.read_quote("quote"),
                '`' => self.read_quote("quasiquote"),
                ',' => if self.consume('@') {
                    self.read_quote("unquote-splice")
                } else {
                    self.read_quote("unquote")
                },

                _ => self.read_atom(),
            }
        } else {
            Ok(Value::Nil)
        }
    }
}

pub fn tokenize(source: String) -> result::Result<Vec<Value>, String> {
    let mut parser = Parser::new(source);
    let mut tokens = vec![];

    while parser.position < parser.length {
        let line = parser.line;

        match parser.read_token() {
            Ok(token) => tokens.push(token),
            Err(message) => return Err(format!("{} in line {}", message, line))
        }
    }

    Ok(tokens)
}
