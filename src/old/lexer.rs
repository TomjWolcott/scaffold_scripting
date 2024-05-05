use std::num::ParseFloatError;
use std::fs::File;
use std::io::Read;
use lazy_static::*;
use regex::*;

macro_rules! re {
    ($lit:literal) => {{
        lazy_static! {
            static ref REGEX: Regex = Regex::new($lit).unwrap();
        }

        &REGEX
    }};
}


#[derive(Debug)]
pub enum LexerError {
    BadNumber(ParseFloatError),
    NoMatchFound(String),
}

pub fn lexer(string: &str) -> Result<Vec<Token>, LexerError> {
    let mut i = 0;
    let mut tokens = Vec::new();
    let mut delimiter_depth = 0;

    while i < string.len() {
        let mut str = &string[i..];

        if let Some(mat) = re!(r"^[\n\r \t]*").find(str) {
            i += mat.len();
        }

        str = &string[i..];

        if let Some(mat) = re!(r"^//[^\r\n]*\r?\n").find(str) {
            i += mat.len();
        }

        str = &string[i..];

        tokens.push(
            if str.starts_with("class") {
                i += 5;
                Token::Class
            } else if let Some(mat) = re!(r"^\p{L}[\p{L}0-9_]*").find(str) {
                i += mat.len();
                Token::Ident(mat.as_str().to_string())
            } else if let Some(mat) = re!(r"^(\+|-)?[0-9]*\.[0-9]+|^(\+|-)?[0-9]+\.?").find(str) {
                i += mat.len();
                match mat.as_str().parse::<f32>() {
                    Ok(num) => Token::Number(num),
                    Err(err) => {
                        return Err(LexerError::BadNumber(err))
                    }
                }
            } else if let Some(_) = re!(r"^[\(\[\{]").find(str) {
                let depth = delimiter_depth;

                delimiter_depth += 1;
                i += 1;

                Token::OpenDelimiter(depth, str.chars().nth(0).unwrap())
            } else if let Some(_) = re!(r"^[\)\]\}]").find(str) {
                delimiter_depth -= 1;
                i += 1;

                Token::CloseDelimiter(delimiter_depth, str.chars().nth(0).unwrap())
            } else if let Some(mat) = re!(r"^(->|::|[\*\-\+/\.:=,;])").find(str) {
                i += mat.len();

                Token::Symbol(mat.as_str().to_string())
            } else {
                return Err(LexerError::NoMatchFound(str.to_string()));
            }
        );
    }

    Ok(tokens)
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Class,
    Ident(String),
    Number(f32),
    OpenDelimiter(usize, char),
    CloseDelimiter(usize, char),
    Symbol(String)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_numbers() {
        let input = "Vec4(+1, -.34, 82., 0.0)";
        let expected = vec![
            Token::Ident("Vec4".to_string()),
            Token::OpenDelimiter(0, '('),
            Token::Number(1.0),
            Token::Symbol(",".to_string()),
            Token::Number(-0.34),
            Token::Symbol(",".to_string()),
            Token::Number(82.0),
            Token::Symbol(",".to_string()),
            Token::Number(0.0),
            Token::CloseDelimiter(0, ')')
        ];

        assert_eq!(lexer(input).unwrap(), expected);
    }

    #[test]
    fn test_lexer() {
        let input = "class MyClass {";
        let expected = vec![
            Token::Class,
            Token::Ident("MyClass".to_string()),
            Token::OpenDelimiter(0, '{')
        ];

        assert_eq!(lexer(input).unwrap(), expected);
    }

    #[test]
    fn test_lexer_with_number() {
        let input = "class MyClass { 1.0 }";
        let expected = vec![
            Token::Class,
            Token::Ident("MyClass".to_string()),
            Token::OpenDelimiter(0, '{'),
            Token::Number(1.0),
            Token::CloseDelimiter(0, '}')
        ];

        assert_eq!(lexer(input).unwrap(), expected);
    }

    #[test]
    fn test_lexer_with_symbol() {
        let input = "class MyClass { 1.0 + 2.0 }";
        let expected = vec![
            Token::Class,
            Token::Ident("MyClass".to_string()),
            Token::OpenDelimiter(0, '{'),
            Token::Number(1.0),
            Token::Symbol("+".to_string()),
            Token::Number(2.0),
            Token::CloseDelimiter(0, '}')
        ];

        assert_eq!(lexer(input).unwrap(), expected);
    }
}