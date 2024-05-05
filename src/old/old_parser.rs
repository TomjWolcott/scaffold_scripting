use crate::lexer::Token;

pub enum ParseError {
    ClassParseError(Vec<Token>)
}

pub trait Parse {
    fn parse(tokens: &mut &[Token]) -> Result<Self, ParseError>;
}

#[derive(Debug, PartialEq, Clone)]
pub struct Script {
    classes: Vec<Class>
}

impl Parse for Script {
    fn parse(tokens: &mut &[Token]) -> Result<Self, ParseError> {
        let mut classes = Vec::new();

        while tokens.len() > 0 {
            match Class::parse(tokens) {
                Ok(class) => classes.push(class),
                Err(err) => { return Err(err) }
            }
        }

        Ok(Self { classes })
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Class {
    fields: Vec<Binding>,
    methods: Vec<Method>
}

impl Parse for Class {
    fn parse(tokens: &mut &[Token]) -> Result<Self, ParseError> {
        match *tokens {
            [
                Token::Class,
                Token::Ident(class_name),
                Token::OpenDelimiter(depth, '{'),
                ..
            ] => {
                let mut fields = Vec::new();
                let mut methods = Vec::new();
                *tokens = &tokens[3..];

                while tokens[0] == Token::CloseDelimiter(*depth, '}') {

                }

                Ok(Class { fields, methods })
            },
            tokens => { Err(ParseError::ClassParseError(tokens.to_vec())) }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Method {
    name: String,
    implementation: Option<String>,
    inputs: Vec<Binding>,
    output: Type,
    body: Block
}

#[derive(Debug, PartialEq, Clone)]
pub struct Block(Vec<Stmt>, Option<Expr>);

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Let(Binding, Expr),
    Expr(Expr)
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Prefix(String, Expr),
    Infix(Expr, String, Expr),
    Postfix(Expr, String),
    Application(Expr, Vec<Expr>),
    Lit(Lit)
}

#[derive(Debug, PartialEq, Clone)]
struct Binding(String, Type);

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Bool,
    F32,
    Vec4,
    Mat4,
    Any
}

#[derive(Debug, PartialEq, Clone)]
pub enum Lit {
    F32(f32),
    Bool(bool)
}
