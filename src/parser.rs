use std::fmt::Display;
use pest::iterators::Pair;
use pest_derive::Parser;
use pest::Parser;

macro_rules! assert_rule {
    ($pair:expr, $($rule:ident)|*) => {
        // debug_assert!(
        //     $($pair .as_rule() == Rule:: $rule || )* false,
        //     "Expected rules: {:?}, found rule: {:?}",
        //     &["" $(, stringify!($rule))*][1..], $pair .as_rule()
        // )
        match $pair.as_rule() {
            $(Rule::$rule)|* => {},
            rule => {
                return Err(ParseError::BadRule(rule, vec![ $(Rule::$rule),* ]));
            }
        }
    };
}

macro_rules! assert_pairs {
    ($pairs:expr, $range:pat) => {
        // debug_assert!(match $pairs .len() {
        //     $range => true,
        //     _ => false
        // }, "Incorrect pairs.len(), {} is not is range {:?}", $pairs .len(), stringify!($range))
        match $pairs.len() {
            $range => {},
            len => {
                return Err(ParseError::IncorrectNumPairs(len, stringify!($range).to_string()));
            }
        }
    };
}


#[derive(Parser)]
#[grammar = "assets/csl_grammar.pest"]
struct ScaffoldParser;

trait Parse where Self: Sized {
    fn parse(pair: Pair<Rule>) -> Result<Self, ParseError>;
}

#[derive(Debug, PartialEq, Clone)]
pub enum ParseError {
    MultipleErrs(Rule, Vec<ParseError>),
    TypeNotFound(String),
    LitNotFound(String),
    BadRule(Rule, Vec<Rule>),
    IncorrectNumPairs(usize, String)
}

#[derive(Debug, PartialEq, Clone)]
pub struct Document {
    classes: Vec<Class>
}

impl Parse for Document {
    fn parse(pair: Pair<Rule>) -> Result<Self, ParseError> {
        assert_rule!(pair, document);

        let classes_result = pair.into_inner()
            .map(|class_pair| Class::parse(class_pair))
            .collect::<Result<Vec<Class>, ParseError>>();

        classes_result.map(|classes| Self { classes })
    }
}

impl Display for Document {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for class in &self.classes {
            write!(f, "{} ", class)?;
        }

        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Class {
    name: String,
    fields: Vec<Binding>,
    methods: Vec<Method>
}

impl Parse for Class {
    fn parse(pair: Pair<Rule>) -> Result<Self, ParseError> {
        assert_rule!(pair, class);

        let mut pairs = pair.into_inner();

        let mut fields = Vec::new();
        let mut methods = Vec::new();

        assert_pairs!(pairs, 1..);
        let name = pairs.next().unwrap().as_str().to_string();

        for pair in pairs {
            assert_rule!(pair, binding | method);

            match Binding::parse(pair.clone()) {
                Ok(field) => fields.push(field),
                Err(err1) => match Method::parse(pair) {
                    Ok(method) => methods.push(method),
                    Err(err2) => {
                        return Err(ParseError::MultipleErrs(Rule::class, vec![err1, err2]))
                    }
                }
            };
        }

        Ok(Self { name, fields, methods })
    }
}

impl Display for Class {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "class {} {{ ", self.name)?;

        for field in &self.fields {
            write!(f, "{}, ", field)?;
        }

        for method in &self.methods {
            write!(f, "{}, ", method)?;
        }

        write!(f, "}}")
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

impl Parse for Method {
    fn parse(pair: Pair<Rule>) -> Result<Self, ParseError> {
        assert_rule!(pair, method);
        let mut pairs = pair.into_inner();

        assert_pairs!(pairs, 3..);
        let name_pair = pairs.next().unwrap();

        let (name, implementation) = match name_pair.as_rule() {
            Rule::implement => {
                let mut impl_pairs = name_pair.into_inner();

                let implementation = impl_pairs.next().unwrap().as_str().to_string();
                let name = impl_pairs.next().unwrap().as_str().to_string();

                (name, Some(implementation))
            },
            Rule::ident => {
                (name_pair.as_str().to_string(), None)
            },
            rule => { panic!("(method) Incorrect Rule: {:?}", rule) }
        };

        let mut inputs = Vec::new();
        let mut pair = pairs.next().unwrap();

        while pair.as_rule() == Rule::binding {
            let input = Binding::parse(pair)?;

            inputs.push(input);
            pair = pairs.next().unwrap();
        }

        let output = Type::parse(pair)?;

        assert_pairs!(pairs, 1..);
        let body = Block::parse(pairs.next().unwrap())?;

        Ok(Self { name, implementation, inputs, output, body })
    }
}

impl Display for Method {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(impl_name) = &self.implementation {
            write!(f, "{}::", impl_name)?;
        }

        write!(f, "{}", self.name)?;

        write!(f, "(")?;

        for (i, input) in self.inputs.iter().enumerate() {
            write!(f, "{}", input)?;

            if i < self.inputs.len() - 1 {
                write!(f, ", ")?;
            }
        }

        write!(f, ") -> {}", self.output)?;

        write!(f, "{}", self.body)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Block(Vec<Stmt>, Option<Expr>);

impl Parse for Block {
    fn parse(pair: Pair<Rule>) -> Result<Self, ParseError> {
        assert_rule!(pair, block);
        let pairs = pair.into_inner();

        let mut stmts = Vec::new();
        let mut expr = None;

        for pair in pairs {
            match pair.as_rule() {
                Rule::stmt => {
                    let stmt = Stmt::parse(pair)?;
                    stmts.push(stmt);
                },
                Rule::expr => {
                    let parsed_expr = Expr::parse(pair)?;
                    expr = Some(parsed_expr);
                },
                rule => panic!("(block) Incorrect Rule: {:?}", rule)
            }
        }

        Ok(Self(stmts, expr))
    }
}

impl Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{ ")?;
        for stmt in &self.0 {
            write!(f, "{} ", stmt)?;
        }

        if let Some(expr) = &self.1 {
            write!(f, "{}", expr)?;
        }

        write!(f, " }}")?;

        Ok(())
    }
}

// stmt  = { (decl | asgn | (expr ~ ";")) }
#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Declare(Binding, Expr),
    Assign(String, Expr),
    Expr(Expr)
}

impl Parse for Stmt {
    fn parse(stmt_pair: Pair<Rule>) -> Result<Self, ParseError> {
        assert_rule!(stmt_pair, stmt);
        let mut stmt_pairs = stmt_pair.into_inner();
        assert_pairs!(stmt_pairs, 1);
        let pair = stmt_pairs.next().unwrap();
        let rule = pair.as_rule();

        Ok(match rule {
            Rule::decl => {
                let mut pairs = pair.into_inner();
                assert_pairs!(pairs, 2);

                Stmt::Declare(
                    Binding::parse(pairs.next().unwrap())?,
                    Expr::parse(pairs.next().unwrap())?
                )
            },
            Rule::asgn => {
                let mut pairs = pair.into_inner();
                assert_pairs!(pairs, 2);

                Stmt::Assign(
                    pairs.next().unwrap().as_str().to_string(),
                    Expr::parse(pairs.next().unwrap())?
                )
            },
            Rule::expr => {
                Stmt::Expr(Expr::parse(pair)?)
            },
            _ => panic!("(stmt) Incorrect Rule: {:?}", rule)
        })
    }
}

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Declare(binding, expr) => {
                write!(f, "let {} = {};", binding, expr)
            },
            Self::Assign(name, expr) => {
                write!(f, "{} = {};", name, expr)
            },
            Self::Expr(expr) => {
                write!(f, "{}", expr)
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    BinExpr(Box<Expr>, String, Box<Expr>),
    UnaryExpr(String, Box<Expr>),
    Application(String, Vec<Expr>),
    Dot(String, String, Vec<Expr>),
    Var(String),
    Lit(Lit),
    Block(Box<Block>)
}

impl Parse for Expr {
    fn parse(expr_pair: Pair<Rule>) -> Result<Self, ParseError> {
        assert_rule!(expr_pair, expr | app | dot | var | lit | block_expr);
        let rule = expr_pair.as_rule();
        let mut expr_pairs = expr_pair.into_inner();

        Ok(match rule {
            Rule::expr => {
                assert_pairs!(expr_pairs, 1);
                let p2_pair = expr_pairs.next().unwrap();

                Op::parse(p2_pair)?.to_expr()
            },
            Rule::app => {
                let name = expr_pairs.next().unwrap().as_str().to_string();
                let exprs_pair = expr_pairs.next().unwrap();
                let pairs = exprs_pair.into_inner();
                let mut args = Vec::new();

                for pair in pairs {
                    args.push(Expr::parse(pair)?);
                }

                Expr::Application(name, args)
            },
            Rule::dot => {
                let name = expr_pairs.next().unwrap().as_str().to_string();
                let method = expr_pairs.next().unwrap().as_str().to_string();
                let exprs_pair = expr_pairs.next().unwrap();
                let pairs = exprs_pair.into_inner();
                let mut args = Vec::new();

                for pair in pairs {
                    args.push(Expr::parse(pair)?);
                }

                Expr::Dot(name, method, args)
            },
            Rule::var => {
                Expr::Var(expr_pairs.next().unwrap().as_str().to_string())
            },
            Rule::lit => {
                Expr::Lit(Lit::parse(expr_pairs.next().unwrap())?)
            },
            Rule::block_expr => {
                Expr::Block(Box::new(Block::parse(expr_pairs.next().unwrap())?))
            },
            _ => panic!("(expr) Incorrect Rule: {:?}", rule)
        })
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::BinExpr(left, op, right) => {
                write!(f, "({} {} {})", left, op, right)
            },
            Self::UnaryExpr(op, expr) => {
                write!(f, "{}{}", op, expr)
            },
            Self::Application(name, args) => {
                write!(f, "{}(", name)?;

                for (i, arg) in args.iter().enumerate() {
                    write!(f, "{}", arg)?;

                    if i < args.len() - 1 {
                        write!(f, ", ")?;
                    }
                }

                write!(f, ")")
            },
            Self::Dot(name, method, args) => {
                write!(f, "{}.{}(", name, method)?;

                for (i, arg) in args.iter().enumerate() {
                    write!(f, "{}", arg)?;

                    if i < args.len() - 1 {
                        write!(f, ", ")?;
                    }
                }

                write!(f, ")")
            },
            Self::Var(name) => {
                write!(f, "{}", name)
            },
            Self::Lit(lit) => {
                write!(f, "{}", lit)
            },
            Self::Block(block) => {
                write!(f, "{}", block)
            }
        }
    }
}

enum Op {
    Expr(Expr),
    Solo(Box<Op>),
    Unary(String, Box<Op>),
    Binop(usize, Expr, String, Box<Op>)
}

impl Parse for Op {
    fn parse(pair: Pair<Rule>) -> Result<Self, ParseError> {
        assert_rule!(pair, prec0 | prec1 | prec2);
        let precedence = match pair.as_rule() {
            Rule::prec2 => 2,
            Rule::prec1 => 1,
            Rule::prec0 => 0,
            rule => panic!("(op) Incorrect Rule: {:?}", rule)
        };

        let mut pairs = pair.into_inner();
        assert_pairs!(pairs, 1..=3);

        Ok(match pairs.len() {
            1 => {
                let pair = pairs.next().unwrap();

                match pair.as_rule() {
                    Rule::prec0 | Rule::prec1 | Rule::prec2 => {
                        Op::Solo(Box::new(Op::parse(pair)?))
                    },
                    _ => {
                        Op::Expr(Expr::parse(pair)?)
                    }
                }
            },
            2 => {panic!("")},
            3 => {
                let left = Op::parse(pairs.next().unwrap())?;
                let op = pairs.next().unwrap().as_str().to_string();
                let right = Op::parse(pairs.next().unwrap())?;

                Op::Binop(precedence, left.to_expr(), op, Box::new(right))
            },
            _ => {panic!("")}
        })
    }
}

impl Op {
    fn to_expr(self) -> Expr {
        match self {
            Self::Expr(expr) => expr,
            Self::Solo(expr) => expr.to_expr(),
            Self::Unary(op, expr) =>
                Expr::UnaryExpr(op, Box::new(expr.to_expr())),
            Self::Binop(p1, expr1, op1, expr2) => {
                match *expr2 {
                    Self::Binop(
                        p2, expr3, op2, expr4
                    ) if p1 == p2 => {
                        let left = Expr::BinExpr(Box::new(expr1), op1, Box::new(expr3));

                        Self::Binop(p2, left, op2, expr4).to_expr()
                    },
                    _ => Expr::BinExpr(Box::new(expr1), op1, Box::new(expr2.to_expr()))
                }
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Binding(String, Type);

impl Parse for Binding {
    fn parse(pair: Pair<Rule>) -> Result<Self, ParseError> {
        assert_rule!(pair, binding);
        let mut pairs = pair.into_inner();

        assert_pairs!(pairs, 2);

        let name = pairs.next().unwrap().as_str().to_string();

        let ty_pair = pairs.next().unwrap();

        let ty = Type::parse(ty_pair)?;

        Ok(Self(name, ty))
    }
}

impl Display for Binding {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.0, self.1)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Bool,
    F32,
    Vec4,
    Mat4,
    Any
}

impl Parse for Type {
    fn parse(pair: Pair<Rule>) -> Result<Self, ParseError> {
        assert_rule!(pair, ty);

        match pair.as_str() {
            "bool" => Ok(Self::Bool),
            "f32" => Ok(Self::F32),
            "Vec4" => Ok(Self::Vec4),
            "Mat4" => Ok(Self::Mat4),
            "Any" => Ok(Self::Any),
            ty => Err(ParseError::TypeNotFound(ty.to_string()))
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool => write!(f, "bool"),
            Self::F32 => write!(f, "f32"),
            Self::Vec4 => write!(f, "Vec4"),
            Self::Mat4 => write!(f, "Mat4"),
            Self::Any => write!(f, "Any")
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Lit {
    F32(f32),
    Bool(bool)
}

impl Parse for Lit {
    fn parse(pair: Pair<Rule>) -> Result<Self, ParseError> {
        // assert_rule!(pair, lit);

        match pair.as_str() {
            "true" => Ok(Self::Bool(true)),
            "false" => Ok(Self::Bool(false)),
            lit => {
                match lit.parse::<f32>() {
                    Ok(num) => Ok(Self::F32(num)),
                    Err(_) => Err(ParseError::LitNotFound(lit.to_string()))
                }
            }
        }
    }
}

impl Display for Lit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::F32(num) => write!(f, "{}", num),
            Self::Bool(b) => write!(f, "{}", b)
        }
    }
}

#[test]
fn test_pest() {
    let script =
        r#"
            class Sphere4D {
                test(abc: f32, xyz: Mat4) -> bool {
                    // (((abc - 3) - (((2 / 8) / 3) * 2)) + 1)
                    abc - 3 - 2 / 8 / 3 * 2 + 1
                }
            }
            class Shell {
                offset: f32,
                shape: Any,
                Proj::proj(vector: Vec4) -> Vec4 {
                    let proj: Vec4 = shape.proj(vector);

                    proj + offset * normalize(vector - proj)
                }
            }
        "#;

    let mut parsed = ScaffoldParser::parse(Rule::document, script).unwrap();

    let document1 = Document::parse(parsed.next().unwrap()).unwrap();

    println!("{}", document1);
}
