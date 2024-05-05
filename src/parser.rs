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
    methods: Vec<Method>,
    instance: Option<Instance>
}

impl Parse for Class {
    fn parse(pair: Pair<Rule>) -> Result<Self, ParseError> {
        assert_rule!(pair, class);

        let mut pairs = pair.into_inner();

        let mut fields = Vec::new();
        let mut methods = Vec::new();

        assert_pairs!(pairs, 1..);
        let name = pairs.next().unwrap().as_str().to_string();
        let mut instance = None;

        loop {
            let Some(pair) = pairs.next() else { break };

            if pair.as_rule() == Rule::instance {
                assert_pairs!(pairs, 0);
                instance = Some(Instance::parse(pair)?);
                break;
            }

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

        Ok(Self { name, fields, methods, instance })
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

        write!(f, "}}")?;

        if let Some(instance) = &self.instance {
            write!(f, " => {}", instance)?;
        }

        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Method {
    name: String,
    implementation: Option<String>,
    bounds: Vec<Bound>,
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

        let mut pair = pairs.next().unwrap();
        let mut bounds = Vec::new();

        if pair.as_rule() == Rule::bounds {

            for bound_pair in pair.into_inner() {
                let bound = Bound::parse(bound_pair)?;
                bounds.push(bound);
            }

            pair = pairs.next().unwrap();
        }

        let mut inputs = Vec::new();

        while pair.as_rule() == Rule::binding {
            let input = Binding::parse(pair)?;

            inputs.push(input);
            pair = pairs.next().unwrap();
        }

        let output = Type::parse(pair)?;

        assert_pairs!(pairs, 1..);
        let body = Block::parse(pairs.next().unwrap())?;

        Ok(Self { name, implementation, bounds, inputs, output, body })
    }
}

impl Display for Method {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(impl_name) = &self.implementation {
            write!(f, "{}::", impl_name)?;
        }

        write!(f, "{}", self.name)?;

        if self.bounds.len() > 0 {
            write!(f, "<")?;

            for (i, bound) in self.bounds.iter().enumerate() {
                write!(f, "{}", bound)?;

                if i < self.bounds.len() - 1 {
                    write!(f, ", ")?;
                }
            }

            write!(f, ">")?;
        }

        write!(f, "(")?;

        for (i, input) in self.inputs.iter().enumerate() {
            write!(f, "{}", input)?;

            if i < self.inputs.len() - 1 {
                write!(f, ", ")?;
            }
        }

        write!(f, ") -> {} ", self.output)?;

        write!(f, "{}", self.body)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Bound {
    name: String,
    impls: Vec<String>
}

impl Parse for Bound {
    fn parse(pair: Pair<Rule>) -> Result<Self, ParseError> {
        assert_rule!(pair, bound);
        let mut pairs = pair.into_inner();

        assert_pairs!(pairs, 2);

        let name = pairs.next().unwrap().as_str().to_string();
        let impls = pairs.map(|pair| pair.as_str().to_string()).collect();

        Ok(Self { name, impls })
    }
}

impl Display for Bound {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: ", self.name)?;

        for (i, impl_name) in self.impls.iter().enumerate() {
            write!(f, "{}", impl_name)?;

            if i < self.impls.len() - 1 {
                write!(f, " + ")?;
            }
        }

        Ok(())
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
        assert_rule!(pair, prec0 | unary | prec1 | prec2);
        let precedence = match pair.as_rule() {
            Rule::prec2 => 2,
            Rule::prec1 => 1,
            Rule::prec0 => 0,
            _ => 100
        };

        let mut pairs = pair.into_inner();
        assert_pairs!(pairs, 1..=3);

        Ok(match pairs.len() {
            1 => {
                let pair = pairs.next().unwrap();

                match pair.as_rule() {
                    Rule::prec0 | Rule::unary | Rule::prec1 | Rule::prec2 => {
                        Op::Solo(Box::new(Op::parse(pair)?))
                    },
                    _ => {
                        Op::Expr(Expr::parse(pair)?)
                    }
                }
            },
            2 => {
                let op = pairs.next().unwrap().as_str().to_string();
                let right = Op::parse(pairs.next().unwrap())?;

                Op::Unary(op, Box::new(right))
            },
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

#[derive(Debug, PartialEq, Clone)]
pub struct Instance {
    name: String,
    key_vals: Vec<KeyVal>
}

impl Parse for Instance {
    fn parse(pair: Pair<Rule>) -> Result<Self, ParseError> {
        assert_rule!(pair, instance);
        let mut pairs = pair.into_inner();

        assert_pairs!(pairs, 1..);
        let name = pairs.next().unwrap().as_str().to_string();

        let mut key_vals = Vec::new();

        for pair in pairs {
            key_vals.push(KeyVal::parse(pair)?);
        }

        Ok(Self { name, key_vals })
    }
}

impl Display for Instance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {{ ", self.name)?;

        for key_val in &self.key_vals {
            write!(f, "{}, ", key_val)?;

        }

        write!(f, "}}")
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct KeyVal {
    key: String,
    value: Value
}

impl Parse for KeyVal {
    fn parse(pair: Pair<Rule>) -> Result<Self, ParseError> {
        assert_rule!(pair, key_val);
        let mut pairs = pair.into_inner();

        assert_pairs!(pairs, 1 | 2);

        let key = pairs.next().unwrap().as_str().to_string();

        let value = if pairs.len() == 0 {
            Value::Expr(Expr::Var(key.clone()))
        } else {
            Value::parse(pairs.next().unwrap())?
        };

        Ok(Self { key, value })
    }
}

impl Display for KeyVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.key, self.value)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Expr(Expr),
    Instance(Instance)
}

impl Parse for Value {
    fn parse(pair: Pair<Rule>) -> Result<Self, ParseError> {
        match pair.as_rule() {
            Rule::expr => {
                Expr::parse(pair).map(Value::Expr)
            },
            Rule::instance => {
                Instance::parse(pair).map(Value::Instance)
            },
            rule => panic!("(value) Incorrect Rule: {:?}", rule)
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Expr(expr) => write!(f, "{}", expr),
            Self::Instance(instance) => write!(f, "{}", instance)
        }
    }
}

#[test]
fn test_pest() {
    let script =
        r#"
            class Sphere4D {
                radius: f32
            }
            class Shell {
                offset: f32,
                shape: Any,
                Proj::proj<shape: Proj>(vector: Vec4) -> Vec4 {
                    let proj: Vec4 = shape.proj(vector);

                    proj + offset * normalize(vector - proj)
                }
            }

            class ShellSphere {
                radius: f32
            } => Shell {
                offset: 3.0 - radius,
                shape: Sphere4D {
                    radius
                }
            }
        "#;

    let mut parsed1 = ScaffoldParser::parse(Rule::document, script).unwrap();
    let document1 = Document::parse(parsed1.next().unwrap()).unwrap();
    let string1 = prettify_string(format!("{document1}"));

    let mut parsed2 = ScaffoldParser::parse(Rule::document, string1.as_str()).unwrap();
    let document2 = Document::parse(parsed2.next().unwrap()).unwrap();
    // let string2 = prettify_string(format!("{document2}"));

    println!("{:#?}", document2);
}

fn prettify_string(mut string: String) -> String {
    let mut brace_depth = 0;
    let mut paren_depth = 0;
    let mut i = 1;
    let mut insert_newline = false;
    const TAB: &'static str = "    ";

    while i < string.len() {
        let owned = string[i..i+1].to_string();
        let slice = &owned[..];

        match slice {
            "{" => brace_depth += 1,
            "}" => {
                brace_depth -= 1;
                insert_newline = true;
            },
            "," | "=" => insert_newline = false,
            _ => {}
        };

        if paren_depth == 0 && insert_newline && slice != " " {
            if brace_depth == 0 && slice == "c" {
                string.insert_str(i, "\n");
                i += 1;
            }
            let inserted_str = format!("\n{}", TAB.repeat(brace_depth));
            string.insert_str(i, inserted_str.as_str());
            i += inserted_str.len();
        }

        match slice {
            "," => insert_newline = !insert_newline,
            "{" | "}" | ";" => insert_newline = true,
            "(" => paren_depth += 1,
            ")" => paren_depth -= 1,
            " " => {},
            _ => insert_newline = false
        };

        i += 1;
    }

    string
}