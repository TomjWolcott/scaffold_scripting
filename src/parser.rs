use crate::enviroment::Environment;
use glam::{Mat4, Vec4};
use pest::iterators::Pair;
use pest::Parser;
use pest_derive::Parser;
use std::any::Any;
use std::borrow::Borrow;
use std::fmt::Display;
use crate::any_value::AnyValue;

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
    fn parse(pair: Pair<Rule>, env: &Environment) -> Result<Self, ParseError>;
}

#[derive(Debug, PartialEq, Clone)]
pub enum ParseError {
    MultipleErrs(Rule, Vec<ParseError>),
    TypeNotFound(String),
    LitNotFound(String),
    BadRule(Rule, Vec<Rule>),
    IncorrectNumPairs(usize, String),
    BadPestParse(pest::error::Error<Rule>)
}

#[derive(Debug, PartialEq, Clone)]
pub struct Document {
    pub interfaces: Vec<Interface>,
    pub classes: Vec<Class>,
    pub functions: Vec<Function>
}

pub fn parse_document(script: impl AsRef<str>, env: &Environment) -> Result<Document, ParseError> {
    let mut parsed = ScaffoldParser::parse(Rule::document, script.as_ref())
        .map_err(ParseError::BadPestParse)?;

    Document::parse(parsed.next().unwrap(), env)
}

impl Document {
    pub fn new() -> Self {
        Self {
            interfaces: Vec::new(),
            classes: Vec::new(),
            functions: Vec::new()
        }
    }

    pub fn from_str(script: impl AsRef<str>, env: &Environment) -> Result<Self, ParseError> {
        let mut parsed = ScaffoldParser::parse(Rule::document, script.as_ref())
            .map_err(ParseError::BadPestParse)?;

        Self::parse(parsed.next().unwrap(), env)
    }

    /// New classes/interfaces overwrite old ones!!
    pub fn parse_and_merge(&mut self, pair: Pair<Rule>, env: &Environment) -> Result<(), ParseError> {
        let document = Document::parse(pair, env)?;

        self.merge(document);

        Ok(())
    }

    pub fn parse_and_merge_str(&mut self, script: impl AsRef<str>, env: &Environment) -> Result<(), ParseError> {
        let mut parsed = ScaffoldParser::parse(Rule::document, script.as_ref())
            .map_err(ParseError::BadPestParse)?;

        self.parse_and_merge(parsed.next().unwrap(), env)
    }

    pub fn merge(&mut self, other: Document) {
        let Document {
            classes: mut other_classes,
            interfaces: mut other_interfaces,
            functions: mut other_functions
        } = other;

        self.classes.retain(|Class { name, .. }| {
            !other_classes.iter().any(|Class { name: other_name, .. }| name == other_name)
        });

        self.interfaces.retain(|Interface { name, .. }| {
            !other_interfaces.iter().any(|Interface { name: other_name, .. }| name == other_name)
        });

        self.functions.retain(|function| {
            !other_functions.iter().any(|other_function| function.get_signature() == other_function.get_signature())
        });

        self.classes.append(&mut other_classes);
        self.interfaces.append(&mut other_interfaces);
        self.functions.append(&mut other_functions);
    }

    pub fn get_interface(&self, name: impl AsRef<str>) -> Option<&Interface> {
        self.interfaces.iter().find(
            |Interface { name: name2, .. }| name2.as_str() == name.as_ref()
        )
    }

    pub fn get_class(&self, name: impl AsRef<str>) -> Option<&Class> {
        self.classes.iter().find(
            |Class { name: name2, .. }| name2.as_str() == name.as_ref()
        )
    }

    /// Returns the method with key from the class or from a default implementation if the class implements it
    pub fn get_method(&self, name: impl AsRef<str>, method_key: &MethodKey) -> Option<&Method> {
        let class = self.get_class(name)?;

        if let Some(method) = class.get_method(method_key) {
            return Some(method);
        }

        let interface = self.get_interface(method_key.0.as_ref()?)?;

        class.get_implementations(interface)
            .map(|impls| {
                impls.into_iter().find(|Method { name, ..}| name == &method_key.1)
            }).flatten()
    }

    pub fn get_class_mut(&mut self, name: impl AsRef<str>) -> Option<&mut Class> {
        self.classes.iter_mut().find(
            |Class { name: name2, .. }| name2.as_str() == name.as_ref()
        )
    }

    pub fn get_method_mut(&mut self, name: impl AsRef<str>, method_key: &MethodKey) -> Option<&mut Method> {
        self.get_class_mut(name)?.get_method_mut(method_key)
    }
}

impl Parse for Document {
    fn parse(pair: Pair<Rule>, env: &Environment) -> Result<Self, ParseError> {
        assert_rule!(pair, document);

        let mut classes = Vec::new();
        let mut interfaces = Vec::new();
        let mut functions = Vec::new();

        for pair in pair.into_inner() {
            match pair.as_rule() {
                Rule::EOI => break,
                Rule::class => {
                    let class = Class::parse(pair, env)?;
                    classes.push(class);
                },
                Rule::interface => {
                    let interface = Interface::parse(pair, env)?;
                    interfaces.push(interface);
                },
                Rule::function => {
                    let function = Function::parse(pair, env)?;
                    functions.push(function);
                }
                rule => { panic!("(document) Incorrect Rule: {:?}", rule) }
            }
        }

        Ok(Self { classes, interfaces, functions })
    }
}

impl Display for Document {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for interface in &self.interfaces {
            write!(f, "{} ", interface)?;
        }

        for class in &self.classes {
            write!(f, "{} ", class)?;
        }

        for function in &self.functions {
            write!(f, "{} ", function)?;
        }

        Ok(())
    }
}


#[derive(Debug, PartialEq, Clone)]
pub struct Interface {
    pub name: String,
    pub methods: Vec<InterfaceMethod>
}

impl Interface {
    pub fn has_method(&self, method_name: impl AsRef<str>) -> bool {
        self.methods.iter().any(
            |method| method.header().0.as_str() == method_name.as_ref()
        )
    }

    pub fn get_method(&self, method_name: impl AsRef<str>) -> Option<&InterfaceMethod> {
        self.methods.iter().find(
            |method| method.header().0.as_str() == method_name.as_ref()
        )
    }
}

impl Parse for Interface {
    fn parse(pair: Pair<Rule>, env: &Environment) -> Result<Self, ParseError> {
        assert_rule!(pair, interface);

        let mut pairs = pair.into_inner();

        assert_pairs!(pairs, 1..);
        let name = pairs.next().unwrap().as_str().to_string();

        let mut methods = Vec::new();

        for method_pair in pairs {
            let method = InterfaceMethod::parse(method_pair, env)?;
            methods.push(method);
        }

        Ok(Self { name, methods })
    }
}

impl Display for Interface {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "interface {} {{ ", self.name)?;

        for method in &self.methods {
            write!(f, "{}, ", method)?;
        }

        write!(f, "}}")
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum InterfaceMethod {
    Header { name: String, inputs: Vec<Binding>, output: Type },
    DefaultImpl(Method),
}

impl InterfaceMethod {
    pub fn header(&self) -> (&String, &Vec<Binding>, &Type) {
        match self {
            Self::Header { name, inputs, output } => (name, inputs, output),
            Self::DefaultImpl(Method { name, inputs, output, .. }) => (name, inputs, output)
        }
    }
}

impl Parse for InterfaceMethod {
    fn parse(pair: Pair<Rule>, env: &Environment) -> Result<Self, ParseError> {
        assert_rule!(pair, method_header | default_method);

        let mut pairs = pair.into_inner();

        assert_pairs!(pairs, 1..);
        let name = pairs.next().unwrap().as_str().to_string();

        let mut inputs = Vec::new();

        let mut pair = pairs.next().unwrap();

        while pair.as_rule() == Rule::binding {
            let input = Binding::parse(pair, env)?;

            inputs.push(input);
            pair = pairs.next().unwrap();
        }

        let output = Type::parse(pair, env)?;

        if let Some(block_pair) = pairs.next() {
            let body = Block::parse(block_pair, env)?;

            Ok(Self::DefaultImpl(Method {
                name, implementation: None, bounds: vec![], inputs, output, body
            }))
        } else {
            Ok(Self::Header { name, inputs, output })
        }
    }
}

impl Display for InterfaceMethod {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Header { name, inputs, output } => {
                write!(f, "{}(", name)?;

                for (i, input) in inputs.iter().enumerate() {
                    write!(f, "{}", input)?;

                    if i < inputs.len() - 1 {
                        write!(f, ", ")?;
                    }
                }

                write!(f, ") -> {}", output)
            },
            Self::DefaultImpl(method) => {
                write!(f, "{}", method)
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Class {
    pub name: String,
    pub fields: Vec<Binding>,
    pub methods: Vec<Method>,
    pub instance: Option<Instance>
}

#[derive(Debug, Clone)]
pub struct MethodKey(pub Option<String>, pub String);

impl MethodKey {
    pub fn new(impl_name: Option<impl AsRef<str>>, name: impl AsRef<str>) -> Self {
        MethodKey(
            impl_name.map(|str| str.as_ref().to_string()),
            name.as_ref().to_string()
        )
    }
}

impl Display for MethodKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(impl_name) = &self.0 {
            write!(f, "{}::", impl_name)?;
        }

        write!(f, "{}", self.1)
    }
}

impl Class {
    pub fn get_method(&self, key: &MethodKey) -> Option<&Method> {
        self.methods.iter().find(|Method { implementation, name, .. }| {
            implementation == &key.0 && name == &key.1
        })
    }

    pub fn get_method_mut(&mut self, key: &MethodKey) -> Option<&mut Method> {
        self.methods.iter_mut().find(|Method { implementation, name, .. }| {
            implementation == &key.0 && name == &key.1
        })
    }

    pub fn get_impl(&self, impl_name: impl AsRef<str>, method_name: impl AsRef<str>) -> Option<&Method> {
        self.methods.iter().find(|Method { implementation, name, .. }| {
            implementation.as_ref().is_some_and(|s| s.as_str() == impl_name.as_ref()) &&
                name.as_str() == method_name.as_ref()
        })
    }

    pub fn get_implementation<'a>(&'a self, interface: &'a Interface, interface_method: &'a InterfaceMethod) -> Option<&'a Method> {
        let (
            name,
            inputs,
            output
        ) = interface_method.header();

        if let Some(method) = self.get_impl(&interface.name, &name) {
            if inputs == &method.inputs && output == &method.output {
                Some(method)
            } else {
                None
            }
        } else if let InterfaceMethod::DefaultImpl(method) = interface_method {
            Some(method)
        } else {
            None
        }
    }

    pub fn get_implementations<'a>(&'a self, interface: &'a Interface) -> Option<Vec<&'a Method>> {
        interface.methods.iter().map(|interface_method| {
            self.get_implementation(interface, interface_method)
        }).collect::<Option<Vec<_>>>()
    }
}

impl Parse for Class {
    fn parse(pair: Pair<Rule>, env: &Environment) -> Result<Self, ParseError> {
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
                instance = Some(Instance::parse(pair, env)?);
                break;
            }

            assert_rule!(pair, binding | method);

            match Binding::parse(pair.clone(), env) {
                Ok(field) => fields.push(field),
                Err(err1) => match Method::parse(pair, env) {
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
    pub name: String,
    pub implementation: Option<String>,
    pub bounds: Vec<Bound>,
    pub inputs: Vec<Binding>,
    pub output: Type,
    pub body: Block
}

impl Parse for Method {
    fn parse(pair: Pair<Rule>, env: &Environment) -> Result<Self, ParseError> {
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
                let bound = Bound::parse(bound_pair, env)?;
                bounds.push(bound);
            }

            pair = pairs.next().unwrap();
        }

        let mut inputs = Vec::new();

        while pair.as_rule() == Rule::binding {
            let input = Binding::parse(pair, env)?;

            inputs.push(input);
            pair = pairs.next().unwrap();
        }

        let output = Type::parse(pair, env)?;

        assert_pairs!(pairs, 1..);
        let body = Block::parse(pairs.next().unwrap(), env)?;

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
pub struct Function {
    pub name: String,
    pub inputs: Vec<Binding>,
    pub output: Type,
    pub body: Block
}

impl Function {
    fn get_signature(&self) -> (&String, Vec<&Type>) {
        (&self.name, self.inputs.iter().map(|Binding(_, ty)| ty).collect())
    }
}

impl Parse for Function {
    fn parse(pair: Pair<Rule>, env: &Environment) -> Result<Self, ParseError> {
        assert_rule!(pair, function);
        let mut pairs = pair.into_inner();

        assert_pairs!(pairs, 3..);
        let name_pair = pairs.next().unwrap();

        assert_rule!(name_pair, ident);
        let name = name_pair.as_str().to_string();

        let mut pair = pairs.next().unwrap();
        let mut bounds = Vec::new();

        if pair.as_rule() == Rule::bounds {
            for bound_pair in pair.into_inner() {
                let bound = Bound::parse(bound_pair, env)?;
                bounds.push(bound);
            }

            pair = pairs.next().unwrap();
        }

        let mut inputs = Vec::new();

        while pair.as_rule() == Rule::binding {
            let input = Binding::parse(pair, env)?;

            inputs.push(input);
            pair = pairs.next().unwrap();
        }

        let output = Type::parse(pair, env)?;

        assert_pairs!(pairs, 1..);
        let body = Block::parse(pairs.next().unwrap(), env)?;

        Ok(Self { name, inputs, output, body })
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn {}", self.name)?;

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
    pub name: String,
    pub impls: Vec<String>
}

impl Bound {
    /// Returns the FIRST interface with a method name that matches
    pub fn get_interface_with_method<'a>(&self, document: &'a Document, method_name: impl AsRef<str>) -> Option<&'a Interface> {
        for impl_name in self.impls.iter() {
            let Some(interface) = document.get_interface(impl_name) else { continue };

            if interface.has_method(&method_name) {
                return Some(interface);
            }
        }

        None
    }
}

impl Parse for Bound {
    fn parse(pair: Pair<Rule>, env: &Environment) -> Result<Self, ParseError> {
        assert_rule!(pair, bound);
        let mut pairs = pair.into_inner();

        assert_pairs!(pairs, 2..);

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


// Useful for tests where I'm just manipulating ASTs
pub fn parse_block(script: impl AsRef<str>, env: &Environment) -> Result<Block, ParseError> {
    let mut parsed = ScaffoldParser::parse(Rule::block_wrapper, script.as_ref()).unwrap();
    Block::parse(parsed.next().unwrap().into_inner().next().unwrap(), env)
}

#[derive(Debug, PartialEq, Clone)]
pub struct Block(pub Vec<Stmt>, pub Option<Expr>);

impl Parse for Block {
    fn parse(pair: Pair<Rule>, env: &Environment) -> Result<Self, ParseError> {
        assert_rule!(pair, block | block_expr);
        let pairs = pair.into_inner();

        let mut stmts = Vec::new();
        let mut expr = None;

        for pair in pairs {
            match pair.as_rule() {
                Rule::stmt => {
                    let stmt = Stmt::parse(pair, env)?;
                    stmts.push(stmt);
                },
                Rule::expr => {
                    let parsed_expr = Expr::parse(pair, env)?;
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

// stmt  = { (decl | asgn | ifelse | (expr ~ ";")) }
#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Declare(Binding, Expr),
    Assign(String, Expr),
    IfElse((Expr, Block), Vec<(Expr, Block)>, Option<Block>),
    Expr(Expr),
    Noop,
}

impl Parse for Stmt {
    fn parse(pair: Pair<Rule>, env: &Environment) -> Result<Self, ParseError> {
        assert_rule!(pair, stmt);
        let mut stmt_pairs = pair.into_inner();
        assert_pairs!(stmt_pairs, 1);
        let pair = stmt_pairs.next().unwrap();
        let rule = pair.as_rule();

        Ok(match rule {
            Rule::decl => {
                let mut pairs = pair.into_inner();
                assert_pairs!(pairs, 2);

                Stmt::Declare(
                    Binding::parse(pairs.next().unwrap(), env)?,
                    Expr::parse(pairs.next().unwrap(), env)?
                )
            },
            Rule::asgn => {
                let mut pairs = pair.into_inner();
                assert_pairs!(pairs, 2);

                Stmt::Assign(
                    pairs.next().unwrap().as_str().to_string(),
                    Expr::parse(pairs.next().unwrap(), env)?
                )
            },
            Rule::ifelse => {
                let mut pairs = pair.into_inner();
                assert_pairs!(pairs, 2..);

                let if_expr = Expr::parse(pairs.next().unwrap(), env)?;
                let if_block = Block::parse(pairs.next().unwrap(), env)?;
                let mut else_ifs = Vec::new();
                let mut else_block = None;

                while pairs.len() > 0 {
                    if pairs.len() > 1 {
                        else_ifs.push((
                            Expr::parse(pairs.next().unwrap(), env)?,
                            Block::parse(pairs.next().unwrap(), env)?
                        ));
                    } else {
                        else_block = Some(Block::parse(pairs.next().unwrap(), env)?);
                    }
                }

                Stmt::IfElse((if_expr, if_block), else_ifs, else_block)
            },
            Rule::expr => {
                Stmt::Expr(Expr::parse(pair, env)?)
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
            Self::IfElse((if_expr, if_block), else_ifs, else_block) => {
                write!(
                    f,
                    "if {} {} {} {}",
                    if_expr,
                    if_block,
                    else_ifs.iter().map(|(expr, block)| format!("{expr} {block}")).collect::<Vec<_>>().join(" "),
                    else_block.as_ref().map(|block| format!("else {block}")).unwrap_or("".to_string())
                )
            },
            Self::Expr(expr) => {
                write!(f, "{};", expr)
            },
            Self::Noop => {
                write!(f, "")
            }
        }
    }
}

pub fn parse_expr(script: impl AsRef<str>, env: &Environment) -> Result<Expr, ParseError> {
    let mut parsed = ScaffoldParser::parse(Rule::expr, script.as_ref()).unwrap();
    Expr::parse(parsed.next().unwrap(), env)
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    BinExpr(Box<Expr>, String, Box<Expr>),
    UnaryExpr(String, Box<Expr>),
    Application(String, Vec<Expr>),
    Dot(String, String, Vec<Expr>),
    Field(Box<Expr>, String),
    TupleAccess(Box<Expr>, usize),
    Tuple(Vec<Expr>),
    Var(String, Type),
    Lit(Lit),
    Block(Box<Block>)
}

impl Parse for Expr {
    fn parse(pair: Pair<Rule>, env: &Environment) -> Result<Self, ParseError> {
        assert_rule!(pair, expr | app | dot | field | tuple_access | tuple | var | lit | block_expr_wrapper);
        let rule = pair.as_rule();
        let mut expr_pairs = pair.into_inner();

        Ok(match rule {
            Rule::expr => {
                assert_pairs!(expr_pairs, 1);
                let p2_pair = expr_pairs.next().unwrap();

                Op::parse(p2_pair, env)?.to_expr()
            },
            Rule::app => {
                let name = expr_pairs.next().unwrap().as_str().to_string();
                let exprs_pair = expr_pairs.next().unwrap();
                let pairs = exprs_pair.into_inner();
                let mut args = Vec::new();

                for pair in pairs {
                    args.push(Expr::parse(pair, env)?);
                }

                Expr::Application(name, args)
            },
            Rule::tuple => {
                let mut elements = Vec::new();

                for pair in expr_pairs {
                    elements.push(Expr::parse(pair, env)?);
                }

                Expr::Tuple(elements)
            },
            Rule::dot => {
                let name = expr_pairs.next().unwrap().as_str().to_string();
                let method = expr_pairs.next().unwrap().as_str().to_string();
                let exprs_pair = expr_pairs.next().unwrap();
                let pairs = exprs_pair.into_inner();
                let mut args = Vec::new();

                for pair in pairs {
                    args.push(Expr::parse(pair, env)?);
                }

                Expr::Dot(name, method, args)
            },
            Rule::field => {
                let expr = Expr::parse(expr_pairs.next().unwrap(), env)?;
                let field = expr_pairs.next().unwrap().as_str().to_string();

                Expr::Field(Box::new(expr), field)
            },
            Rule::tuple_access => {
                let expr = Expr::parse(expr_pairs.next().unwrap(), env)?;
                let index = expr_pairs.next().unwrap().as_str().parse().unwrap();

                Expr::TupleAccess(Box::new(expr), index)
            },
            Rule::var => {
                Expr::Var(expr_pairs.next().unwrap().as_str().to_string(), Type::Auto)
            },
            Rule::lit => {
                Expr::Lit(Lit::parse(expr_pairs.next().unwrap(), env)?)
            },
            Rule::block_expr_wrapper => {
                Expr::Block(Box::new(Block::parse(expr_pairs.next().unwrap(), env)?))
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
            Self::Field(name, field) => {
                write!(f, "{}.{}", name, field)
            },
            Self::TupleAccess(expr, index) => {
                write!(f, "{}.{}", expr, index)
            },
            Self::Tuple(elements) => {
                write!(f, "(")?;

                for (i, element) in elements.iter().enumerate() {
                    write!(f, "{}", element)?;

                    if i < elements.len() - 1 || i == 0 {
                        write!(f, ", ")?;
                    }
                }

                write!(f, ")")
            },
            Self::Var(name, _) => {
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

// Intermediate step for ensuring left-recursive order of ops
enum Op {
    Expr(Expr),
    Solo(Box<Op>),
    Unary(String, Box<Op>),
    Binop(usize, Expr, String, Box<Op>)
}

impl Parse for Op {
    fn parse(pair: Pair<Rule>, env: &Environment) -> Result<Self, ParseError> {
        assert_rule!(pair, prec0 | unary | prec1 | prec2 | prec3 | prec4);
        let precedence = match pair.as_rule() {
            Rule::prec4 => 4,
            Rule::prec3 => 3,
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
                    Rule::prec0 | Rule::unary | Rule::prec1 | Rule::prec2 | Rule::prec3 | Rule::prec4 => {
                        Op::Solo(Box::new(Op::parse(pair, env)?))
                    },
                    _ => {
                        Op::Expr(Expr::parse(pair, env)?)
                    }
                }
            },
            2 => {
                let op = pairs.next().unwrap().as_str().to_string();
                let right = Op::parse(pairs.next().unwrap(), env)?;

                Op::Unary(op, Box::new(right))
            },
            3 => {
                let left = Op::parse(pairs.next().unwrap(), env)?;
                let op = pairs.next().unwrap().as_str().to_string();
                let right = Op::parse(pairs.next().unwrap(), env)?;

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
pub struct Binding(pub String, pub Type);

impl Parse for Binding {
    fn parse(pair: Pair<Rule>, env: &Environment) -> Result<Self, ParseError> {
        match pair.as_rule() {
            Rule::binding => {
                let mut pairs = pair.into_inner();

                assert_pairs!(pairs, 1..=2);

                let name = pairs.next().unwrap().as_str().to_string();

                let ty = Type::parse(pairs.next().unwrap(), env)?;

                Ok(Self(name, ty))
            }
            Rule::ident => {
                Ok(Self(pair.as_str().to_string(), Type::Auto))
            }
            rule => Err(ParseError::BadRule(rule, vec![Rule::binding, Rule::ident]))
        }
    }
}

impl Display for Binding {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.0, self.1)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Type {
    Bool,
    F32,
    Vec4,
    Mat4x4,
    Class,
    Auto,
    Unit,
    Tuple(Vec<Type>),
    Custom(String)
}

impl Parse for Type {
    fn parse(pair: Pair<Rule>, env: &Environment) -> Result<Self, ParseError> {
        assert_rule!(pair, ty);

        match pair.as_str() {
            "bool" => Ok(Self::Bool),
            "f32" => Ok(Self::F32),
            "vec4" => Ok(Self::Vec4),
            "mat4x4" => Ok(Self::Mat4x4),
            "Class" => Ok(Self::Class),
            "()" => Ok(Self::Unit),
            ty => {
                let mut pairs = pair.into_inner();

                if pairs.len() == 1 && pairs.peek().unwrap().as_rule() == Rule::ident {
                    let type_name = pairs.next().unwrap().as_str().to_string();

                    if env.type_name_exists(&type_name) {
                        Ok(Self::Custom(type_name))
                    } else {
                        Err(ParseError::TypeNotFound(type_name))
                    }
                } else {
                    let tuple_types = pairs
                        .map(|pair| Type::parse(pair, env))
                        .collect::<Result<Vec<Type>, _>>()?;

                    if tuple_types.len() > 0 {
                        Ok(Self::Tuple(tuple_types))
                    } else {
                        Err(ParseError::TypeNotFound(ty.to_string()))
                    }
                }
            }
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool => write!(f, "bool"),
            Self::F32 => write!(f, "f32"),
            Self::Vec4 => write!(f, "Vec4"),
            Self::Mat4x4 => write!(f, "Mat4x4"),
            Self::Class => write!(f, "Class"),
            Self::Auto => write!(f, "Auto"),
            Self::Unit => write!(f, "()"),
            Self::Tuple(elem_types) => {
                write!(f, "(")?;

                for (i, element) in elem_types.iter().enumerate() {
                    write!(f, "{}", element)?;

                    if i < elem_types.len() - 1 || i == 0 {
                        write!(f, ", ")?;
                    }
                }

                write!(f, ")")
            },
            Self::Custom(ty) => write!(f, "{ty}")
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Lit {
    F32(f32),
    Bool(bool),
    Vec4(Vec4),
    Mat4x4(Mat4),
    Tuple(Vec<Lit>),
    Custom(Box<dyn AnyValue>, String),
    Unit
}

impl Lit {
    pub fn get_type(&self) -> Type {
        match self {
            Self::F32(_) => Type::F32,
            Self::Bool(_) => Type::Bool,
            Self::Vec4(_) => Type::Vec4,
            Self::Mat4x4(_) => Type::Mat4x4,
            Self::Tuple(elements) => Type::Tuple(elements.iter().map(|lit| lit.get_type()).collect()),
            Self::Custom(_, name) => Type::Custom(name.clone()),
            Self::Unit => Type::Unit
        }
    }

    pub fn matches_type(&self, other: &Self) -> bool {
        self.get_type() == other.get_type()
    }
}

impl Parse for Lit {
    fn parse(pair: Pair<Rule>, env: &Environment) -> Result<Self, ParseError> {
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
            Self::Bool(b) => write!(f, "{}", b),
            Self::Vec4(vec) => write!(f, "{}", vec),
            Self::Mat4x4(mat) => write!(f, "{}", mat),
            Self::Unit => write!(f, "()"),
            Self::Tuple(elements) => {
                write!(f, "(")?;

                for (i, element) in elements.iter().enumerate() {
                    write!(f, "{}", element)?;

                    if i < elements.len() - 1 || i == 0 {
                        write!(f, ", ")?;
                    }
                }

                write!(f, ")")
            },
            _ => {
                // Custom types are not directly displayable
                write!(f, "<custom type>")
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Instance {
    pub name: String,
    pub key_vals: Vec<KeyVal>
}

impl Parse for Instance {
    fn parse(pair: Pair<Rule>, env: &Environment) -> Result<Self, ParseError> {
        assert_rule!(pair, instance);
        let mut pairs = pair.into_inner();

        assert_pairs!(pairs, 1..);
        let name = pairs.next().unwrap().as_str().to_string();

        let mut key_vals = Vec::new();

        for pair in pairs {
            key_vals.push(KeyVal::parse(pair, env)?);
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
    pub key: String,
    pub value: Value
}

impl Parse for KeyVal {
    fn parse(pair: Pair<Rule>, env: &Environment) -> Result<Self, ParseError> {
        assert_rule!(pair, key_val);
        let mut pairs = pair.into_inner();

        assert_pairs!(pairs, 1 | 2);

        let key = pairs.next().unwrap().as_str().to_string();

        let value = if pairs.len() == 0 {
            Value::Expr(Expr::Var(key.clone(), Type::Auto))
        } else {
            Value::parse(pairs.next().unwrap(), env)?
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
    fn parse(pair: Pair<Rule>, env: &Environment) -> Result<Self, ParseError> {
        match pair.as_rule() {
            Rule::expr => {
                Expr::parse(pair, env).map(Value::Expr)
            },
            Rule::instance => {
                Instance::parse(pair, env).map(Value::Instance)
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

#[cfg(test)]
mod tests {
    use crate::enviroment::Environment;
    use crate::interpreter::Eval;
    use crate::parser::{parse_block, Document, Parse, Rule, ScaffoldParser};
    use crate::test_helpers;
    use pest::Parser;
    use crate::scope::Scope;

    #[test]
    fn test_pest() {
        let script =
            r#"
            interface Sdf {
                sdf(vector: vec4) -> f32,
                grad(vector: vec4) -> vec4 {
                    abc
                }
            }

            class Sphere4D {
                radius: f32
            }
            class Shell {
                offset: f32,
                shape: Class,
                Proj::proj<shape: Proj>(vector: vec4) -> vec4 {
                    let proj_2938sdc: vec4 = shape.proj(vector);

                    proj_2938sdc + offset * normalize(vector - proj_2938sdc)
                },
                Sdf::sdf<shape: Sdf>(vector: vec4) -> f32 {
                    abs(shape.sdf(vector)) - offset
                },
                Sdf::grad(vector: vec4) -> vec4 {
                    def
                }
            }

            interface Proj {
                proj(vector: vec4) -> vec4
            }

            class ShellSphere {
                radius: f32
            } => Shell {
                offset: 3.0 - radius,
                shape: Sphere4D {
                    radius
                }
            }

            fn abc(xyz: vec4, qqq: f32) -> (f64, f32) {
                let qyz = 3.0;

                if (abc > 3.0) {
                    s = 4.0;
                } else if (abc > 2.0) {
                    s = 3.0;
                } else {
                    s = 3.0;
                }

                (s, 3)
            }
        "#;

        let mut env = Environment::new();
        env.register_type::<f64>("f64".into());

        let mut parsed = ScaffoldParser::parse(Rule::document, script).unwrap();
        let document = Document::parse(parsed.next().unwrap(), &env).unwrap();
        let string = test_helpers::prettify_string(format!("{document}"));
        let implementations = document
            .get_class("Shell")
            .unwrap()
            .get_implementations(document.get_interface("Sdf").unwrap());

        println!("impls:\n{}\n\n", implementations.unwrap().iter().map(|x| format!("{x}")).collect::<Vec<_>>().join("\n"));

        println!("{}", string);
    }

    #[test]
    fn tuple_test() {
        let script = r#"{
            let a: (f32, f32) = (1.0, 2.0);
            let b: (f32, (f32, f32), mat4x4) = (1.0, (1 + 3, -.1 + 8), 3 * mat4x4(X, Z, Y, W));
            let c: (f32, vec4, f32, f32) = (1.0, vec4(1, 3, 2, 1) / 8, 3.0, 4.0);
            b.1
        }"#;

        let block = parse_block(script, &Environment::new()).unwrap();
        let string = test_helpers::prettify_string(format!("{block}"));
        let mut env = Environment::new();

        println!("{}\nwhich returns: {:?}", string, block.eval(&mut Scope::new(), &env));
    }
}