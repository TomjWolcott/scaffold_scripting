use crate::enviroment::{Environment, SslIdentifier};
use glam::{Mat4, Vec4};
use pest::iterators::Pair;
use pest::Parser;
use pest_derive::Parser;
use std::any::Any;
use std::borrow::Borrow;
use std::fmt::{Debug, Display, Formatter};
use std::ops::Deref;
use std::path::PathBuf;
use std::sync::Arc;
use crate::any_value::AnyValue;
use crate::ast_operations::{gen_ident, AssignTypes};
use crate::interpreter::Eval;
use crate::prelude::TreeNodeMut;
use crate::scope::Scope;
use crate::tree_walk::{Options, WalkTreeMut};
use anyhow::{anyhow, Context, Result as AnyResult};
use crate::define_span_wrapper;
use crate::parser_span::{SslScript, SslSpan};

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
    fn parse(pair: Pair<Rule>, env: &Environment, script: &Arc<SslScript>) -> Result<Self, ParseError>;
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
    pub functions: Vec<Function>,
    pub consts: Vec<Constant>
}

pub fn parse_document(script_str: impl AsRef<str>, path_opt: Option<PathBuf>, env: &Environment) -> Result<Document, ParseError> {
    let script = Arc::new(SslScript::new(script_str.as_ref().to_string(), path_opt));

    let mut parsed = ScaffoldParser::parse(Rule::document, script.source.as_str())
        .map_err(ParseError::BadPestParse)?;

    Document::parse(parsed.next().unwrap(), env, &script)
}

#[derive(Debug, Clone)]
pub enum DocumentItem {
    Function(Function),
    Constant(Constant)
}

impl Document {
    pub fn new() -> Self {
        Self {
            interfaces: Vec::new(),
            classes: Vec::new(),
            functions: Vec::new(),
            consts: Vec::new()
        }
    }

    pub fn from_str(script_str: impl AsRef<str>, path_opt: Option<PathBuf>, env: &Environment) -> Result<Self, ParseError> {
        let script = Arc::new(SslScript::new(script_str.as_ref().to_string(), path_opt));

        let mut parsed = ScaffoldParser::parse(Rule::document, script.source.as_str())
            .map_err(ParseError::BadPestParse)?;

        Document::parse(parsed.next().unwrap(), env, &script)
    }

    pub fn parse_and_merge_str(&mut self, script_str: impl AsRef<str>, path_opt: Option<PathBuf>, env: &Environment) -> Result<(), ParseError> {
        let document = Document::from_str(script_str, path_opt, env)?;

        self.merge(document);

        Ok(())
    }

    pub fn merge(&mut self, other: Document) {
        let Document {
            classes: mut other_classes,
            interfaces: mut other_interfaces,
            functions: mut other_functions,
            consts: mut other_consts
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

        self.consts.retain(|constant| {
            !other_consts.iter().any(|other_constant| constant.0 == other_constant.0)
        });

        self.classes.append(&mut other_classes);
        self.interfaces.append(&mut other_interfaces);
        self.functions.append(&mut other_functions);
        self.consts.append(&mut other_consts);
    }

    pub fn add_to_environment(&self, env: &mut Environment) -> AnyResult<()> {
        let mut document_items = self.functions.iter()
            .map(|f| (false, DocumentItem::Function(f.clone())))
            .chain(self.consts.iter().map(|c| (false, DocumentItem::Constant(c.clone()))))
            .collect::<Vec<_>>();

        if document_items.len() == 0 { return Ok(()) };

        let mut something_changed_last_cycle = true;
        let mut i = 0;
        let num_items = document_items.len();
        let mut errs = Vec::new();

        while i > 0 || something_changed_last_cycle {
            let (flag, item) = &mut document_items[i];
            println!("something_changed: {something_changed_last_cycle}, flag: {flag}, item: {item:?}");

            if i == 0 {
                something_changed_last_cycle = false;
                errs = Vec::new();
            }

            i = (i + 1) % num_items;
            if *flag { continue };

            match item {
                DocumentItem::Constant(Constant(Binding(name, _), expr)) => {
                    if let Err(err) = expr.assign_types(env) {
                        errs.push((item.clone(), err));
                    } else {
                        match expr.eval(&mut Scope::new(), env) {
                            Ok(lit) => {
                                env.register_const(SslIdentifier::new(&name), lit);
                                something_changed_last_cycle = true;
                                *flag = true;
                            }
                            Err(err) => {
                                errs.push((item.clone(), err));
                            }
                        }
                    }
                }
                DocumentItem::Function(f) => {
                    if env.get_fn(&f.name, f.input_types()).is_none() {
                        env.insert_signature(f);
                        something_changed_last_cycle = true;
                    }

                    if let Err(err) = f.assign_types(env) {
                        errs.push((item.clone(), err));
                    } else {
                        env.insert_ssl_fn(f.clone());
                        something_changed_last_cycle = true;
                        *flag = true;
                    }
                }
            }
        }

        env.remove_signatures();

        if errs.len() > 0 {
            let mut err_string = "Could not fully compile constants and functions into env.".to_string();

            for (i, (item, err)) in errs.into_iter().enumerate() {
                err_string = match item {
                    DocumentItem::Constant(Constant(Binding(name, _), _)) =>
                        format!("{err_string}\n  [{i}]: Error on constant {name}\n    {}", format!("{err:?}").replace("\n", "\n    ")),
                    DocumentItem::Function(f) =>
                        format!("{err_string}\n  [{i}]: Error on function {}\n    {}", f.get_signature_string(), format!("{err:?}").replace("\n", "\n    ")),
                }
            }

            Err(anyhow!("{err_string}"))
        } else {
            Ok(())
        }
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
    fn parse(pair: Pair<Rule>, env: &Environment, script: &Arc<SslScript>) -> Result<Self, ParseError> {
        assert_rule!(pair, document);

        let mut classes = Vec::new();
        let mut interfaces = Vec::new();
        let mut functions = Vec::new();
        let mut consts = Vec::new();

        for pair in pair.into_inner() {
            match pair.as_rule() {
                Rule::EOI => break,
                Rule::class => {
                    let class = Class::parse(pair, env, script)?;
                    classes.push(class);
                },
                Rule::interface => {
                    let interface = Interface::parse(pair, env, script)?;
                    interfaces.push(interface);
                },
                Rule::function => {
                    let function = Function::parse(pair, env, script)?;
                    functions.push(function);
                }
                Rule::constant => {
                    consts.append(&mut Vec::<Constant>::parse(pair, env, script)?);
                }
                rule => { panic!("(document) Incorrect Rule: {:?}", rule) }
            }
        }

        Ok(Self { classes, interfaces, functions, consts })
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
    fn parse(pair: Pair<Rule>, env: &Environment, script: &Arc<SslScript>) -> Result<Self, ParseError> {
        assert_rule!(pair, interface);

        let mut pairs = pair.into_inner();

        assert_pairs!(pairs, 1..);
        let name = pairs.next().unwrap().as_str().to_string();

        let mut methods = Vec::new();

        for method_pair in pairs {
            let method = InterfaceMethod::parse(method_pair, env, script)?;
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
    fn parse(pair: Pair<Rule>, env: &Environment, script: &Arc<SslScript>) -> Result<Self, ParseError> {
        assert_rule!(pair, method_header | default_method);

        let mut pairs = pair.into_inner();

        assert_pairs!(pairs, 1..);
        let name = pairs.next().unwrap().as_str().to_string();

        let mut inputs = Vec::new();

        let mut pair = pairs.next().unwrap();

        while pair.as_rule() == Rule::binding {
            let input = Binding::parse(pair, env, script)?;

            inputs.push(input);
            pair = pairs.next().unwrap();
        }

        let output = Type::parse(pair, env, script)?;

        if let Some(block_pair) = pairs.next() {
            let body = Block::parse(block_pair, env, script)?;

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
    fn parse(pair: Pair<Rule>, env: &Environment, script: &Arc<SslScript>) -> Result<Self, ParseError> {
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
                instance = Some(Instance::parse(pair, env, script)?);
                break;
            }

            assert_rule!(pair, binding | method);

            match Binding::parse(pair.clone(), env, script) {
                Ok(field) => fields.push(field),
                Err(err1) => match Method::parse(pair, env, script) {
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
    fn parse(pair: Pair<Rule>, env: &Environment, script: &Arc<SslScript>) -> Result<Self, ParseError> {
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
                let bound = Bound::parse(bound_pair, env, script)?;
                bounds.push(bound);
            }

            pair = pairs.next().unwrap();
        }

        let mut inputs = Vec::new();

        while pair.as_rule() == Rule::binding {
            let input = Binding::parse(pair, env, script)?;

            inputs.push(input);
            pair = pairs.next().unwrap();
        }

        let output = Type::parse(pair, env, script)?;

        assert_pairs!(pairs, 1..);
        let body = Block::parse(pairs.next().unwrap(), env, script)?;

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

    fn get_signature_string(&self) -> String {
        format!(
            "{}({})",
            self.name,
            self.inputs.iter().map(|Binding(_, ty)| ty.to_string()).collect::<Vec<_>>().join(", ")
        )
    }

    fn input_types(&self) -> Vec<Type> {
        self.inputs.iter().map(|Binding(_, ty)| ty.clone()).collect()
    }
}

impl Parse for Function {
    fn parse(pair: Pair<Rule>, env: &Environment, script: &Arc<SslScript>) -> Result<Self, ParseError> {
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
                let bound = Bound::parse(bound_pair, env, script)?;
                bounds.push(bound);
            }

            pair = pairs.next().unwrap();
        }

        let mut inputs = Vec::new();

        while pair.as_rule() == Rule::binding {
            let input = Binding::parse(pair, env, script)?;

            inputs.push(input);
            pair = pairs.next().unwrap();
        }

        let output = Type::parse(pair, env, script)?;

        assert_pairs!(pairs, 1..);
        let body = Block::parse(pairs.next().unwrap(), env, script)?;

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
    fn parse(pair: Pair<Rule>, env: &Environment, script: &Arc<SslScript>) -> Result<Self, ParseError> {
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
pub fn parse_block(script_str: impl AsRef<str>, env: &Environment) -> Result<Block, ParseError> {
    let script = Arc::new(SslScript::new(script_str.as_ref().to_string(), None));
    let mut parsed = ScaffoldParser::parse(Rule::block_wrapper, script.source.as_str()).unwrap();
    Block::parse(parsed.next().unwrap().into_inner().next().unwrap(), env, &script)
}

#[derive(Debug, PartialEq, Clone)]
pub struct Block(pub Vec<Stmt>, pub Option<Expr>);

const INSERTED_TUPLE_NAME: &str = "_t__tuple";

impl Block {
    fn simplify_destructuring(&mut self) {
        self.walk_tree_mut_with_options(Options::default(), &mut |node| {
            match node {
                TreeNodeMut::Block(block) => {
                    let mut i = 0;

                    while i < block.0.len() {
                        if let Stmt::Declare(LvalueDeclare::TupleDestructure(_), _) = &block.0[i] {
                            let tuple_name = INSERTED_TUPLE_NAME.to_string();

                            let Stmt::Declare(lvalue, expr) = block.0.remove(i) else { unreachable!() };
                            let mut destructure_stack: Vec<(_, Expr)> = vec![(lvalue, ExprInner::Var(tuple_name.clone(), Type::Auto).into())];
                            let mut new_stmts = vec![Stmt::Declare(
                                LvalueDeclare::Binding(Binding(tuple_name, Type::Auto)),
                                expr
                            )];

                            while let Some((lvalue, tuple_access)) = destructure_stack.pop() {
                                match lvalue {
                                    LvalueDeclare::TupleDestructure(v) => {
                                        destructure_stack.append(&mut v.into_iter().enumerate().map(|(i, lvalue)| {
                                            (lvalue, ExprInner::TupleAccess(Box::new(tuple_access.clone()).into(), i).into())
                                        }).rev().collect());
                                    }
                                    lvalue => {
                                        new_stmts.push(Stmt::Declare(lvalue, tuple_access));
                                    }
                                }
                            }

                            block.0.splice(i..i, new_stmts);
                        } else if let Stmt::Assign(Lvalue::TupleDestructure(_), _) = &block.0[i] {
                            let tuple_name = "____tuple".to_string();

                            let Stmt::Assign(lvalue, expr) = block.0.remove(i) else { unreachable!() };
                            let mut destructure_stack: Vec<(_, Expr)> = vec![(lvalue, ExprInner::Var(tuple_name.clone(), Type::Auto).into())];
                            let mut new_stmts = vec![Stmt::Declare(
                                LvalueDeclare::Binding(Binding(tuple_name, Type::Auto)),
                                expr
                            )];

                            while let Some((lvalue, tuple_access)) = destructure_stack.pop() {
                                match lvalue {
                                    Lvalue::TupleDestructure(v) => {
                                        destructure_stack.append(&mut v.into_iter().enumerate().map(|(i, lvalue)| {
                                            (lvalue, ExprInner::TupleAccess(Box::new(tuple_access.clone()).into(), i).into())
                                        }).rev().collect());
                                    }
                                    lvalue => {
                                        new_stmts.push(Stmt::Assign(lvalue, tuple_access));
                                    }
                                }
                            }
                            block.0.splice(i..i, new_stmts);
                        }
                        i += 1;
                    }
                }
                _ => {}
            }
            Ok::<(), ()>(())
        }).unwrap()
    }
}

impl Parse for Block {
    fn parse(pair: Pair<Rule>, env: &Environment, script: &Arc<SslScript>) -> Result<Self, ParseError> {
        assert_rule!(pair, block | block_expr);
        let pairs = pair.into_inner();

        let mut stmts = Vec::new();
        let mut expr = None;

        for pair in pairs {
            match pair.as_rule() {
                Rule::stmt => {
                    let stmt = Stmt::parse(pair, env, script)?;
                    stmts.push(stmt);
                },
                Rule::expr => {
                    let parsed_expr = Expr::parse(pair, env, script)?;
                    expr = Some(parsed_expr);
                },
                rule => panic!("(block) Incorrect Rule: {:?}", rule)
            }
        }

        let mut block = Self(stmts, expr);

        block.simplify_destructuring();

        Ok(block)
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

// constant = { "const" ~ w ~ decl_lvalue ~ w ~ "=" ~ expr ~ ";" }
#[derive(Debug, PartialEq, Clone)]
pub struct Constant(Binding, Expr);

impl Parse for Vec<Constant> {
    fn parse(pair: Pair<Rule>, env: &Environment, script: &Arc<SslScript>) -> Result<Self, ParseError> {
        let tuple_ident = gen_ident("____tuple");

        assert_rule!(pair, constant);
        let mut pairs = pair.into_inner();
        assert_pairs!(pairs, 2);
        let mut consts = LvalueDeclare::parse(pairs.next().unwrap(), env, script)?
            .to_tuple_accesses(&tuple_ident).into_iter().map(|(binding, expr)| {
                Constant(binding, expr)
            }).collect::<Vec<_>>();

        consts.insert(0, Constant(Binding(tuple_ident, Type::Auto), Expr::parse(pairs.next().unwrap(), env, script)?));

        Ok(consts)
    }
}

impl Display for Constant {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "const {} = {}", self.0, self.1)
    }
}

// stmt  = { (decl | asgn | ifelse | (expr ~ ";")) }
#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Declare(LvalueDeclare, Expr),
    Assign(Lvalue, Expr),
    IfElse((Expr, Block), Vec<(Expr, Block)>, Option<Block>),
    Expr(Expr),
    Noop,
}

impl Stmt {
    pub fn get_name(&self) -> String {
        match self {
            Stmt::Declare(_, _) => "Declare".to_string(),
            Stmt::Assign(_, _) => "Assign".to_string(),
            Stmt::IfElse(_, _, _) => "IfElse".to_string(),
            Stmt::Expr(_) => "Expr".to_string(),
            Stmt::Noop => "Noop".to_string(),
        }
    }
}

impl Parse for Stmt {
    fn parse(pair: Pair<Rule>, env: &Environment, script: &Arc<SslScript>) -> Result<Self, ParseError> {
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
                    LvalueDeclare::parse(pairs.next().unwrap(), env, script)?,
                    Expr::parse(pairs.next().unwrap(), env, script)?
                )
            },
            Rule::asgn => {
                let mut pairs = pair.into_inner();
                assert_pairs!(pairs, 2);

                Stmt::Assign(
                    Lvalue::parse(pairs.next().unwrap(), env, script)?,
                    Expr::parse(pairs.next().unwrap(), env, script)?
                )
            },
            Rule::ifelse => {
                let mut pairs = pair.into_inner();
                assert_pairs!(pairs, 2..);

                let if_expr = Expr::parse(pairs.next().unwrap(), env, script)?;
                let if_block = Block::parse(pairs.next().unwrap(), env, script)?;
                let mut else_ifs = Vec::new();
                let mut else_block = None;

                while pairs.len() > 0 {
                    if pairs.len() > 1 {
                        else_ifs.push((
                            Expr::parse(pairs.next().unwrap(), env, script)?,
                            Block::parse(pairs.next().unwrap(), env, script)?
                        ));
                    } else {
                        else_block = Some(Block::parse(pairs.next().unwrap(), env, script)?);
                    }
                }

                Stmt::IfElse((if_expr, if_block), else_ifs, else_block)
            },
            Rule::expr => {
                Stmt::Expr(Expr::parse(pair, env, script)?)
            },
            _ => panic!("(stmt) Incorrect Rule: {:?}", rule)
        })
    }
}

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Declare(lvalue, expr) => {
                write!(f, "let {} = {};", lvalue, expr)
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

// decl_lvalue = { (binding | ident) | ("(" ~ w ~ (decl_lvalue ~ w ~ "," ~ w)+ ~ (decl_lvalue ~ w)? ~ ")") }
#[derive(Debug, PartialEq, Clone)]
pub enum LvalueDeclare {
    Binding(Binding),
    TupleDestructure(Vec<LvalueDeclare>)
}

impl LvalueDeclare {
    pub fn get_binding(&self) -> &Binding {
        match self {
            LvalueDeclare::Binding(binding) => binding,
            LvalueDeclare::TupleDestructure(v) => panic!("All tuple destructuring should have been removed at this point: {v:?}")
        }
    }

    pub fn get_binding_mut(&mut self) -> &mut Binding {
        match self {
            LvalueDeclare::Binding(binding) => binding,
            LvalueDeclare::TupleDestructure(v) => panic!("All tuple destructuring should have been removed at this point: {v:?}")
        }
    }

    fn to_tuple_accesses(self, tuple_name: &String) -> Vec<(Binding, Expr)> {
        match self {
            Self::Binding(binding) => vec![(binding, ExprInner::Var(tuple_name.clone(), Type::Auto).into())],
            Self::TupleDestructure(v) => v.into_iter().enumerate().map(|(i, lvalue)| {
                lvalue.to_tuple_accesses(tuple_name)
                    .into_iter()
                    .map(|(var_name, expr)| (var_name, ExprInner::TupleAccess(Box::new(expr), i).into()))
                    .collect::<Vec<_>>()
            }).flatten().collect::<Vec<_>>()
        }
    }
}

impl Parse for LvalueDeclare {
    fn parse(pair: Pair<Rule>, env: &Environment, script: &Arc<SslScript>) -> Result<Self, ParseError> {
        assert_rule!(pair, decl_lvalue);
        let mut pairs = pair.into_inner();
        assert_pairs!(pairs, 1..);
        let mut lvalue_pair = pairs.next().unwrap();

        match lvalue_pair.as_rule() {
            Rule::binding => {
                Ok(LvalueDeclare::Binding(Binding::parse(lvalue_pair, env, script)?))
            }
            Rule::ident => {
                Ok(LvalueDeclare::Binding(Binding(lvalue_pair.as_str().to_string(), Type::Auto)))
            }
            Rule::decl_lvalue => {
                let mut tuple_destructure_items = Vec::new();

                loop {
                    tuple_destructure_items.push(Self::parse(lvalue_pair, env, script)?);

                    let Some(next_pair) = pairs.next() else { break; };

                    lvalue_pair = next_pair;
                }

                Ok(LvalueDeclare::TupleDestructure(tuple_destructure_items))
            }
            r => {
                Err(ParseError::BadRule(r, vec![Rule::binding, Rule::ident, Rule::decl_lvalue]))
            }
        }
    }
}

impl Display for LvalueDeclare {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Binding(Binding(var, Type::Auto)) => write!(f, "{var}"),
            Self::Binding(Binding(var, ty)) => write!(f, "{var}: {ty}"),
            Self::TupleDestructure(tuple_items) => write!(f, "({})", tuple_items.iter().map(|lvalue| {
                format!("{lvalue}")
            }).collect::<Vec<_>>().join(", "))
        }
    }
}

// lvalue = { (ident ~ ("." ~ (ident | tuple_id))*) | ("(" ~ w ~ (lvalue ~ w ~ "," ~ w)+ ~ (lvalue ~ w)? ~ ")") }
#[derive(Debug, PartialEq, Clone)]
pub enum Lvalue {
    Var(String),
    Fields(String, Vec<LvalueField>),
    TupleDestructure(Vec<Lvalue>)
}

impl Lvalue {
    pub fn get_var_name(&self) -> &String {
        match self {
            Lvalue::Var(var) => var,
            Lvalue::Fields(var, _) => var,
            Lvalue::TupleDestructure(v) => panic!("All tuple destructuring should have been removed at this point: {v:?}")
        }
    }

    pub fn get_var_name_mut(&mut self) -> &mut String {
        match self {
            Lvalue::Var(var) => var,
            Lvalue::Fields(var, _) => var,
            Lvalue::TupleDestructure(v) => panic!("All tuple destructuring should have been removed at this point: {v:?}")
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum LvalueField {
    Field(String),
    TupleAccess(usize)
}

impl Display for LvalueField {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Field(s) => write!(f, "{s}"),
            Self::TupleAccess(n) => write!(f, "{n}")
        }
    }
}

impl Parse for Lvalue {
    fn parse(pair: Pair<Rule>, env: &Environment, script: &Arc<SslScript>) -> Result<Self, ParseError> {
        assert_rule!(pair, lvalue);
        let mut pairs = pair.into_inner();
        assert_pairs!(pairs, 1..);
        let mut lvalue_pair = pairs.next().unwrap();

        match lvalue_pair.as_rule() {
            Rule::ident => {
                if pairs.len() == 0 {
                    Ok(Lvalue::Var(lvalue_pair.as_str().to_string()))
                } else {
                    let mut next_pair = pairs.next().unwrap();
                    let mut fields = Vec::new();

                    loop {
                        fields.push(match next_pair.as_rule() {
                            Rule::ident => LvalueField::Field(next_pair.as_str().to_string()),
                            Rule::tuple_id => LvalueField::TupleAccess(next_pair.as_str().parse().unwrap()),
                            r => { return Err(ParseError::BadRule(r, vec![Rule::ident, Rule::tuple_id])) }
                        });

                        let Some(next_next_pair) = pairs.next() else { break; };

                        next_pair = next_next_pair;
                    }

                    Ok(Lvalue::Fields(lvalue_pair.as_str().to_string(), fields))
                }
            }
            Rule::lvalue => {
                let mut tuple_destructure_items = Vec::new();

                loop {
                    tuple_destructure_items.push(Self::parse(lvalue_pair, env, script)?);

                    let Some(next_pair) = pairs.next() else { break; };

                    lvalue_pair = next_pair;
                }

                Ok(Lvalue::TupleDestructure(tuple_destructure_items))
            }
            r => {
                Err(ParseError::BadRule(r, vec![Rule::ident, Rule::lvalue]))
            }
        }
    }
}

impl Display for Lvalue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Var(var) => write!(f, "{var}"),
            Self::Fields(var, fields) => write!(f, "{var}{}", fields.iter().map(|field| {
                format!(".{field}")
            }).collect::<String>()),
            Self::TupleDestructure(tuple_items) => write!(f, "({})", tuple_items.iter().map(|lvalue| {
                format!("{lvalue}")
            }).collect::<Vec<_>>().join(", "))
        }
    }
}

pub fn parse_expr(script_str: impl AsRef<str>, env: &Environment) -> Result<Expr, ParseError> {
    let script = Arc::new(SslScript::new(script_str.as_ref().to_string(), None));
    let mut parsed = ScaffoldParser::parse(Rule::expr, script.source.as_str()).unwrap();
    Expr::parse(parsed.next().unwrap(), env, &script)
}

define_span_wrapper!(Expr, ExprInner);

#[derive(Debug, PartialEq, Clone)]
pub enum ExprInner {
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

impl ExprInner {
    pub fn get_name(&self) -> String {
        match self {
            Self::BinExpr(_, _, _) => "BinExpr".to_string(),
            Self::UnaryExpr(_, _) => "UnaryExpr".to_string(),
            Self::Application(_, _) => "Application".to_string(),
            Self::Dot(_, _, _) => "Dot".to_string(),
            Self::Field(_, _) => "Field".to_string(),
            Self::TupleAccess(_, _) => "TupleAccess".to_string(),
            Self::Tuple(_) => "Tuple".to_string(),
            Self::Var(_, _) => "Var".to_string(),
            Self::Lit(_) => "Lit".to_string(),
            Self::Block(_) => "Block".to_string()
        }
    }
}

impl Parse for Expr {
    fn parse(pair: Pair<Rule>, env: &Environment, script: &Arc<SslScript>) -> Result<Self, ParseError> {
        assert_rule!(pair, expr | app | dot | tuple | var | lit | block_expr_wrapper);
        let rule = pair.as_rule();
        let span = pair.as_span();
        let mut expr_pairs = pair.into_inner();

        Ok(Expr(match rule {
            Rule::expr => {
                assert_pairs!(expr_pairs, 1);
                let p2_pair = expr_pairs.next().unwrap();

                Op::parse(p2_pair, env, script)?.to_expr().0
            },
            Rule::app => {
                let name = expr_pairs.next().unwrap().as_str().to_string();
                let exprs_pair = expr_pairs.next().unwrap();
                let pairs = exprs_pair.into_inner();
                let mut args = Vec::new();

                for pair in pairs {
                    args.push(Expr::parse(pair, env, script)?);
                }

                ExprInner::Application(name, args)
            },
            Rule::tuple => {
                let mut elements = Vec::new();

                for pair in expr_pairs {
                    elements.push(Expr::parse(pair, env, script)?);
                }

                ExprInner::Tuple(elements)
            },
            Rule::dot => {
                let name = expr_pairs.next().unwrap().as_str().to_string();
                let method = expr_pairs.next().unwrap().as_str().to_string();
                let exprs_pair = expr_pairs.next().unwrap();
                let pairs = exprs_pair.into_inner();
                let mut args = Vec::new();

                for pair in pairs {
                    args.push(Expr::parse(pair, env, script)?);
                }

                ExprInner::Dot(name, method, args)
            },
            Rule::var => {
                ExprInner::Var(expr_pairs.next().unwrap().as_str().to_string(), Type::Auto)
            },
            Rule::lit => {
                ExprInner::Lit(Lit::parse(expr_pairs.next().unwrap(), env, script)?)
            },
            Rule::block_expr_wrapper => {
                ExprInner::Block(Box::new(Block::parse(expr_pairs.next().unwrap(), env, script)?))
            },
            _ => panic!("(expr) Incorrect Rule: {:?}", rule)
        }, SslSpan::from_span(span, script.clone())))
    }
}

impl Display for ExprInner {
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
    Unary(String, Box<Op>, SslSpan),
    Binop(usize, Expr, String, Box<Op>, SslSpan)
}

impl Parse for Op {
    fn parse(pair: Pair<Rule>, env: &Environment, script: &Arc<SslScript>) -> Result<Self, ParseError> {
        assert_rule!(pair, prec0 | unary | prec1 | prec2 | prec3 | prec4);
        let precedence = match pair.as_rule() {
            Rule::prec4 => 4,
            Rule::prec3 => 3,
            Rule::prec2 => 2,
            Rule::prec1 => 1,
            Rule::prec0 => 0,
            _ => 100
        };

        let full_span = SslSpan::from_span(pair.as_span(), script.clone());
        let mut pairs = pair.into_inner();
        assert_pairs!(pairs, 1..=3);

        Ok(match (pairs.len(), pairs.next().unwrap()) {
            (n, pair) if pair.as_rule() == Rule::prec0 && n > 1 => {
                let mut op = Op::parse(pair, env, script)?;

                while let Some(pair) = pairs.next() {
                    let span = SslSpan::from_span(pair.as_span(), script.clone());
                    op = match pair.as_rule() {
                        Rule::ident => Op::Expr(Expr(ExprInner::Field(Box::new(op.to_expr()), pair.as_str().to_string()), span)),
                        Rule::tuple_id => Op::Expr(Expr(ExprInner::TupleAccess(Box::new(op.to_expr()), pair.as_str().parse().unwrap()), span)),
                        r => return Err(ParseError::BadRule(r, vec![Rule::ident, Rule::tuple_id]))
                    };
                }

                op
            },
            (1, pair) => {
                match pair.as_rule() {
                    Rule::prec0 | Rule::unary | Rule::prec1 | Rule::prec2 | Rule::prec3 | Rule::prec4 => {
                        Op::Solo(Box::new(Op::parse(pair, env, script)?))
                    },
                    _ => {
                        Op::Expr(Expr::parse(pair, env, script)?)
                    }
                }
            },
            (2, first_pair) => {
                let op = first_pair.as_str().to_string();
                let right = Op::parse(pairs.next().unwrap(), env, script)?;

                Op::Unary(op, Box::new(right), full_span)
            },
            (3, left_pair) => {
                let left = Op::parse(left_pair, env, script)?;
                let op = pairs.next().unwrap().as_str().to_string();
                let right = Op::parse(pairs.next().unwrap(), env, script)?;

                Op::Binop(precedence, left.to_expr(), op, Box::new(right), full_span)
            },
            left_over => {panic!("{left_over:?}")}
        })
    }
}

impl Op {
    fn to_expr(self) -> Expr {
        match self {
            Self::Expr(expr) => expr,
            Self::Solo(expr) => expr.to_expr(),
            Self::Unary(op, expr, span) =>
                Expr(ExprInner::UnaryExpr(op, Box::new(expr.to_expr())), span),
            Self::Binop(p1, expr1, op1, expr2, span1) => {
                match *expr2 {
                    Self::Binop(
                        p2, expr3, op2, expr4, span2
                    ) if p1 == p2 => {
                        let left = Expr(ExprInner::BinExpr(Box::new(expr1), op1, Box::new(expr3)), span2);

                        Self::Binop(p2, left, op2, expr4, span1).to_expr()
                    },
                    _ => Expr(ExprInner::BinExpr(Box::new(expr1), op1, Box::new(expr2.to_expr())), span1)
                }
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Binding(pub String, pub Type);

impl Parse for Binding {
    fn parse(pair: Pair<Rule>, env: &Environment, script: &Arc<SslScript>) -> Result<Self, ParseError> {
        match pair.as_rule() {
            Rule::binding => {
                let mut pairs = pair.into_inner();

                assert_pairs!(pairs, 1..=2);

                let name = pairs.next().unwrap().as_str().to_string();

                let ty = Type::parse(pairs.next().unwrap(), env, script)?;

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
    fn parse(pair: Pair<Rule>, env: &Environment, script: &Arc<SslScript>) -> Result<Self, ParseError> {
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
                        .map(|pair| Type::parse(pair, env, script))
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
    fn parse(pair: Pair<Rule>, env: &Environment, script: &Arc<SslScript>) -> Result<Self, ParseError> {
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
    fn parse(pair: Pair<Rule>, env: &Environment, script: &Arc<SslScript>) -> Result<Self, ParseError> {
        assert_rule!(pair, instance);
        let mut pairs = pair.into_inner();

        assert_pairs!(pairs, 1..);
        let name = pairs.next().unwrap().as_str().to_string();

        let mut key_vals = Vec::new();

        for pair in pairs {
            key_vals.push(KeyVal::parse(pair, env, script)?);
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
    fn parse(pair: Pair<Rule>, env: &Environment, script: &Arc<SslScript>) -> Result<Self, ParseError> {
        assert_rule!(pair, key_val);
        let span = SslSpan::from_span(pair.as_span(), script.clone());
        let mut pairs = pair.into_inner();

        assert_pairs!(pairs, 1 | 2);

        let key = pairs.next().unwrap().as_str().to_string();

        let value = if pairs.len() == 0 {
            Value::Expr(Expr(ExprInner::Var(key.clone(), Type::Auto), span))
        } else {
            Value::parse(pairs.next().unwrap(), env, script)?
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
    fn parse(pair: Pair<Rule>, env: &Environment, script: &Arc<SslScript>) -> Result<Self, ParseError> {
        match pair.as_rule() {
            Rule::expr => {
                Expr::parse(pair, env, script).map(Value::Expr)
            },
            Rule::instance => {
                Instance::parse(pair, env, script).map(Value::Instance)
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
    use std::sync::Arc;
    use crate::enviroment::Environment;
    use crate::interpreter::Eval;
    use crate::parser::{parse_block, Document, Parse, Rule, ScaffoldParser};
    use crate::test_helpers;
    use pest::Parser;
    use crate::parser_span::SslScript;
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
        let document = Document::parse(parsed.next().unwrap(), &env, &Arc::new(SslScript::new(script.to_string(), None))).unwrap();
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
            let (a: f32, b) = (1.0, 2.0);
            let v = vec4(1, 3, 4, 5);
            (a, (b, a), v.x) = (4.0, (1 + 3, -.1 + 8), 8.0);
            let c: (f32, vec4, f32, f32) = (1.0, vec4(1, 3, 2, 1) / 8, 3.0, 4.0);
            (v, a, b)
        }"#;

        let block = parse_block(script, &Environment::new()).unwrap();
        let string = test_helpers::prettify_string(format!("{block}"));
        let env = Environment::new();

        println!("{}\nwhich returns: {:?}", string, block.eval(&mut Scope::new(), &env));
    }
}