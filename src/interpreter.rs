use std::collections::HashMap;
use std::fmt::{Debug, Display};
use glam::{Mat4, Vec4};
use crate::assemble::AssembledStructure;
use crate::parser::{Binding, Block, Expr, ExprInner, Lit, Lvalue, LvalueDeclare, LvalueField, Stmt, Type};
use anyhow::{anyhow, Context, Result as AnyResult};
use once_cell::sync::Lazy;
use crate::enviroment::{Environment, SslType};
use crate::scope::Scope;

pub trait IntoArgs {
    fn into_args(self) -> Vec<Lit>;
}

impl IntoArgs for Vec<Lit> {
    fn into_args(self) -> Vec<Lit> {
        self
    }
}

macro_rules! impl_tuple_stuff {
    ($($ty:ident),*) => {
        #[allow(non_camel_case_types)]
        impl<$($ty : Into<Lit>),*> IntoArgs for ($($ty,)*) {
            fn into_args(self) -> Vec<Lit> {
                let ( $($ty,)* ) = self;

                vec![ $( $ty .into() ),* ]
            }
        }

        #[allow(non_camel_case_types)]
        impl<$($ty: TryFrom<Lit, Error=anyhow::Error>),*> TryFrom<Lit> for ($($ty,)*) {
            type Error = anyhow::Error;

            fn try_from(value: Lit) -> Result<Self, Self::Error> {
                match value {
                    Lit::Tuple(fields) => {
                        let mut iter = fields.into_iter();

                        $(
                            let $ty = iter.next().ok_or_else(|| anyhow!("Not enough fields in tuple"))?.try_into()?;
                        )*

                        if iter.next().is_some() {
                            return Err(anyhow!("Too many fields in tuple"));
                        }

                        Ok(($($ty,)*))
                    }
                    _ => Err(anyhow!("Field not supported"))
                }
            }
        }
    };
}

impl<T: Into<Lit>> IntoArgs for T {
    fn into_args(self) -> Vec<Lit> {
        vec![self.into()]
    }
}

impl_tuple_stuff!(a);
impl_tuple_stuff!(a, b);
impl_tuple_stuff!(a, b, c);
impl_tuple_stuff!(a, b, c, d);
impl_tuple_stuff!(a, b, c, d, e);
impl_tuple_stuff!(a, b, c, d, e, f);
impl_tuple_stuff!(a, b, c, d, e, f, g);
impl_tuple_stuff!(a, b, c, d, e, f, g, h);
impl_tuple_stuff!(a, b, c, d, e, f, g, h, i);
impl_tuple_stuff!(a, b, c, d, e, f, g, h, i, j);
impl_tuple_stuff!(a, b, c, d, e, f, g, h, i, j, k);
impl_tuple_stuff!(a, b, c, d, e, f, g, h, i, j, k, l);
impl_tuple_stuff!(a, b, c, d, e, f, g, h, i, j, k, l, m);
impl_tuple_stuff!(a, b, c, d, e, f, g, h, i, j, k, l, m, n);
impl_tuple_stuff!(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o);
impl_tuple_stuff!(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p);
impl_tuple_stuff!(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q);

impl TryFrom<Lit> for f32 {
    type Error = anyhow::Error;

    fn try_from(field: Lit) -> AnyResult<Self> {
        match field {
            Lit::F32(f) => Ok(f),
            _ => Err(anyhow!("Field not supported"))
        }
    }
}

impl TryFrom<Lit> for bool {
    type Error = anyhow::Error;

    fn try_from(field: Lit) -> AnyResult<Self> {
        match field {
            Lit::Bool(b) => Ok(b),
            _ => Err(anyhow!("Field not supported"))
        }
    }
}

impl TryFrom<Lit> for Vec4 {
    type Error = anyhow::Error;

    fn try_from(field: Lit) -> AnyResult<Self> {
        match field {
            Lit::Vec4(v) => Ok(v),
            _ => Err(anyhow!("Field not supported"))
        }
    }
}

impl TryFrom<Lit> for Mat4 {
    type Error = anyhow::Error;

    fn try_from(field: Lit) -> AnyResult<Self> {
        match field {
            Lit::Mat4x4(m) => Ok(m),
            _ => Err(anyhow!("Field not supported"))
        }
    }
}

impl TryFrom<Lit> for () {
    type Error = anyhow::Error;

    fn try_from(field: Lit) -> AnyResult<Self> {
        match field {
            Lit::Unit => Ok(()),
            _ => Err(anyhow!("Field not supported"))
        }
    }

}

impl From<f32> for Lit {
    fn from(f: f32) -> Lit {
        Lit::F32(f)
    }
}

impl From<bool> for Lit {
    fn from(b: bool) -> Lit {
        Lit::Bool(b)
    }
}

impl From<Vec4> for Lit {
    fn from(v: Vec4) -> Lit {
        Lit::Vec4(v)
    }
}

impl From<Mat4> for Lit {
    fn from(m: Mat4) -> Lit {
        Lit::Mat4x4(m)
    }
}

impl From<()> for Lit {
    fn from(_: ()) -> Lit {
        Lit::Unit
    }
}

impl AssembledStructure {
    pub fn eval_method<OUT: TryFrom<Lit, Error=anyhow::Error>>(&self, method_name: impl AsRef<str>, args: impl IntoArgs) -> AnyResult<OUT> {
        /* TODO: It's bad to search for the method every single time, I need to find
                a way to let the user have it/get it fast.  Perhaps give an index? */
        let method = self.get_method(&method_name)
            .with_context(|| format!("Could not find method {}", method_name.as_ref()))?;

        let mut scope = self.evaluated_scope.clone();

        for (Binding(name, _), field) in method.inputs.iter().zip(args.into_args().into_iter()) {
            scope.push(name.clone(), field);
        }

        Ok(method.body.eval(&mut scope, &self.env)?.try_into()?)
    }
}

pub trait Eval {
    fn eval_into<OUT: TryFrom<Lit, Error=anyhow::Error>>(&self, env: &Environment) -> AnyResult<OUT> {
        self.eval(&mut Scope::new(), env)?.try_into()
    }

    fn eval(&self, scope: &mut Scope<Lit>, env: &Environment) -> AnyResult<Lit>;

    fn eval_type(&self, env: &Environment) -> AnyResult<Type>;
}

impl Eval for Block {
    fn eval(&self, scope: &mut Scope<Lit>, env: &Environment) -> AnyResult<Lit> {
        let scope_size = scope.size();

        for stmt in self.0.iter() {
            stmt.eval(scope, env)?;
        }

        let return_value = if let Some(expr) = &self.1 {
            Ok(expr.eval(scope, env)?)
        } else {
            Ok(Lit::Unit)
        };

        scope.resize(scope_size);

        return_value
    }

    fn eval_type(&self, env: &Environment) -> AnyResult<Type> {
        if let Some(expr) = &self.1 {
            Ok(expr.eval_type(env)?)
        } else {
            Ok(Type::Unit)
        }
    }
}

impl Eval for Stmt {
    fn eval(&self, scope: &mut Scope<Lit>, env: &Environment) -> AnyResult<Lit> {
        match self {
            Stmt::Declare(lvalue, expr) => {
                let lit = expr.eval(scope, env)?;
                let LvalueDeclare::Binding(Binding(var, ty)) = lvalue else {
                    return Err(anyhow!("Expected binding, but found tuple destruct, which should have been removed by this point"));
                };

                scope.push(var.clone(), lit)
            }
            Stmt::Assign(lvalue, expr) => {
                let lit = expr.eval(scope, env)?;
                match lvalue {
                    Lvalue::Fields(var, fields) => {
                        let val = scope.get_mut(var).with_context(|| format!("var {var} not found in scope"))?;

                        *val = LvalueField::get_fields(val.clone(), lit, &fields[..], env)?;
                    }
                    Lvalue::Var(var) => {
                        let val = scope.get_mut(var).with_context(|| format!("var {var} not found in scope"))?;

                        *val = lit;
                    }
                    Lvalue::TupleDestructure(_) => return Err(
                        anyhow!("Expected binding, but found tuple destruct, which should have been removed by this point")
                    )
                }
            }
            Stmt::IfElse((if_expr, if_block), else_ifs, else_block) => {
                let iter = [(if_expr, if_block)].into_iter().chain(else_ifs.iter().map(|(a, b)| (a, b)));

                for (expr, block) in iter {
                    match expr.eval(scope, env)? {
                        Lit::Bool(true) => {
                            block.eval(scope, env)?;

                            return Ok(Lit::Unit);
                        }
                        Lit::Bool(_) => {}
                        lit => return Err(anyhow!("if statement expr must evaluate to boolean, instead evaluated to {lit}"))
                    }
                }

                if let Some(block) = else_block {
                    block.eval(scope, env)?;
                }
            }
            Stmt::Expr(expr) => {
                expr.eval(scope, env)?;
            }
            Stmt::Noop => {}
        }

        Ok(Lit::Unit)
    }

    fn eval_type(&self, _env: &Environment) -> AnyResult<Type> {
        Ok(Type::Unit)
    }
}

impl LvalueField {
    fn get_fields(val: Lit, lit: Lit, fields: &[Self], env: &Environment) -> AnyResult<Lit> {
        if fields.len() == 0 {
            return Ok(lit);
        }

        match &fields[0] {
            LvalueField::Field(field) => {
                let f = &*env.get_field(&field, val.get_type()).ok_or(
                    anyhow!("Could not find field {} on type {}", field, val.get_type())
                )?.2;

                let new_lit = LvalueField::get_fields(f.get(val.clone(), env), lit, &fields[1..], env)?;

                Ok(f.set(val, new_lit, env))
            }
            LvalueField::TupleAccess(i) => {
                let Lit::Tuple(mut v) = val else { return Err(anyhow!("Cannot call tuple access on {}", val)); };
                if *i >= v.len() { return Err(anyhow!("Tuple accessor out of bounds: {i} >= {}", v.len())) };

                v[*i] = LvalueField::get_fields(v[*i].clone(), lit, &fields[1..], env)?;

                Ok(Lit::Tuple(v))
            }
        }
    }
}

impl Eval for Expr {
    fn eval(&self, scope: &mut Scope<Lit>, env: &Environment) -> AnyResult<Lit> {
        self.0.eval(scope, env).context(self.span())
    }

    fn eval_type(&self, env: &Environment) -> AnyResult<Type> {
        self.0.eval_type(env).context(self.span())
    }
}

impl Eval for ExprInner {
    fn eval(&self, scope: &mut Scope<Lit>, env: &Environment) -> AnyResult<Lit> {
        match self {
            ExprInner::BinExpr(left, symbol, right) => {
                let (left, sym, right) = (left.eval(scope, env)?, symbol.as_str(), right.eval(scope, env)?);

                if sym == "==" {
                    return Ok(Lit::Bool(left == right));
                } else if sym == "!=" {
                    return Ok(Lit::Bool(left != right));
                }

                let (_, op) = &*env.get_binary_op(sym, left.get_type(), right.get_type())
                    .ok_or(anyhow!(
                        "Could not find binary operation with signature {} {} {}",
                        left.get_type(), symbol, right.get_type()
                    ))?;

                Ok(op.call((left, right), env))
            }
            ExprInner::UnaryExpr(symbol, right) => {
                let (sym, right) = (symbol.as_str(), right.eval(scope, env)?);

                let (_, op) = &*env.get_unary_op(sym, right.get_type())
                    .ok_or(anyhow!(
                        "Could not find unary operation with signature {} {}",
                        symbol, right.get_type()
                    ))?;

                Ok(op.call(right, env))
            }
            ExprInner::Application(fn_name, args) => {
                let (input_types, inputs) = args.iter()
                    .map(|arg| arg.eval(scope, env).map(|input| (input.get_type(), input)))
                    .collect::<AnyResult<(Vec<_>, Vec<_>)>>()?;

                if let ("select", [x_false, x_true, Lit::Bool(condition)]) = (fn_name.as_str(), &inputs[..]) {
                    return if !x_false.matches_type(x_true) {
                        Err(anyhow!("Types do not match in select for {} and {}", x_false.get_type(), x_true.get_type()))
                    } else if *condition {
                        Ok(x_true.clone())
                    } else {
                        Ok(x_false.clone())
                    };
                }

                let function = &*env.get_fn(fn_name, input_types.clone())
                    .ok_or(anyhow!(
                        "Could not find signature {fn_name}({})",
                        inputs.iter().map(|input| format!("{}", input.get_type())).collect::<Vec<_>>().join(", ")
                    ))?;

                Ok(function.call(&inputs, env))
            },
            ExprInner::Dot(_, _, _) => Err(anyhow!("EVAL NOT SUPPORTED FOR DOT")),
            ExprInner::Field(expr, field_name) => {
                let value = expr.eval(scope, env)?;
                let ty = value.get_type();

                let (_, _, field) = &*env.get_field(field_name, ty.clone())
                    .ok_or(anyhow!("Field {field_name} not found in {ty}"))?;

                Ok(field.get(value, env))
            }
            ExprInner::TupleAccess(expr, index) => match expr.eval(scope, env)? {
                Lit::Tuple(fields) => {
                    let index = *index as usize;
                    if index < fields.len() {
                        Ok(fields[index].clone())
                    } else {
                        Err(anyhow!("Index out of bounds for tuple, tried to access index {index} in a {}-tuple", fields.len()))
                    }
                },
                lit => Err(anyhow!("Tuple access not supported for {}", lit.get_type()))
            }
            ExprInner::Tuple(elements) => {
                Ok(Lit::Tuple(elements.iter().map(|expr| expr.eval(scope, env)).collect::<AnyResult<Vec<_>>>()?))
            },
            ExprInner::Var(var, _) => {
                if let Some(value) = scope.get(var) {
                    return Ok(value.clone());
                }

                let (_, value) = &*env.get_const(var)
                    .ok_or(anyhow!("var {var} not found in scope"))?;

                Ok(value.clone())
            },
            ExprInner::Lit(lit) => Ok(lit.clone()),
            ExprInner::Block(block) => block.eval(scope, env)
        }
    }

    fn eval_type(&self, env: &Environment) -> AnyResult<Type> {
        match self {
            ExprInner::BinExpr(left, symbol, right) => {
                let (left, sym, right) = (left.eval_type(env)?, symbol.as_str(), right.eval_type(env)?);

                if sym == "==" || sym == "!=" {
                    return Ok(Type::Bool);
                }

                let (_, op) = &*env.get_binary_op(sym, left.clone(), right.clone())
                    .ok_or(anyhow!(
                        "Could not find binary operation with signature {} {} {}",
                        left, symbol, right
                    ))?;

                Ok(op.output(env))
            }
            ExprInner::UnaryExpr(symbol, right) => {
                let (sym, right) = (symbol.as_str(), right.eval_type(env)?);

                let (_, op) = &*env.get_unary_op(sym, right.clone())
                    .ok_or(anyhow!(
                        "Could not find unary operation with signature {} {}",
                        symbol, right
                    ))?;

                Ok(op.output(env))
            }
            ExprInner::Application(fn_name, args) => {
                let (input_types) = args.iter()
                    .map(|arg| arg.eval_type(env))
                    .collect::<AnyResult<Vec<_>>>()?;

                if let ("select", [x_false, x_true, Type::Bool]) = (fn_name.as_str(), &input_types[..]) {
                    return if x_false != x_true {
                        Err(anyhow!("Types do not match in select for {} and {}", x_false, x_true))
                    } else {
                        Ok(x_false.clone())
                    };
                }

                let function = &*env.get_fn(fn_name, input_types.clone())
                    .ok_or(anyhow!(
                        "Could not find signature {fn_name}({})",
                        input_types.iter().map(|ty| ty.to_string()).collect::<Vec<_>>().join(", ")
                    ))?;

                Ok(function.output(env))
            },
            ExprInner::Dot(_, _, _) => Err(anyhow!("EVAL NOT SUPPORTED FOR DOT")),
            ExprInner::Field(expr, field_name) => {
                let ty = expr.eval_type(env)?;

                let (_, _, get_field) = &*env.get_field(field_name, ty.clone())
                    .ok_or(anyhow!("Field {field_name} not found in {ty}"))?;

                Ok(get_field.output(env))
            }
            ExprInner::TupleAccess(expr, index) => match expr.eval_type(env)? {
                Type::Tuple(fields) => {
                    let index = *index as usize;
                    if index < fields.len() {
                        Ok(fields[index].clone())
                    } else {
                        Err(anyhow!("Index out of bounds for tuple, tried to access index {index} in a {}-tuple", fields.len()))
                    }
                },
                ty => Err(anyhow!("Tuple access not supported for {}", ty))
            }
            ExprInner::Tuple(elements) => {
                Ok(Type::Tuple(elements.iter().map(|expr| expr.eval_type(env)).collect::<AnyResult<Vec<_>>>()?))
            },
            ExprInner::Var(name, ty) => {
                if *ty == Type::Auto {
                    Err(anyhow!("Type::Auto found on {name}.  eval_type must be run after assign_types to get rid of all instances of Type::Auto"))
                } else {
                    Ok(ty.clone())
                }

            },
            ExprInner::Lit(lit) => Ok(lit.get_type()),
            ExprInner::Block(block) => block.eval_type(env),
        }
    }
}

macro_rules! define_eval {
    (($pat:pat => ($eval:expr, $ty:expr);)*) => {

    };
}

#[cfg(test)]
mod tests {
    use glam::Vec4;
    use crate::assemble::AssembledStructure;
    use crate::enviroment::Environment;
    use crate::interpreter::Eval;
    use crate::parser::{parse_block, parse_document};
    use crate::test_helpers::{get_test_stuff, prettify_string};

    #[test]
    fn try_eval_fns() {
        let mut env = Environment::new();
        let document = parse_document(r#"
            const H = 4 + A;
            const (A, q) = (1, 2 + H);

            fn abc(a: f32) -> (f32, vec4) {
                let q = a * 29.0;

                q = H + q;

                (q + a, (q % H) * vec4(1, 2, 1 / q, 2))
            }

            fn fib(n: f32) -> f32 {
                let x = 1;

                if (n > 1) {
                    x = n * fib(n - 1) + abc(1.0);
                }

                x
            }
        "#, None, &env).unwrap();

        document.add_to_environment(&mut env).unwrap();

        println!("Doc:\n{document}");

        let wgsl_output = env.get_wgsl_code().unwrap();

        println!("wgsl:\n{}\n// -------\n{}", wgsl_output.definitions.to_wgsl_definition_code(&env).unwrap(), wgsl_output.wgsl_code);

        let block = parse_block(r#"{
            let (a, v) = abc(4.0);
            let q = abc(5.0);

            (fib(5.0), a, q, v)
        }"#, &env).unwrap();

        println!("Eval: {:?}", block.eval_into::<(f32, f32, (f32, Vec4), Vec4)>(&env).unwrap());
    }

    #[test]
    fn try_eval_block() {
        let env = Environment::new();
        let block = parse_block(r#"{
            let x: f32 = 4;
            select(x + 2, 2, x < x + 1)
        }"#, &env).unwrap();

        println!("Eval: {}", block.eval_into::<f32>(&env).unwrap())
    }

    #[test]
    fn try_eval_block_tuple() {
        let env = Environment::new();
        let block = parse_block(r#"{
            let v: vec4 = 2 * vec4(1, 2, 3, 4);
            let x: f32 = v.z;
            select((x + 2, 5 * ZEROS), (-Infinity, x * ONES), x < x + 1)
        }"#, &env).unwrap();

        let (min, max): (f32, Vec4) = block.eval_into(&env).unwrap();

        println!("Eval: ({}, {})", min, max);
    }

    #[test]
    fn try_eval() {
        let (env, document, structure) = get_test_stuff(0, 1);
        println!("Document: {document}\nStructure: {structure}");

        let assembled_structure = AssembledStructure::new(&document, structure, &env).unwrap();

        println!("Assembled Structure: {}", prettify_string(format!("{assembled_structure}")));

        println!("result: {}", assembled_structure.eval_method::<Vec4>("proj", 5.0 * Vec4::X + Vec4::Y).unwrap())
    }
}
