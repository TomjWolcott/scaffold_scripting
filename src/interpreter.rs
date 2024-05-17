use std::fmt::Display;
use glam::{Mat4, Vec4};
use crate::assemble::AssembledStructure;
use crate::parser::{Binding, Block, Expr, Lit, Stmt};
use anyhow::{anyhow, Context, Result as AnyResult};

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

#[derive(Debug, Clone)]
pub struct Scope(Vec<(String, Lit)>);

impl Scope {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn get(&self, name: impl AsRef<str>) -> Option<&Lit> {
        self.0.iter().find(|(n, _)| n.as_str() == name.as_ref()).map(|(_, field)| field)
    }

    pub fn get_mut(&mut self, name: impl AsRef<str>) -> Option<&mut Lit> {
        self.0.iter_mut().find(|(n, _)| n.as_str() == name.as_ref()).map(|(_, field)| field)
    }

    pub fn push(&mut self, name: String, field: Lit) {
        self.0.push((name, field));
    }

    pub fn size(&self) -> usize {
       self.0.len()
    }

    pub fn resize(&mut self, size: usize) {
        self.0.splice(size.., []);
    }
}

impl Display for Scope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{ ")?;

        for (name, field) in self.0.iter() {
            write!(f, "{}: {}, ", name, field)?;
        }

        write!(f, " }}")
    }
}

impl Scope {
    pub fn iter(&self) -> ScopeIterator {
        ScopeIterator {
            scope: self,
            index: 0
        }
    }
}

pub struct ScopeIterator<'a> {
    scope: &'a Scope,
    index: usize
}

impl<'a> Iterator for ScopeIterator<'a> {
    type Item = (&'a String, &'a Lit);

    fn next(&mut self) -> Option<Self::Item> {
        let result = self.scope.0.get(self.index).map(|(name, field)| (name, field));
        self.index += 1;
        result
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

        Ok(method.body.eval(&mut scope)?.try_into()?)
    }
}

pub trait Eval {
    fn eval_into<OUT: TryFrom<Lit, Error=anyhow::Error>>(&self) -> AnyResult<OUT> {
        self.eval(&mut Scope::new())?.try_into()
    }

    fn eval(&self, scope: &mut Scope) -> AnyResult<Lit>;
}

impl Eval for Block {
    fn eval(&self, scope: &mut Scope) -> AnyResult<Lit> {
        let scope_size = scope.size();

        for stmt in self.0.iter() {
            stmt.eval(scope)?;
        }

        let return_value = if let Some(expr) = &self.1 {
            Ok(expr.eval(scope)?)
        } else {
            Ok(Lit::Unit)
        };

        scope.resize(scope_size);

        return_value
    }
}

impl Eval for Stmt {
    fn eval(&self, scope: &mut Scope) -> AnyResult<Lit> {
        match self {
            Stmt::Declare(Binding(var, _), expr) => {
                let eval = expr.eval(scope)?;
                scope.push(var.clone(), eval);
            }
            Stmt::Assign(var, expr) => {
                let eval_field = expr.eval(scope)?;
                let field = scope.get_mut(var).with_context(|| format!("var {var} not found in scope"))?;

                if field.matches_type(&eval_field) {
                    *field = eval_field;
                } else {
                    return Err(anyhow!(
                        "Types do not match: {} and {} when assigning var {var}",
                        field.get_type(), eval_field.get_type()
                    ));
                }
            }
            Stmt::Expr(expr) => {
                expr.eval(scope)?;
            }
            Stmt::Noop => {}
        }

        Ok(Lit::Unit)
    }
}

impl Eval for Expr {
    fn eval(&self, scope: &mut Scope) -> AnyResult<Lit> {
        match self {
            Expr::BinExpr(left, symbol, right) =>
                match (left.eval(scope)?, symbol.as_str(), right.eval(scope)?) {
                    (Lit::F32(n1), "+", Lit::F32(n2)) => Ok(Lit::F32(n1 + n2)),
                    (Lit::F32(n1), "-", Lit::F32(n2)) => Ok(Lit::F32(n1 - n2)),
                    (Lit::F32(n1), "*", Lit::F32(n2)) => Ok(Lit::F32(n1 * n2)),
                    (Lit::F32(n1), "/", Lit::F32(n2)) => Ok(Lit::F32(n1 / n2)),

                    (Lit::Vec4(v1), "+", Lit::Vec4(v2)) => Ok(Lit::Vec4(v1 + v2)),
                    (Lit::Vec4(v1), "-", Lit::Vec4(v2)) => Ok(Lit::Vec4(v1 - v2)),

                    (Lit::F32(n), "*", Lit::Vec4(v)) => Ok(Lit::Vec4(n * v)),
                    (Lit::Vec4(v), "*", Lit::F32(n)) => Ok(Lit::Vec4(v * n)),
                    (Lit::Vec4(v), "/", Lit::F32(n)) => Ok(Lit::Vec4(v / n)),

                    (Lit::Mat4x4(m), "*", Lit::Vec4(v)) => Ok(Lit::Vec4(m * v)),
                    (Lit::F32(n), "%", Lit::F32(n2)) => Ok(Lit::F32(n % n2)),
                    (Lit::Vec4(v), "%", Lit::Vec4(v2)) => Ok(Lit::Vec4(v % v2)),


                    // boolean ops
                    (Lit::Bool(b1), "&&", Lit::Bool(b2)) => Ok(Lit::Bool(b1 && b2)),
                    (Lit::Bool(b1), "||", Lit::Bool(b2)) => Ok(Lit::Bool(b1 || b2)),
                    (x1, "==", x2) => Ok(Lit::Bool(x1 == x2)),
                    (x1, "!=", x2) => Ok(Lit::Bool(x1 != x2)),
                    (Lit::F32(n1), "<", Lit::F32(n2)) => Ok(Lit::Bool(n1 < n2)),
                    (Lit::F32(n1), ">", Lit::F32(n2)) => Ok(Lit::Bool(n1 > n2)),
                    (Lit::F32(n1), "<=", Lit::F32(n2)) => Ok(Lit::Bool(n1 <= n2)),
                    (Lit::F32(n1), ">=", Lit::F32(n2)) => Ok(Lit::Bool(n1 >= n2)),


                    (f1, symbol, f2) => Err(anyhow!(
                        "Could not find binary operation with signature {} {} {}",
                        f1.get_type(), symbol, f2.get_type()
                    ))
                }
            Expr::UnaryExpr(symbol, right) => {
                match (symbol.as_str(), right.eval(scope)?) {
                    ("-", Lit::F32(n)) => Ok(Lit::F32(-n)),
                    ("-", Lit::Vec4(v)) => Ok(Lit::Vec4(-v)),
                    ("-", Lit::Mat4x4(m)) => Ok(Lit::Mat4x4(-m)),
                    ("+", Lit::F32(n)) => Ok(Lit::F32(n)),
                    ("+", Lit::Vec4(v)) => Ok(Lit::Vec4(v)),
                    ("+", Lit::Mat4x4(m)) => Ok(Lit::Mat4x4(m)),
                    ("!", Lit::Bool(b)) => Ok(Lit::Bool(!b)),

                    (symbol, right) => Err(anyhow!(
                        "Could not find unary operation with signature {} {}",
                        symbol, right.get_type()
                    ))
                }
            }
            Expr::Application(fn_name, args) => {
                let inputs = args.iter()
                    .map(|arg| arg.eval(scope))
                    .collect::<AnyResult<Vec<_>>>()?;

                match (fn_name.as_str(), &inputs[..]) {
                    //constructors
                    ("vec4", &[
                        Lit::F32(x), Lit::F32(y), Lit::F32(z), Lit::F32(w)
                    ]) => Ok(Lit::Vec4(Vec4::new(x, y, z, w))),
                    ("mat4x4", &[
                        Lit::F32(a), Lit::F32(b), Lit::F32(c), Lit::F32(d),
                        Lit::F32(e), Lit::F32(f), Lit::F32(g), Lit::F32(h),
                        Lit::F32(i), Lit::F32(j), Lit::F32(k), Lit::F32(l),
                        Lit::F32(m), Lit::F32(n), Lit::F32(o), Lit::F32(p)
                    ]) => Ok(Lit::Mat4x4(Mat4::from_cols_array_2d(&[
                        [a, b, c, d],
                        [e, f, g, h],
                        [i, j, k, l],
                        [m, n, o, p],
                    ]))),
                    ("mat4x4", &[
                        Lit::Vec4(v1), Lit::Vec4(v2), Lit::Vec4(v3), Lit::Vec4(v4)
                    ]) => Ok(Lit::Mat4x4(Mat4::from_cols(v1, v2, v3, v4))),

                    ("normalize", &[Lit::Vec4(vector)]) => Ok(Lit::Vec4(vector.normalize())),
                    ("dot", [
                        Lit::Vec4(vector),
                        Lit::Vec4(vector2),
                    ]) => Ok(Lit::F32(vector.dot(vector2.clone()))),
                    ("length", &[Lit::Vec4(vector)]) => Ok(Lit::F32(vector.length())),
                    ("length_squared",& [Lit::Vec4(vector)]) => Ok(Lit::F32(vector.length_squared())),
                    // distance fn
                    ("distance", &[Lit::Vec4(v1), Lit::Vec4(v2)]) => Ok(Lit::F32(v1.distance(v2))),

                    ("max", &[Lit::F32(f1), Lit::F32(f2)]) => Ok(Lit::F32(f1.max(f2))),
                    ("min", &[Lit::F32(f1), Lit::F32(f2)]) => Ok(Lit::F32(f1.min(f2))),
                    ("max", &[Lit::Vec4(v1), Lit::Vec4(v2)]) => Ok(Lit::Vec4(v1.max(v2))),
                    ("min", &[Lit::Vec4(v1), Lit::Vec4(v2)]) => Ok(Lit::Vec4(v1.min(v2))),
                    ("clamp", &[Lit::F32(f), Lit::F32(min), Lit::F32(max)]) => Ok(Lit::F32(f.clamp(min, max))),
                    ("clamp", &[Lit::Vec4(v), Lit::Vec4(min), Lit::Vec4(max)]) => Ok(Lit::Vec4(v.clamp(min, max))),
                    ("cos", &[Lit::F32(f)]) => Ok(Lit::F32(f.cos())),
                    ("sin", &[Lit::F32(f)]) => Ok(Lit::F32(f.sin())),
                    ("tan", &[Lit::F32(f)]) => Ok(Lit::F32(f.tan())),
                    ("acos", &[Lit::F32(f)]) => Ok(Lit::F32(f.acos())),
                    ("asin", &[Lit::F32(f)]) => Ok(Lit::F32(f.asin())),
                    ("atan", &[Lit::F32(f)]) => Ok(Lit::F32(f.atan())),
                    ("atan2", &[Lit::F32(f1), Lit::F32(f2)]) => Ok(Lit::F32(f1.atan2(f2))),
                    ("pow", &[Lit::F32(f1), Lit::F32(f2)]) => Ok(Lit::F32(f1.powf(f2))),
                    ("sqrt", &[Lit::F32(f)]) => Ok(Lit::F32(f.sqrt())),
                    ("exp", &[Lit::F32(f)]) => Ok(Lit::F32(f.exp())),
                    // ("ln", [Lit::F32(f)]) => Ok(Lit::F32(f.ln())),
                    ("log2", &[Lit::F32(f)]) => Ok(Lit::F32(f.log2())),
                    // ("log10", [Lit::F32(f)]) => Ok(Lit::F32(f.log10())),
                    ("abs", &[Lit::F32(f)]) => Ok(Lit::F32(f.abs())),
                    ("abs", &[Lit::Vec4(v)]) => Ok(Lit::Vec4(v.abs())),
                    ("floor", &[Lit::F32(f)]) => Ok(Lit::F32(f.floor())),
                    //ceil
                    ("ceil", &[Lit::F32(f)]) => Ok(Lit::F32(f.ceil())),
                    ("round", &[Lit::F32(f)]) => Ok(Lit::F32(f.round())),
                    ("fract", &[Lit::F32(f)]) => Ok(Lit::F32(f.fract())),
                    ("fract", &[Lit::Vec4(v)]) => Ok(Lit::Vec4(v.fract())),
                    ("trunc", &[Lit::F32(f)]) => Ok(Lit::F32(f.trunc())),
                    ("trunc", &[Lit::Vec4(v)]) => Ok(Lit::Vec4(v.trunc())),

                    ("select", [x_false, x_true, Lit::Bool(condition)]) => {
                        if !x_false.matches_type(x_true) {
                            Err(anyhow!("Types do not match in select for {} and {}", x_false.get_type(), x_true.get_type()))
                        } else if *condition {
                            Ok(x_true.clone())
                        } else {
                            Ok(x_false.clone())
                        }
                    }

                    (fn_name, inputs) => Err(anyhow!(
                        "Could not find signature {fn_name}({})",
                        inputs.iter().map(|input| format!("{}", input.get_type())).collect::<Vec<_>>().join(", ")
                    ))
                }
            },
            Expr::Dot(_, _, _) => Err(anyhow!("EVAL NOT SUPPORTED FOR DOT")),
            Expr::Tuple(elements) => {
                Ok(Lit::Tuple(elements.iter().map(|expr| expr.eval(scope)).collect::<AnyResult<Vec<_>>>()?))
            },
            Expr::Var(var) => match var.as_str() {
                "X" => Ok(Lit::Vec4(Vec4::X)),
                "Y" => Ok(Lit::Vec4(Vec4::Y)),
                "Z" => Ok(Lit::Vec4(Vec4::Z)),
                "W" => Ok(Lit::Vec4(Vec4::W)),
                "ONES" => Ok(Lit::Vec4(Vec4::ONE)),
                "PI" => Ok(Lit::F32(std::f32::consts::PI)),
                "E" => Ok(Lit::F32(std::f32::consts::E)),
                "IDENTITY" => Ok(Lit::Mat4x4(Mat4::IDENTITY)),
                var_string => scope.get(var_string).with_context(|| format!("var {var} not found in scope")).cloned()
            },
            Expr::Lit(lit) => Ok(lit.clone()),
            Expr::Block(block) => block.eval(scope)
        }
    }
}

#[cfg(test)]
mod tests {
    use glam::Vec4;
    use crate::assemble::AssembledStructure;
    use crate::interpreter::{Eval};
    use crate::parser::parse_block;
    use crate::test_helpers::{get_test_stuff, prettify_string};

    #[test]
    fn try_eval_block() {
        let block = parse_block(r#"{
            let x: f32 = 4;
            select(x + 2, 2, x < x + 1)
        }"#).unwrap();

        println!("Eval: {}", block.eval_into::<f32>().unwrap())
    }

    #[test]
    fn try_eval_block_tuple() {
        let block = parse_block(r#"{
            let x: f32 = 4;
            select((x + 2, 5), (2, x), x < x + 1)
        }"#).unwrap();

        let (min, max): (f32, f32) = block.eval_into().unwrap();

        println!("Eval: ({}, {})", min, max);
    }

    #[test]
    fn try_eval() {
        let (document, structure) = get_test_stuff(0, 1);
        println!("Document: {document}\nStructure: {structure}");

        let assembled_structure = AssembledStructure::new(&document, structure).unwrap();

        println!("Assembled Structure: {}", prettify_string(format!("{assembled_structure}")));

        println!("result: {}", assembled_structure.eval_method::<Vec4>("proj", 5.0 * Vec4::X + Vec4::Y).unwrap())
    }
}
