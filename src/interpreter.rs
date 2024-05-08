use glam::{Mat4, Vec4};
use crate::assemble::{AssembledField, AssembledStructure, get_test_stuff};
use crate::parser::{Binding, Block, Expr, Lit, Method, MethodKey, prettify_string, Stmt};
use anyhow::{anyhow, Context, Result as AnyResult};

pub trait IntoArgs {
    fn into_args(self) -> Vec<AssembledField>;
}

impl IntoArgs for Vec<AssembledField> {
    fn into_args(self) -> Vec<AssembledField> {
        self
    }
}

macro_rules! impl_into_args {
    ($($ty:ident),*) => {
        #[allow(non_camel_case_types)]
        impl<$($ty : Into<AssembledField>),*> IntoArgs for ($($ty),*) {
            fn into_args(self) -> Vec<AssembledField> {
                let ( $($ty),* ) = self;

                vec![ $( $ty .into() ),* ]
            }
        }
    };
}



impl_into_args!();
impl_into_args!(a);
impl_into_args!(a, b);
impl_into_args!(a, b, c);
impl_into_args!(a, b, c, d);
impl_into_args!(a, b, c, d, e);
impl_into_args!(a, b, c, d, e, f);
impl_into_args!(a, b, c, d, e, f, g);
impl_into_args!(a, b, c, d, e, f, g, h);
impl_into_args!(a, b, c, d, e, f, g, h, i);
impl_into_args!(a, b, c, d, e, f, g, h, i, j);
impl_into_args!(a, b, c, d, e, f, g, h, i, j, k);
impl_into_args!(a, b, c, d, e, f, g, h, i, j, k, l);
impl_into_args!(a, b, c, d, e, f, g, h, i, j, k, l, m);
impl_into_args!(a, b, c, d, e, f, g, h, i, j, k, l, m, n);
impl_into_args!(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o);
impl_into_args!(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p);
impl_into_args!(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q);

impl TryFrom<AssembledField> for f32 {
    type Error = anyhow::Error;

    fn try_from(field: AssembledField) -> AnyResult<Self> {
        match field {
            AssembledField::F32(f) => Ok(f),
            _ => Err(anyhow!("Field not supported"))
        }
    }
}

impl TryFrom<AssembledField> for bool {
    type Error = anyhow::Error;

    fn try_from(field: AssembledField) -> AnyResult<Self> {
        match field {
            AssembledField::Bool(b) => Ok(b),
            _ => Err(anyhow!("Field not supported"))
        }
    }
}

impl TryFrom<AssembledField> for Vec4 {
    type Error = anyhow::Error;

    fn try_from(field: AssembledField) -> AnyResult<Self> {
        match field {
            AssembledField::Vec4(v) => Ok(v),
            _ => Err(anyhow!("Field not supported"))
        }
    }
}

impl TryFrom<AssembledField> for Mat4 {
    type Error = anyhow::Error;

    fn try_from(field: AssembledField) -> AnyResult<Self> {
        match field {
            AssembledField::Mat4(m) => Ok(m),
            _ => Err(anyhow!("Field not supported"))
        }
    }
}

impl From<f32> for AssembledField {
    fn from(f: f32) -> AssembledField {
        AssembledField::F32(f)
    }
}

impl From<bool> for AssembledField {
    fn from(b: bool) -> AssembledField {
        AssembledField::Bool(b)
    }
}

impl From<Vec4> for AssembledField {
    fn from(v: Vec4) -> AssembledField {
        AssembledField::Vec4(v)
    }
}

impl From<Mat4> for AssembledField {
    fn from(m: Mat4) -> AssembledField {
        AssembledField::Mat4(m)
    }
}

struct Scope(Vec<(String, AssembledField)>);

impl Scope {
    fn get(&self, name: impl AsRef<str>) -> Option<&AssembledField> {
        self.0.iter().find(|(n, _)| n.as_str() == name.as_ref()).map(|(_, field)| field)
    }

    fn get_mut(&mut self, name: impl AsRef<str>) -> Option<&mut AssembledField> {
        self.0.iter_mut().find(|(n, _)| n.as_str() == name.as_ref()).map(|(_, field)| field)
    }

    fn push(&mut self, name: String, field: AssembledField) {
        self.0.push((name, field));
    }
}

impl AssembledStructure {
    pub fn eval_method<OUT: TryFrom<AssembledField, Error=anyhow::Error>>(&self, method_name: impl AsRef<str>, args: impl IntoArgs) -> AnyResult<OUT> {
        let method = self.methods.iter()
            .find(|Method { name, .. } | name.as_str() == method_name.as_ref())
            .with_context(|| format!("Could not find method {}", method_name.as_ref()))?;

        let mut scope = Scope(
            method.inputs.iter()
                .zip(args.into_args().into_iter())
                .map(|(Binding(name, _), field)| (name.clone(), field))
                .collect::<Vec<_>>()
        );

        scope.0.append(&mut self.fields.clone());

        Ok(method.body.eval(&mut scope)?.try_into()?)
    }
}

trait Eval {
    fn eval(&self, scope: &mut Scope) -> AnyResult<AssembledField>;
}

impl Eval for Block {
    fn eval(&self, scope: &mut Scope) -> AnyResult<AssembledField> {
        for stmt in self.0.iter() {
            stmt.eval(scope)?;
        }

        if let Some(expr) = &self.1 {
            Ok(expr.eval(scope)?)
        } else {
            Ok(AssembledField::Unit)
        }
    }
}

impl Eval for Stmt {
    fn eval(&self, scope: &mut Scope) -> AnyResult<AssembledField> {
        match self {
            Stmt::Declare(Binding(var, _), expr) => {
                let eval = expr.eval(scope)?;
                scope.push(var.clone(), eval);
            }
            Stmt::Assign(var, expr) => {
                let eval_field = expr.eval(scope)?;
                let field = scope.get_mut(var).with_context(|| format!("var {var} not found in scope"))?;

                if field.types_match(&eval_field) {
                    *field = eval_field;
                } else {
                    return Err(anyhow!(
                        "Types do not match: {} and {} when assigning var {var}",
                        field.type_string(), eval_field.type_string()
                    ));
                }
            }
            Stmt::Expr(expr) => {
                expr.eval(scope)?;
            }
            Stmt::Noop => {}
        }

        Ok(AssembledField::Unit)
    }
}

impl Eval for Expr {
    fn eval(&self, scope: &mut Scope) -> AnyResult<AssembledField> {
        use AssembledField as AF;

        match self {
            Expr::BinExpr(left, symbol, right) =>
                match (left.eval(scope)?, symbol.as_str(), right.eval(scope)?) {
                    (AF::F32(n1), "+", AF::F32(n2)) => Ok(AF::F32(n1 + n2)),
                    (AF::F32(n1), "-", AF::F32(n2)) => Ok(AF::F32(n1 - n2)),
                    (AF::F32(n1), "*", AF::F32(n2)) => Ok(AF::F32(n1 * n2)),
                    (AF::F32(n1), "/", AF::F32(n2)) => Ok(AF::F32(n1 / n2)),

                    (AF::Vec4(v1), "+", AF::Vec4(v2)) => Ok(AF::Vec4(v1 + v2)),
                    (AF::Vec4(v1), "-", AF::Vec4(v2)) => Ok(AF::Vec4(v1 - v2)),

                    (AF::F32(n), "*", AF::Vec4(v)) => Ok(AF::Vec4(n * v)),
                    (AF::Vec4(v), "*", AF::F32(n)) => Ok(AF::Vec4(v * n)),
                    (AF::Vec4(v), "/", AF::F32(n)) => Ok(AF::Vec4(v / n)),

                    (AF::Mat4(m), "*", AF::Vec4(v)) => Ok(AF::Vec4(m * v)),

                    (f1, symbol, f2) => Err(anyhow!(
                        "Could not find binary operation with signature {} {} {}",
                        f1.type_string(), symbol, f2.type_string()
                    ))
                }
            Expr::UnaryExpr(symbol, right) => {
                match (symbol.as_str(), right.eval(scope)?) {
                    ("-", AF::F32(n)) => Ok(AF::F32(-n)),
                    ("-", AF::Vec4(v)) => Ok(AF::Vec4(-v)),
                    ("-", AF::Mat4(m)) => Ok(AF::Mat4(-m)),
                    ("!", AF::Bool(b)) => Ok(AF::Bool(!b)),


                    (symbol, right) => Err(anyhow!(
                        "Could not find unary operation with signature {} {}",
                        symbol, right.type_string()
                    ))
                }
            }
            Expr::Application(fn_name, args) => {
                let inputs = args.iter()
                    .map(|arg| arg.eval(scope))
                    .collect::<AnyResult<Vec<_>>>()?;

                match (fn_name.as_str(), &inputs[..]) {
                    ("normalize", [AF::Vec4(vector)]) => Ok(AF::Vec4(vector.normalize())),
                    ("dot", [
                        AF::Vec4(vector),
                        AF::Vec4(vector2),
                    ]) => Ok(AF::F32(vector.dot(vector2.clone()))),

                    (fn_name, inputs) => Err(anyhow!(
                        "Could not find signature {fn_name}({})",
                        inputs.iter().map(|input| input.type_string()).collect::<Vec<_>>().join(", ")
                    ))
                }
            },
            Expr::Dot(_, _, _) => Err(anyhow!("EVAL NOT SUPPORTED FOR DOT")),
            Expr::Var(var) => Ok(scope.get(var).with_context(|| format!("var {var} not found in scope"))?.clone()),
            Expr::Lit(lit) => match lit {
                Lit::F32(f) => Ok(AssembledField::F32(*f)),
                Lit::Bool(b) => Ok(AssembledField::Bool(*b))
            }
            Expr::Block(block) => block.eval(scope)
        }
    }
}


#[test]
fn try_eval() {
    let (document, structure) = get_test_stuff(0, 1);
    println!("Document: {document}\nStructure: {structure}");

    let assembled_structure = AssembledStructure::new(&document, structure).unwrap();

    println!("Assembled Structure: {}", prettify_string(format!("{assembled_structure}")));

    println!("result: {}", assembled_structure.eval_method::<Vec4>("proj", 5.0 * Vec4::X + Vec4::Y).unwrap())
}