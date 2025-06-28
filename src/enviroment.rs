use std::any::{type_name, Any, TypeId};
use std::borrow::Borrow;
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::marker::PhantomData;
use std::ops::Deref;
use std::sync::{Arc};
use parking_lot::{MappedRwLockReadGuard, RwLock, RwLockReadGuard, RwLockWriteGuard};
use glam::{Mat4, Vec4};
use once_cell::sync::Lazy;
use crate::parser::{Function, Lit, Type};
use crate::any_value::{AnyValue, AsDynPartialEq};

#[test]
fn test() {
    let mut env = Environment::new();

    env.register_type::<String>(SslIdentifier::new("String"));
    let _ = env.register_fn(|a: f32| {2.0}, SslIdentifier::new("hello"));

    println!("{:#?}", env.inner())
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct SslIdentifier {
    name: String,
    wgsl_name: Option<String>
}

impl SslIdentifier {
    pub fn new(name: impl AsRef<str>) -> Self {
        Self {
            name: name.as_ref().to_string(),
            wgsl_name: None
        }
    }

    pub fn new_with_wgsl_name(name: impl AsRef<str>, wgsl_name: impl AsRef<str>) -> Self {
        Self {
            name: name.as_ref().to_string(),
            wgsl_name: Some(wgsl_name.as_ref().to_string())
        }
    }

    pub fn name(&self) -> &String {
        &self.name
    }

    pub fn wgsl_name(&self) -> &String {
        self.wgsl_name.as_ref().unwrap_or(&self.name)
    }
}

impl From<&str> for SslIdentifier {
    fn from(value: &str) -> Self {
        SslIdentifier::new(value)
    }
}

impl From<String> for SslIdentifier {
    fn from(value: String) -> Self {
        SslIdentifier::new(value)
    }
}

#[derive(Debug)]
pub enum RegisterError {
    TypeNotRegistered(usize, String),

}


static GLOBAL_ENV: Lazy<Environment> = Lazy::new(|| {
    let mut env = Environment::new();

    env.register_unary_op(|n: f32| -n, "-".into()).unwrap();
    env.register_unary_op(|v: Vec4| -v, "-".into()).unwrap();
    env.register_unary_op(|m: Mat4| -m, "-".into()).unwrap();
    env.register_unary_op(|n: f32| n, "+".into()).unwrap();
    env.register_unary_op(|v: Vec4| v, "+".into()).unwrap();
    env.register_unary_op(|m: Mat4| m, "+".into()).unwrap();
    env.register_unary_op(|b: bool| !b, "!".into()).unwrap();

    env.register_binary_op(|n1: f32, n2: f32| n1 + n2, "+".into()).unwrap();
    env.register_binary_op(|n1: f32, n2: f32| n1 - n2, "-".into()).unwrap();
    env.register_binary_op(|n1: f32, n2: f32| n1 * n2, "*".into()).unwrap();
    env.register_binary_op(|n1: f32, n2: f32| n1 / n2, "/".into()).unwrap();
    env.register_binary_op(|v1: Vec4, v2: Vec4| v1 * v2, "*".into()).unwrap();
    env.register_binary_op(|v1: Vec4, v2: Vec4| v1 / v2, "/".into()).unwrap();

    env.register_binary_op(|v1: Vec4, v2: Vec4| v1 + v2, "+".into()).unwrap();
    env.register_binary_op(|v1: Vec4, v2: Vec4| v1 - v2, "-".into()).unwrap();

    env.register_binary_op(|n: f32, v: Vec4| n * v, "*".into()).unwrap();
    env.register_binary_op(|v: Vec4, n: f32| v * n, "*".into()).unwrap();
    env.register_binary_op(|v: Vec4, n: f32| v / n, "/".into()).unwrap();

    env.register_binary_op(|m: Mat4, m2: Mat4| m * m2, "*".into()).unwrap();
    env.register_binary_op(|m: Mat4, n: f32| m * n, "*".into()).unwrap();
    env.register_binary_op(|n: f32, m: Mat4| n * m, "*".into()).unwrap();
    env.register_binary_op(|m: Mat4, v: Vec4| m * v, "*".into()).unwrap();
    env.register_binary_op(|n: f32, n2: f32| n % n2, "%".into()).unwrap();
    env.register_binary_op(|v: Vec4, v2: Vec4| v % v2, "%".into()).unwrap();

    // boolean ops
    env.register_binary_op(|b1: bool, b2: bool| b1 && b2, "&&".into()).unwrap();
    env.register_binary_op(|b1: bool, b2: bool| b1 || b2, "||".into()).unwrap();
    env.register_binary_op(|n1: f32, n2: f32| n1 < n2, "<".into()).unwrap();
    env.register_binary_op(|n1: f32, n2: f32| n1 > n2, ">".into()).unwrap();
    env.register_binary_op(|n1: f32, n2: f32| n1 <= n2, "<=".into()).unwrap();
    env.register_binary_op(|n1: f32, n2: f32| n1 >= n2, ">=".into()).unwrap();

    // Register constructors
    env.register_fn(
        |x: f32, y: f32, z: f32, w: f32| Vec4::new(x, y, z, w),
        "vec4".into(),
    ).unwrap();

    env.register_fn(
        |v1: Vec4, v2: Vec4, v3: Vec4, v4: Vec4| Mat4::from_cols(v1, v2, v3, v4),
        "mat4x4".into(),
    ).unwrap();

    // Register utility functions
    env.register_fn(|vector: Vec4| vector.normalize(), "normalize".into()).unwrap();
    env.register_fn(|vector: Vec4, vector2: Vec4| vector.dot(vector2), "dot".into()).unwrap();
    env.register_fn(|vector: Vec4| vector.length(), "length".into()).unwrap();
    env.register_fn(|vector: Vec4| vector.length_squared(), "length_squared".into()).unwrap();
    env.register_fn(|v1: Vec4, v2: Vec4| v1.distance(v2), "distance".into()).unwrap();

    // Register mathematical functions
    env.register_fn(|f1: f32, f2: f32, t: f32| f1 * (1.0 - t) + f2 * t, "mix".into()).unwrap();
    env.register_fn(|v1: Vec4, v2: Vec4, t: f32| v1 * (1.0 - t) + v2 * t, "mix".into()).unwrap();
    env.register_fn(|m1: Mat4, m2: Mat4, t: f32| m1 * (1.0 - t) + m2 * t, "mix".into()).unwrap();
    env.register_fn(|edge: f32, x: f32| if x < edge { 0.0 } else { 1.0 }, "step".into()).unwrap();
    env.register_fn(
        |edge: Vec4, x: Vec4| Vec4::new(
            if x.x < edge.x { 0.0 } else { 1.0 },
            if x.y < edge.y { 0.0 } else { 1.0 },
            if x.z < edge.z { 0.0 } else { 1.0 },
            if x.w < edge.w { 0.0 } else { 1.0 },
        ),
        "step".into(),
    ).unwrap();

    // Register smoothstep function
    env.register_fn(
        |edge0: f32, edge1: f32, x: f32| {
            let t = ((x - edge0) / (edge1 - edge0)).clamp(0.0, 1.0);
            t * t * (3.0 - 2.0 * t)
        },
        "smoothstep".into(),
    ).unwrap();

    env.register_fn(
        |edge0: Vec4, edge1: Vec4, x: Vec4| {
            let t = ((x - edge0) / (edge1 - edge0)).clamp(Vec4::ZERO, Vec4::ONE);
            t * t * (Vec4::ONE * 3.0 - Vec4::ONE * 2.0 * t)
        },
        "smoothstep".into(),
    ).unwrap();

    // Register max, min, clamp, and other mathematical functions
    env.register_fn(|f1: f32, f2: f32| f1.max(f2), "max".into()).unwrap();
    env.register_fn(|f1: f32, f2: f32| f1.min(f2), "min".into()).unwrap();
    env.register_fn(|v1: Vec4, v2: Vec4| v1.max(v2), "max".into()).unwrap();
    env.register_fn(|v1: Vec4, v2: Vec4| v1.min(v2), "min".into()).unwrap();
    env.register_fn(|f: f32, min: f32, max: f32| f.clamp(min, max), "clamp".into()).unwrap();
    env.register_fn(|v: Vec4, min: Vec4, max: Vec4| v.clamp(min, max), "clamp".into()).unwrap();
    env.register_fn(|f: f32| f.cos(), "cos".into()).unwrap();
    env.register_fn(|f: f32| f.sin(), "sin".into()).unwrap();
    env.register_fn(|f: f32| f.tan(), "tan".into()).unwrap();
    env.register_fn(|f: f32| f.acos(), "acos".into()).unwrap();
    env.register_fn(|f: f32| f.asin(), "asin".into()).unwrap();
    env.register_fn(|f: f32| f.atan(), "atan".into()).unwrap();
    env.register_fn(|f1: f32, f2: f32| f1.atan2(f2), "atan2".into()).unwrap();
    env.register_fn(|f1: f32, f2: f32| f1.powf(f2), "pow".into()).unwrap();
    env.register_fn(|f: f32| f.sqrt(), "sqrt".into()).unwrap();
    env.register_fn(|f: f32| f.exp(), "exp".into()).unwrap();
    env.register_fn(|f: f32| f.log2(), "log2".into()).unwrap();
    env.register_fn(|f: f32| f.abs(), "abs".into()).unwrap();
    env.register_fn(|v: Vec4| v.abs(), "abs".into()).unwrap();
    env.register_fn(|f: f32| f.floor(), "floor".into()).unwrap();
    env.register_fn(|f: f32| f.ceil(), "ceil".into()).unwrap();
    env.register_fn(|f: f32| f.round(), "round".into()).unwrap();
    env.register_fn(|f: f32| f.fract(), "fract".into()).unwrap();
    env.register_fn(|v: Vec4| v.fract(), "fract".into()).unwrap();
    env.register_fn(|f: f32| f.trunc(), "trunc".into()).unwrap();
    env.register_fn(|v: Vec4| v.trunc(), "trunc".into()).unwrap();
    env.register_fn(|f: f32| f.signum(), "sign".into()).unwrap();
    env.register_fn(|v: Vec4| v.signum(), "sign".into()).unwrap();

    env.register_field((|v: Vec4| v.x, |mut v: Vec4, n: f32| {v.x = n; v}), "x".into(), None).unwrap();
    env.register_field((|v: Vec4| v.y, |mut v: Vec4, n: f32| {v.y = n; v}), "y".into(), None).unwrap();
    env.register_field((|v: Vec4| v.z, |mut v: Vec4, n: f32| {v.z = n; v}), "z".into(), None).unwrap();
    env.register_field((|v: Vec4| v.w, |mut v: Vec4, n: f32| {v.w = n; v}), "w".into(), None).unwrap();

    env.register_const("X".into(), Vec4::X.lit(&env));
    env.register_const("Y".into(), Vec4::Y.lit(&env));
    env.register_const("Z".into(), Vec4::Z.lit(&env));
    env.register_const("W".into(), Vec4::W.lit(&env));
    env.register_const("ONES".into(), Vec4::ONE.lit(&env));
    env.register_const("ZEROS".into(), Vec4::ZERO.lit(&env));
    env.register_const("PI".into(), std::f32::consts::PI.lit(&env));
    env.register_const("E".into(), std::f32::consts::E.lit(&env));
    env.register_const("IDENTITY".into(), Mat4::IDENTITY.lit(&env));
    env.register_const("INFINITY".into(), f32::INFINITY.lit(&env));

    env
});

#[derive(Debug, Clone)]
/// Holds all registered data for Ssl: unary ops, binary ops, functions, consts, types, fields
pub struct Environment(Arc<RwLock<EnvironmentInner>>);

impl Environment {
    pub fn new() -> Self {
        Environment(Arc::new(RwLock::new(EnvironmentInner::new())))
    }

    fn inner(&self) -> RwLockReadGuard<EnvironmentInner> {
        self.0.read()
    }

    fn inner_mut(&mut self) -> RwLockWriteGuard<EnvironmentInner> {
        self.0.write()
    }

    fn get_item<T>(&self, f: &impl Fn(&EnvironmentInner) -> Option<&T>) -> Option<MappedRwLockReadGuard<T>> {
        if let Some(item) = RwLockReadGuard::try_map(self.inner(), f).ok() {
            Some(item)
        } else if let Some(item) = RwLockReadGuard::try_map(GLOBAL_ENV.inner(), f).ok() {
            Some(item)
        } else {
            None
        }
    }


    pub fn register_unary_op<In1: SslType, Out: SslType, FN: IntoSslUnaryOp<In1, Out>>(
        &mut self,
        function: FN,
        sym: SslIdentifier
    ) -> Result<(), RegisterError> {
        let in_type = In1::ssl_type(&self);

        self.inner_mut().unary_ops.insert(
            (sym.name, in_type),
            (sym.wgsl_name, Box::new(function.into_callable_function()))
        );

        Ok(())
    }

    pub fn get_unary_op(&self, sym: impl AsRef<str>, in1_type: Type) -> Option<MappedRwLockReadGuard<(Option<String>, Box<dyn SslUnaryOp>)>> {
        self.get_item(&|inner| {
            inner.unary_ops.get(&(sym.as_ref().to_string(), in1_type.clone()))
        })
    }


    pub fn register_binary_op<In1: SslType, In2: SslType, Out: SslType, FN: IntoSslBinaryOp<In1, In2, Out>>(
        &mut self,
        function: FN,
        sym: SslIdentifier
    ) -> Result<(), RegisterError> {
        let in1_type = In1::ssl_type(&self);
        let in2_type = In2::ssl_type(&self);

        self.inner_mut().binary_ops.insert(
            (sym.name, in1_type, in2_type),
            (sym.wgsl_name, Box::new(function.into_callable_function()))
        );

        Ok(())
    }

    pub fn get_binary_op(&self, sym: impl AsRef<str>, in1_type: Type, in2_type: Type) -> Option<MappedRwLockReadGuard<(Option<String>, Box<dyn SslBinaryOp>)>> {
        self.get_item(&|inner| {
            inner.binary_ops.get(&(sym.as_ref().to_string(), in1_type.clone(), in2_type.clone()))
        })
    }


    pub fn register_fn<Params: FunctionParams, Out: SslType, FN: IntoSslCallableFn<Params, Out>>(
        &mut self,
        function: FN,
        name: SslIdentifier
    ) -> Result<(), RegisterError> {
        let input_types = Params::input_types(self);

        self.inner_mut().functions.insert(
            (name.name, input_types),
            (name.wgsl_name, Box::new(function.into_callable_function()))
        );

        Ok(())
    }

    pub fn get_fn(&self, sym: impl AsRef<str>, ins: Vec<Type>) -> Option<MappedRwLockReadGuard<(Option<String>, Box<dyn SslCallableFn>)>> {
        self.get_item(&|inner| {
            inner.functions.get(&(sym.as_ref().to_string(), ins.clone()))
        })
    }


    pub fn register_const(&mut self, sym: SslIdentifier, lit: Lit) {
        self.inner_mut().consts.insert(sym.name, (sym.wgsl_name, lit));
    }

    pub fn get_const(&self, sym: impl AsRef<str>) -> Option<MappedRwLockReadGuard<(Option<String>, Lit)>> {
        self.get_item(&|inner| {
            inner.consts.get(&sym.as_ref().to_string())
        })
    }


    pub fn register_field<Var: SslType, Value: SslType, FN: IntoSslField<Var, Value>>(
        &mut self,
        function: FN,
        name: SslIdentifier,
        wgsl_index: Option<usize>
    ) -> Result<(), RegisterError> {
        let in_type = Var::ssl_type(&self);

        self.inner_mut().field.insert(
            (name.name, in_type),
            (name.wgsl_name, wgsl_index, Box::new(function.into_callable_function()))
        );

        Ok(())
    }

    pub fn get_field(&self, name: impl AsRef<str>, in1_type: Type) -> Option<MappedRwLockReadGuard<(Option<String>, Option<usize>, Box<dyn SslField>)>> {
        self.get_item(&|inner| {
            inner.field.get(&(name.as_ref().to_string(), in1_type.clone()))
        })
    }


    pub fn register_type<T: Any>(&mut self, type_name: SslIdentifier) {
        self.inner_mut().types.push((type_name.name.clone(), TypeId::of::<T>(), type_name.wgsl_name))
    }

    pub fn get_registered_type<T: Any>(&self) -> Option<Type> {
        let type_id = TypeId::of::<T>();

        self.inner().types.iter()
            .find(|(_, other_type_id, _)| type_id == *other_type_id)
            .map(|(name, _, _)| Type::Custom(name.clone()))
    }

    pub fn type_name_exists(&self, name: &String) -> bool {
        self.inner().types.iter()
            .any(|(other_name, _, _)| name == other_name)
    }

    pub fn get_wgsl_name(&self, ty: &Type) -> Option<String> {
        match ty {
            Type::F32 => Some("f32".to_string()),
            Type::Bool => Some("bool".to_string()),
            // If you add angle-brackets, you'll need to re-write how tuple names are made
            Type::Vec4 => Some("vec4".to_string()),
            Type::Mat4x4 => Some("mat4x4".to_string()),
            Type::Unit => Some("unit".to_string()),
            Type::Custom(type_name) => {
                let inner = self.inner();
                let Some((name, _, wgsl_name)) = inner.types.iter().find(|(name, _, _)| name == type_name) else {
                    return None
                };

                Some(wgsl_name.as_ref().unwrap_or(&name).clone())
            }
            Type::Tuple(tuple_type) => {
                Some(format!(
                    "tuple_{}_0",
                    tuple_type.iter()
                        .map(|ty| self.get_wgsl_name(ty))
                        .collect::<Option<Vec<String>>>()?.join("_")
                ))
            }
            _ => None
        }
    }

    pub fn get_type_name(&self, type_id: &TypeId) -> Option<String> {
        self.inner().types.iter()
            .find(|(_, other_type_id, _)| other_type_id == type_id)
            .map(|(name, _, _)| name.clone())
    }
}

#[derive(Debug)]
struct EnvironmentInner {
    /// Map from registered unary ops defined by (symbol, input) to (wgsl_symbol, fn)
    unary_ops: HashMap<(String, Type), (Option<String>, Box<dyn SslUnaryOp>)>,
    /// Map from registered binary ops defined by (symbol, input1, input2) to (wgsl_symbol, fn)
    binary_ops: HashMap<(String, Type, Type), (Option<String>, Box<dyn SslBinaryOp>)>,
    /// Map from registered functions defined by (name, inputs) to (wgsl_name, fn)
    functions: HashMap<(String, Vec<Type>), (Option<String>, Box<dyn SslCallableFn>)>,
    /// Map from registered constants to (wgsl_name, value)
    consts: HashMap<String, (Option<String>, Lit)>,
    /// Map from (field_name, type) to (wgsl_name, wgsl_index_opt, get_field)
    field: HashMap<(String, Type), (Option<String>, Option<usize>, Box<dyn SslField>)>,
    /// List of registered types, (name, type_id, wgsl_name)
    types: Vec<(String, TypeId, Option<String>)>,
}

impl EnvironmentInner {
    fn new() -> Self {
        EnvironmentInner {
            unary_ops: Default::default(),
            binary_ops: Default::default(),
            functions: Default::default(),
            consts: Default::default(),
            types: vec![],
            field: Default::default(),
        }
    }
}

trait FunctionParams: 'static + Send + Sync {
    /// Assumes all types are registered in the environment
    fn input_types(env: &Environment) -> Vec<Type>;

    fn type_ids() -> Vec<TypeId>;
}

struct SslCallableFnObj<F: SslCallable<Params, Out> + 'static + Send + Sync, Params: FunctionParams, Out: SslType> {
    f: F,
    params: PhantomData<(Params, Out)>
}

impl<Params: FunctionParams, Out: SslType, F: SslCallable<Params, Out> + 'static + Send + Sync> SslCallableFnObj<F, Params, Out> {
    fn new(f: F) -> Self {
        Self {
            f,
            params: Default::default(),
        }
    }
}

// SslCallableFn ---------------------------------------
pub trait SslCallableFn: 'static + Send + Sync {
    fn call(&self, inputs: &Vec<Lit>, env: &Environment) -> Lit;

    fn output(&self, env: &Environment) -> Type;
}

impl Debug for dyn SslCallableFn {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<Function>")
    }
}

trait IntoSslCallableFn<Params, Out>: 'static + Send + Sync {
    type Function: SslCallableFn;

    fn into_callable_function(self) -> Self::Function;
}

impl<Params: FunctionParams, Out: SslType, F: SslCallable<Params, Out> + 'static + Send + Sync> IntoSslCallableFn<Params, Out> for F {
    type Function = SslCallableFnObj<F, Params, Out>;

    fn into_callable_function(self) -> Self::Function {
        SslCallableFnObj {
            f: self,
            params: Default::default(),
        }
    }
}

impl<Params: FunctionParams, Out: SslType, F: SslCallable<Params, Out> + 'static + Send + Sync> SslCallableFn for SslCallableFnObj<F, Params, Out> {
    fn call(&self, inputs: &Vec<Lit>, env: &Environment) -> Lit {
        SslCallable::call(&self.f, inputs, env)
    }

    fn output(&self, env: &Environment) -> Type {
        Out::ssl_type(env)
    }
}


// SslUnaryOp ---------------------------------------
pub trait SslUnaryOp: 'static + Send + Sync {
    fn call(&self, input: Lit, env: &Environment) -> Lit;

    fn output(&self, env: &Environment) -> Type;
}

impl Debug for dyn SslUnaryOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<UnaryOp>")
    }
}

trait IntoSslUnaryOp<In1, Out>: 'static + Send + Sync {
    type Function: SslUnaryOp;

    fn into_callable_function(self) -> Self::Function;
}

impl<In1: SslType, Out: SslType, F: SslCallable<(In1,), Out> + 'static + Send + Sync> IntoSslUnaryOp<In1, Out> for F {
    type Function = SslCallableFnObj<F, (In1,), Out>;

    fn into_callable_function(self) -> Self::Function {
        SslCallableFnObj {
            f: self,
            params: Default::default(),
        }
    }
}

impl<P1: SslType, Out: SslType, F: SslCallable<(P1,), Out> + 'static + Send + Sync> SslUnaryOp for SslCallableFnObj<F, (P1,), Out> {
    fn call(&self, input: Lit, env: &Environment) -> Lit {
        self.f.call(&vec![input], env)
    }

    fn output(&self, env: &Environment) -> Type {
        Out::ssl_type(env)
    }
}


// SslBinaryOp ---------------------------------------
pub trait SslBinaryOp: 'static + Send + Sync {
    fn call(&self, inputs: (Lit, Lit), env: &Environment) -> Lit;

    fn output(&self, env: &Environment) -> Type;
}

impl Debug for dyn SslBinaryOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<BinaryOp>")
    }
}

trait IntoSslBinaryOp<In1, In2, Out>: 'static + Send + Sync {
    type Function: SslBinaryOp;

    fn into_callable_function(self) -> Self::Function;
}

impl<In1: SslType, In2: SslType, Out: SslType, F: SslCallable<(In1, In2), Out> + 'static + Send + Sync> IntoSslBinaryOp<In1, In2, Out> for F {
    type Function = SslCallableFnObj<F, (In1, In2), Out>;

    fn into_callable_function(self) -> Self::Function {
        SslCallableFnObj {
            f: self,
            params: Default::default(),
        }
    }
}

impl<P1: SslType, P2: SslType, Out: SslType, F: SslCallable<(P1, P2), Out> + 'static + Send + Sync> SslBinaryOp for SslCallableFnObj<F, (P1, P2), Out> {
    fn call(&self, inputs: (Lit, Lit), env: &Environment) -> Lit {
        self.f.call(&vec![inputs.0, inputs.1], env)
    }

    fn output(&self, env: &Environment) -> Type {
        Out::ssl_type(env)
    }
}

// Field -------------------------
pub trait SslField: 'static + Send + Sync {
    fn get(&self, var: Lit, env: &Environment) -> Lit;

    fn set(&self, var: Lit, value: Lit, env: &Environment) -> Lit;

    fn output(&self, env: &Environment) -> Type;
}

impl Debug for dyn SslField {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<Field>")
    }
}

trait IntoSslField<Var, Value>: 'static + Send + Sync {
    type Function: SslField;

    fn into_callable_function(self) -> Self::Function;
}

impl<Var: SslType, Value: SslType, F_get, F_set> IntoSslField<Var, Value> for (F_get, F_set) where
    F_get: SslCallable<(Var,), Value> + 'static + Send + Sync,
    F_set: SslCallable<(Var, Value), Var> + 'static + Send + Sync
{
    type Function = (SslCallableFnObj<F_get, (Var,), Value>, SslCallableFnObj<F_set, (Var, Value), Var>);

    fn into_callable_function(self) -> Self::Function {
        (SslCallableFnObj {
            f: self.0,
            params: Default::default(),
        }, SslCallableFnObj {
            f: self.1,
            params: Default::default(),
        }, )
    }
}

impl<Var: SslType, Value: SslType, F_get, F_set> SslField for (SslCallableFnObj<F_get, (Var,), Value>, SslCallableFnObj<F_set, (Var, Value), Var>) where
    F_get: SslCallable<(Var,), Value> + 'static + Send + Sync,
    F_set: SslCallable<(Var, Value), Var> + 'static + Send + Sync
{
    fn get(&self, var: Lit, env: &Environment) -> Lit {
        self.0.f.call(&vec![var], env)
    }

    fn set(&self, var: Lit, value: Lit, env: &Environment) -> Lit {
        self.1.f.call(&vec![var, value], env)
    }

    fn output(&self, env: &Environment) -> Type {
        Value::ssl_type(env)
    }
}

// Everything else --------------------------------------
trait SslCallable<Params: FunctionParams, OUT: SslType> {
    fn call(&self, inputs: &Vec<Lit>, env: &Environment) -> Lit;
}

pub trait SslType: 'static + Send + Sync + Clone + PartialEq + AnyValue + Sized {
    fn ssl_type(env: &Environment) -> Type {
        env.get_registered_type::<Self>()
            .unwrap_or_else(|| panic!("Type {} not registered", type_name::<Self>()))
    }

    fn lit(self, env: &Environment) -> Lit {
        Lit::Custom(Box::new(self), env.get_type_name(&TypeId::of::<Self>()).unwrap().clone())
    }

    fn from_lit(lit: Lit) -> Self {
        match lit {
            Lit::Custom(boxed, _) => boxed.as_any().downcast_ref::<Self>().expect("Failed to downcast Lit::Custom").clone(),
            _ => panic!("Expected Lit::Custom, got {:?}", lit)
        }
    }
}


// Impl SslType for types
impl SslType for bool {
    fn ssl_type(_env: &Environment) -> Type {
        Type::Bool
    }

    fn lit(self, _env: &Environment) -> Lit {
        Lit::Bool(self)
    }

    fn from_lit(lit: Lit) -> Self {
        match lit { Lit::Bool(v) => v, _ => panic!() }
    }
}

impl SslType for f32 {
    fn ssl_type(_env: &Environment) -> Type {
        Type::F32
    }

    fn lit(self, _env: &Environment) -> Lit {
        Lit::F32(self)
    }

    fn from_lit(lit: Lit) -> Self {
        match lit { Lit::F32(v) => v, _ => panic!() }
    }
}

impl SslType for Vec4 {
    fn ssl_type(_env: &Environment) -> Type {
        Type::Vec4
    }

    fn lit(self, _env: &Environment) -> Lit {
        Lit::Vec4(self)
    }

    fn from_lit(lit: Lit) -> Self {
        match lit { Lit::Vec4(v) => v, _ => panic!() }
    }
}

impl SslType for Mat4 {
    fn ssl_type(_env: &Environment) -> Type {
        Type::Mat4x4
    }

    fn lit(self, _env: &Environment) -> Lit {
        Lit::Mat4x4(self)
    }

    fn from_lit(lit: Lit) -> Self {
        match lit { Lit::Mat4x4(v) => v, _ => panic!() }
    }
}

macro_rules! define_impls {
    ($n:literal | $($param:ident),*) => {
        impl<$($param : SslType),*> FunctionParams for ($($param,)*) {
            fn input_types(env: &Environment) -> Vec<Type> {
                vec![$($param::ssl_type(env)),*]
            }

            fn type_ids() -> Vec<TypeId> {
                vec![$(TypeId::of::<$param>()),*]
            }
        }

        impl<$($param : SslType,)* OUT: SslType, FN: Fn($($param),*) -> OUT> SslCallable<($($param ,)*), OUT> for FN {
            fn call(&self, inputs: &Vec<Lit>, env: &Environment) -> Lit {
                debug_assert_eq!(inputs.len(), $n);
                let mut iter = inputs.iter();

                self($($param ::from_lit(iter.next().unwrap().clone())),*).lit(env)
            }
        }

        impl<$($param : SslType),*> SslType for ($($param,)*) {
            fn ssl_type(env: &Environment) -> Type {
                Type::Tuple(vec![$($param ::ssl_type(env)),*])
            }

            fn lit(self, env: &Environment) -> Lit {
                #[allow(non_snake_case)]
                let ($($param,)*) = self;

                Lit::Tuple(vec![$($param.lit(env)),*])
            }

            fn from_lit(lit: Lit) -> Self {
                match lit {
                    Lit::Tuple(v) => {
                        let mut iter = v.into_iter();

                        ($($param ::from_lit(iter.next().unwrap()),)*)
                    },
                    _ => panic!()
                }
            }
        }
    };
}

define_impls!(0 |);
define_impls!(1 | P1);
define_impls!(2 | P1, P2);
define_impls!(3 | P1, P2, P3);
define_impls!(4 | P1, P2, P3, P4);
define_impls!(5 | P1, P2, P3, P4, P5);
define_impls!(6 | P1, P2, P3, P4, P5, P6);
define_impls!(7 | P1, P2, P3, P4, P5, P6, P7);
define_impls!(8 | P1, P2, P3, P4, P5, P6, P7, P8);
define_impls!(9 | P1, P2, P3, P4, P5, P6, P7, P8, P9);
define_impls!(10 | P1, P2, P3, P4, P5, P6, P7, P8, P9, P10);
define_impls!(11 | P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11);
define_impls!(12 | P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12);