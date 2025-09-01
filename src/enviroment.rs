use std::any::{type_name, Any, TypeId};
use std::borrow::Borrow;
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::marker::PhantomData;
use std::ops::Deref;
use std::sync::{Arc};
use std::sync::atomic::{AtomicU32, AtomicUsize, Ordering};
use parking_lot::{MappedRwLockReadGuard, RwLock, RwLockReadGuard, RwLockWriteGuard};
use glam::{Mat4, Vec4};
use once_cell::sync::Lazy;
use crate::parser::{Binding, Function, Lit, Type};
use crate::any_value::{AnyValue, AsDynPartialEq};
use crate::interpreter::Eval;
use crate::prelude::{ToWgsl, ToWgslError, WgslDefinitions, WgslOutput};
use anyhow::{Context, Result as AnyResult};

#[cfg(feature="bevy_tracing")]
use bevy::log::info_span;

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
    env.register_binary_op(|v: Vec4, n: f32| v % n, "%".into()).unwrap();
    env.register_binary_op(|n: f32, v: Vec4| n % v, "%".into()).unwrap();

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

    // f32 and Vec4 math functions
    env.register_fn(|x: f32| x.abs(), "abs".into()).unwrap();
    env.register_fn(|v: Vec4| v.abs(), "abs".into()).unwrap();

    env.register_fn(|x: f32| x.acos(), "acos".into()).unwrap();
    env.register_fn(|v: Vec4| v.map(f32::acos), "acos".into()).unwrap();

    env.register_fn(|x: f32| x.acosh(), "acosh".into()).unwrap();
    env.register_fn(|v: Vec4| v.map(f32::acosh), "acosh".into()).unwrap();

    env.register_fn(|x: f32| x.asin(), "asin".into()).unwrap();
    env.register_fn(|v: Vec4| v.map(f32::asin), "asin".into()).unwrap();

    env.register_fn(|x: f32| x.asinh(), "asinh".into()).unwrap();
    env.register_fn(|v: Vec4| v.map(f32::asinh), "asinh".into()).unwrap();

    env.register_fn(|x: f32| x.atan(), "atan".into()).unwrap();
    env.register_fn(|v: Vec4| v.map(f32::atan), "atan".into()).unwrap();

    env.register_fn(|x: f32| x.atanh(), "atanh".into()).unwrap();
    env.register_fn(|v: Vec4| v.map(f32::atanh), "atanh".into()).unwrap();

    env.register_fn(|x: f32, y: f32| x.atan2(y), "atan2".into()).unwrap();
    env.register_fn(|x: Vec4, y: Vec4| Vec4::new(x.x.atan2(y.x), x.y.atan2(y.y), x.z.atan2(y.z), x.w.atan2(y.w)), "atan2".into()).unwrap();

    env.register_fn(|x: f32| x.ceil(), "ceil".into()).unwrap();
    env.register_fn(|v: Vec4| v.map(f32::ceil), "ceil".into()).unwrap();

    env.register_fn(|x: f32, min: f32, max: f32| x.clamp(min, max), "clamp".into()).unwrap();
    env.register_fn(|v: Vec4, min: Vec4, max: Vec4| v.clamp(min, max), "clamp".into()).unwrap();

    env.register_fn(|x: f32| x.cos(), "cos".into()).unwrap();
    env.register_fn(|v: Vec4| v.map(f32::cos), "cos".into()).unwrap();

    env.register_fn(|x: f32| x.cosh(), "cosh".into()).unwrap();
    env.register_fn(|v: Vec4| v.map(f32::cosh), "cosh".into()).unwrap();

    // env.register_fn(|x: f32| x.to_bits().leading_zeros() as f32, "countLeadingZeros".into()).unwrap();
    //
    // env.register_fn(|x: f32| x.to_bits().count_ones() as f32, "countOneBits".into()).unwrap();
    //
    // env.register_fn(|x: f32| x.to_bits().trailing_zeros() as f32, "countTrailingZeros".into()).unwrap();

    env.register_fn(|x: f32| x.to_degrees(), "degrees".into()).unwrap();
    env.register_fn(|v: Vec4| v.map(f32::to_degrees), "degrees".into()).unwrap();

    env.register_fn(|m: Mat4| m.determinant(), "determinant".into()).unwrap();

    env.register_fn(|a: Vec4, b: Vec4| a.distance(b), "distance".into()).unwrap();

    env.register_fn(|a: Vec4, b: Vec4| a.dot(b), "dot".into()).unwrap();

    env.register_fn(|x: f32| x.exp(), "exp".into()).unwrap();
    env.register_fn(|v: Vec4| v.map(f32::exp), "exp".into()).unwrap();

    env.register_fn(|x: f32| x.exp2(), "exp2".into()).unwrap();
    env.register_fn(|v: Vec4| v.map(f32::exp2), "exp2".into()).unwrap();

    env.register_fn(|n: f32| n.floor(), "floor".into()).unwrap();
    env.register_fn(|v: Vec4| v.map(f32::floor), "floor".into()).unwrap();

    env.register_fn(|a: f32, b: f32, c: f32| a.mul_add(b, c), "fma".into()).unwrap();
    env.register_fn(|v: Vec4, b: Vec4, c: Vec4| Vec4::new(v.x.mul_add(b.x, c.x), v.y.mul_add(b.y, c.y), v.z.mul_add(b.z, c.z), v.w.mul_add(b.w, c.w)), "fma".into()).unwrap();

    env.register_fn(|x: f32| x.fract(), "fract".into()).unwrap();
    env.register_fn(|v: Vec4| v.map(f32::fract), "fract".into()).unwrap();

    env.register_fn(|x: f32| x.sqrt().recip(), "inverseSqrt".into()).unwrap();
    env.register_fn(|v: Vec4| v.map(|x| x.sqrt().recip()), "inverseSqrt".into()).unwrap();

    env.register_fn(|v: Vec4| v.length(), "length".into()).unwrap();

    env.register_fn(|x: f32| x.ln(), "log".into()).unwrap();
    env.register_fn(|v: Vec4| v.map(f32::ln), "log".into()).unwrap();

    env.register_fn(|x: f32| x.log2(), "log2".into()).unwrap();
    env.register_fn(|v: Vec4| v.map(f32::log2), "log2".into()).unwrap();

    env.register_fn(|a: f32, b: f32| a.max(b), "max".into()).unwrap();
    env.register_fn(|a: Vec4, b: Vec4| a.max(b), "max".into()).unwrap();

    env.register_fn(|a: f32, b: f32| a.min(b), "min".into()).unwrap();
    env.register_fn(|a: Vec4, b: Vec4| a.min(b), "min".into()).unwrap();

    env.register_fn(|a: f32, b: f32, t: f32| a * (1.0 - t) + b * t, "mix".into()).unwrap();
    env.register_fn(|a: Vec4, b: Vec4, t: Vec4| a * (Vec4::ONE - t) + b * t, "mix".into()).unwrap();

    env.register_fn(|v: Vec4| v.normalize(), "normalize".into()).unwrap();

    env.register_fn(|a: f32, b: f32| a.powf(b), "pow".into()).unwrap();
    env.register_fn(|a: Vec4, b: Vec4| Vec4::new(a.x.powf(b.x), a.y.powf(b.y), a.z.powf(b.z), a.w.powf(b.w)), "pow".into()).unwrap();

    env.register_fn(|x: f32| x.to_radians(), "radians".into()).unwrap();
    env.register_fn(|v: Vec4| v.map(f32::to_radians), "radians".into()).unwrap();

    env.register_fn(|v: Vec4, n: Vec4| v.reflect(n), "reflect".into()).unwrap();

    env.register_fn(|i: Vec4, n: Vec4, eta: f32| i.refract(n, eta), "refract".into()).unwrap();

    env.register_fn(|x: f32| x.round(), "round".into()).unwrap();
    env.register_fn(|v: Vec4| v.map(f32::round), "round".into()).unwrap();

    env.register_fn(|x: f32| x.clamp(0.0, 1.0), "saturate".into()).unwrap();
    env.register_fn(|v: Vec4| v.clamp(Vec4::ZERO, Vec4::ONE), "saturate".into()).unwrap();

    env.register_fn(|x: f32| x.signum(), "sign".into()).unwrap();
    env.register_fn(|v: Vec4| v.map(f32::signum), "sign".into()).unwrap();

    env.register_fn(|x: f32| x.sin(), "sin".into()).unwrap();
    env.register_fn(|v: Vec4| v.map(f32::sin), "sin".into()).unwrap();

    env.register_fn(|x: f32| x.sinh(), "sinh".into()).unwrap();
    env.register_fn(|v: Vec4| v.map(f32::sinh), "sinh".into()).unwrap();

    env.register_fn(|edge0: f32, edge1: f32, x: f32| {
        let t = ((x - edge0) / (edge1 - edge0)).clamp(0.0, 1.0);
        t * t * (3.0 - 2.0 * t)
    }, "smoothstep".into()).unwrap();

    env.register_fn(|edge0: Vec4, edge1: Vec4, x: Vec4| {
        let t = ((x - edge0) / (edge1 - edge0)).clamp(Vec4::ZERO, Vec4::ONE);
        t * t * (Vec4::ONE * 3.0 - Vec4::ONE * 2.0 * t)
    }, "smoothstep".into()).unwrap();

    env.register_fn(|x: f32| x.sqrt(), "sqrt".into()).unwrap();
    env.register_fn(|v: Vec4| v.map(f32::sqrt), "sqrt".into()).unwrap();

    env.register_fn(|edge: f32, x: f32| if x <= edge { 0.0 } else { 1.0 }, "step".into()).unwrap();

    env.register_fn(|edge: Vec4, x: Vec4| Vec4::select(x.cmplt(edge), Vec4::splat(0.0), Vec4::splat(1.0)), "step".into()).unwrap();

    env.register_fn(|x: f32| x.tan(), "tan".into()).unwrap();
    env.register_fn(|v: Vec4| v.map(f32::tan), "tan".into()).unwrap();

    env.register_fn(|x: f32| x.tanh(), "tanh".into()).unwrap();
    env.register_fn(|v: Vec4| v.map(f32::tanh), "tanh".into()).unwrap();

    env.register_fn(|m: Mat4| m.transpose(), "transpose".into()).unwrap();

    env.register_fn(|x: f32| x.trunc(), "trunc".into()).unwrap();
    env.register_fn(|v: Vec4| v.map(f32::trunc), "trunc".into()).unwrap();


    env.register_field(|v: Vec4| v.x, |mut v: Vec4, n: f32| {v.x = n; v}, "x".into(), None).unwrap();
    env.register_field(|v: Vec4| v.y, |mut v: Vec4, n: f32| {v.y = n; v}, "y".into(), None).unwrap();
    env.register_field(|v: Vec4| v.z, |mut v: Vec4, n: f32| {v.z = n; v}, "z".into(), None).unwrap();
    env.register_field(|v: Vec4| v.w, |mut v: Vec4, n: f32| {v.w = n; v}, "w".into(), None).unwrap();

    env.register_const("X".into(), Vec4::X.lit());
    env.register_const("Y".into(), Vec4::Y.lit());
    env.register_const("Z".into(), Vec4::Z.lit());
    env.register_const("W".into(), Vec4::W.lit());
    env.register_const("ONES".into(), Vec4::ONE.lit());
    env.register_const("ZEROS".into(), Vec4::ZERO.lit());
    env.register_const("PI".into(), std::f32::consts::PI.lit());
    env.register_const("E".into(), std::f32::consts::E.lit());
    env.register_const("IDENTITY".into(), Mat4::IDENTITY.lit());
    env.register_const("BIG_NUMBER".into(), 1_000_000.0.lit());

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

    fn inner_mut(&self) -> RwLockWriteGuard<EnvironmentInner> {
        self.0.write()
    }

    pub fn id(&self) -> u32 {
        self.0.read().env_id
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

    pub fn get_wgsl_code(&self) -> AnyResult<WgslOutput> {
        let mut wgsl_code = String::new();
        let mut wgsl_defs = WgslDefinitions::new();

        for (name, (wgsl_name, lit)) in self.inner().constants.iter().chain(GLOBAL_ENV.inner().constants.iter()) {
            let name = wgsl_name.as_ref().unwrap_or(name);
            let ty = self.get_wgsl_name(&lit.get_type()).context("Expected lit to have valid wgsl type name")?;
            let lit_code = lit.to_wgsl_rec(&mut Vec::new(), 0, &mut wgsl_defs, self)?;

            wgsl_code.push_str(format!("const {name}: {ty} = {};\n", lit_code).as_str());
        }

        for ((name, inputs), env_func) in self.inner().functions.iter().chain(GLOBAL_ENV.inner().functions.iter()) {
            match env_func {
                EnvironmentFunction::RustImpl { .. } => {},
                EnvironmentFunction::RustWgsl { wgsl_impl, .. } => {
                    wgsl_code.push('\n');
                    wgsl_code.push_str(wgsl_impl.as_str());
                    wgsl_code.push('\n');
                }
                EnvironmentFunction::Ssl(func) => {
                    wgsl_code.push('\n');
                    wgsl_code.push_str(func.to_wgsl_rec(&mut Vec::new(), 0, &mut wgsl_defs, self)?.as_str());
                    wgsl_code.push('\n');
                }
                EnvironmentFunction::SignatureOnly(output) => {
                    wgsl_code.push_str(format!("\n// FnSignature {{ name: \"{name}\", inputs: \"{inputs:?}\", output: \"{output:?}\" }}\n").as_str());
                }
            }
        }

        Ok(WgslOutput { wgsl_code, definitions: wgsl_defs })
    }


    pub fn register_unary_op<In1: SslType, Out: SslType, FN: IntoSslCallableFn<(In1,), Out>>(
        &self,
        function: FN,
        sym: SslIdentifier
    ) -> Result<(), RegisterError> {
        let in_type = In1::ssl_type();

        self.inner_mut().unary_ops.insert(
            (sym.name, in_type),
            (sym.wgsl_name, Arc::new(function.into_callable_function()))
        );

        Ok(())
    }

    pub fn get_unary_op(&self, sym: impl AsRef<str>, in1_type: Type) -> Option<MappedRwLockReadGuard<(Option<String>, Arc<dyn SslCallableFn>)>> {
        self.get_item(&|inner| {
            inner.unary_ops.get(&(sym.as_ref().to_string(), in1_type.clone()))
        })
    }


    pub fn register_binary_op<In1: SslType, In2: SslType, Out: SslType, FN: IntoSslCallableFn<(In1, In2), Out>>(
        &self,
        function: FN,
        sym: SslIdentifier
    ) -> Result<(), RegisterError> {
        let in1_type = In1::ssl_type();
        let in2_type = In2::ssl_type();

        self.inner_mut().binary_ops.insert(
            (sym.name, in1_type, in2_type),
            (sym.wgsl_name, Arc::new(function.into_callable_function()))
        );

        Ok(())
    }

    pub fn get_binary_op(&self, sym: impl AsRef<str>, in1_type: Type, in2_type: Type) -> Option<MappedRwLockReadGuard<(Option<String>, Arc<dyn SslCallableFn>)>> {
        self.get_item(&|inner| {
            inner.binary_ops.get(&(sym.as_ref().to_string(), in1_type.clone(), in2_type.clone()))
        })
    }


    pub fn register_fn<Params: FunctionParams, Out: SslType, FN: IntoSslCallableFn<Params, Out>>(
        &self,
        function: FN,
        name: SslIdentifier
    ) -> Result<(), RegisterError> {
        let input_types = Params::input_types();

        self.inner_mut().functions.insert(
            (name.name, input_types),
            EnvironmentFunction::RustImpl {
                func: Arc::new(function.into_callable_function()),
                wgsl_name: name.wgsl_name
            }
        );

        Ok(())
    }

    pub fn register_fn_with_wgsl_code<Params: FunctionParams, Out: SslType, FN: IntoSslCallableFn<Params, Out>>(
        &self,
        function: FN,
        name: impl AsRef<str>,
        wgsl_code: String
    ) -> Result<(), RegisterError> {
        let input_types = Params::input_types();

        self.inner_mut().functions.insert(
            (name.as_ref().to_string(), input_types),
            EnvironmentFunction::RustWgsl {
                func: Arc::new(function.into_callable_function()),
                wgsl_impl: wgsl_code
            }
        );

        Ok(())
    }

    pub fn insert_ssl_fn(&self, function: Function) {
        self.inner_mut().functions.insert(
            (function.name.clone(), function.inputs.iter().map(|Binding(_, ty)| ty.clone()).collect()),
            EnvironmentFunction::Ssl(function)
        );
    }

    /// Inserts a signature, but these MUST be removed and cannot be gotten by get_fn
    pub(crate) fn insert_signature(&self, function: &Function) {
        self.inner_mut().functions.insert(
            (function.name.clone(), function.inputs.iter().map(|Binding(_, ty)| ty.clone()).collect()),
            EnvironmentFunction::SignatureOnly(function.output.clone())
        );
    }

    pub(crate) fn remove_signatures(&self) {
        self.inner_mut().functions
            .retain(|_, f| !matches!(f, EnvironmentFunction::SignatureOnly(_)));
    }

    pub fn get_env_fn(&self, sym: impl AsRef<str>, ins: Vec<Type>) -> Option<MappedRwLockReadGuard<EnvironmentFunction>> {
        self.get_item(&|inner| {
            inner.functions.get(&(sym.as_ref().to_string(), ins.clone()))
        })
    }


    pub fn register_const(&self, sym: SslIdentifier, lit: Lit) {
        self.inner_mut().constants.insert(sym.name, (sym.wgsl_name, lit));
    }

    pub fn get_env_const(&self, sym: impl AsRef<str>) -> Option<MappedRwLockReadGuard<(Option<String>, Lit)>> {
        self.get_item(&|inner| {
            inner.constants.get(&sym.as_ref().to_string())
        })
    }


    pub fn register_field<Var: SslType, Value: SslType, GETTER: IntoSslCallableFn<(Var,), Value>, SETTER: IntoSslCallableFn<(Var, Value), Var>>(
        &self,
        getter: GETTER,
        setter: SETTER,
        name: SslIdentifier,
        wgsl_index: Option<usize>
    ) -> Result<(), RegisterError> {
        let in_type = Var::ssl_type();

        self.inner_mut().field.insert(
            (name.name, in_type),
            (name.wgsl_name, wgsl_index, Arc::new(getter.into_callable_function()), Arc::new(setter.into_callable_function()))
        );

        Ok(())
    }

    pub fn get_field(&self, name: impl AsRef<str>, in1_type: Type) -> Option<MappedRwLockReadGuard<(Option<String>, Option<usize>, Arc<dyn SslCallableFn>, Arc<dyn SslCallableFn>)>> {
        self.get_item(&|inner| {
            inner.field.get(&(name.as_ref().to_string(), in1_type.clone()))
        })
    }


    pub fn register_type<T: Any>(&self, type_name: SslIdentifier) {
        self.inner_mut().types.push((type_name.name.clone(), TypeId::of::<T>(), type_name.wgsl_name))
    }

    pub fn type_id_exists(&self, id: &TypeId) -> bool {
        self.inner().types.iter()
            .any(|(_, other_id, _)| id == other_id)
    }

    pub fn get_type_id(&self, name: impl AsRef<str>) -> Option<TypeId> {
        self.inner().types.iter()
            .find(|(other_name, _, _)| name.as_ref() == other_name.as_str())
            .map(|(_, type_id, _)| type_id.clone())
    }

    pub fn get_wgsl_name(&self, ty: &Type) -> Option<String> {
        match ty {
            Type::F32 => Some("f32".to_string()),
            Type::Bool => Some("bool".to_string()),
            // If you add angle-brackets, you'll need to re-write how tuple names are made
            Type::Vec4 => Some("vec4<f32>".to_string()),
            Type::Mat4x4 => Some("mat4x4<f32>".to_string()),
            Type::Unit => Some("unit".to_string()),
            Type::Custom(type_id) => {
                let inner = self.inner();
                let Some((name, _, wgsl_name)) = inner.types.iter().find(|(_, id, _)| id == type_id) else {
                    return None
                };

                Some(wgsl_name.as_ref().unwrap_or(&name).clone())
            }
            Type::Tuple(tuple_type) => {
                Some(format!(
                    "tuple_{}_0",
                    tuple_type.iter()
                        .map(|ty| self.get_wgsl_name(ty).map(
                            |s| s.replace(">", "").replace("<", "")
                        ))
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
    env_id: u32,

    /// Map from registered unary ops defined by (symbol, input) to (wgsl_symbol, fn)
    unary_ops: HashMap<(String, Type), (Option<String>, Arc<dyn SslCallableFn>)>,
    /// Map from registered binary ops defined by (symbol, input1, input2) to (wgsl_symbol, fn)
    binary_ops: HashMap<(String, Type, Type), (Option<String>, Arc<dyn SslCallableFn>)>,
    /// Map from registered functions defined by (name, inputs) to env_function
    functions: HashMap<(String, Vec<Type>), EnvironmentFunction>,
    /// Map from registered constants to (wgsl_name, const_index)
    constants: HashMap<String, (Option<String>, Lit)>,
    /// Map from (field_name, type) to (wgsl_name, wgsl_index_opt, get_field, set_field)
    field: HashMap<(String, Type), (Option<String>, Option<usize>, Arc<dyn SslCallableFn>, Arc<dyn SslCallableFn>)>,
    /// List of registered types, (name, type_id, wgsl_name)
    types: Vec<(String, TypeId, Option<String>)>,
}

impl EnvironmentInner {
    fn new() -> Self {
        static ENV_ID_COUNTER: AtomicU32 = AtomicU32::new(0);

        EnvironmentInner {
            env_id: ENV_ID_COUNTER.fetch_add(1, Ordering::SeqCst),
            unary_ops: Default::default(),
            binary_ops: Default::default(),
            functions: Default::default(),
            constants: Default::default(),
            types: vec![],
            field: Default::default(),
        }
    }
}

#[derive(Debug)]
pub enum EnvironmentFunction {
    RustImpl {
        func: Arc<dyn SslCallableFn>,
        wgsl_name: Option<String>
    },
    RustWgsl {
        func: Arc<dyn SslCallableFn>,
        wgsl_impl: String
    },
    Ssl(Function),
    SignatureOnly(Type)
}

impl EnvironmentFunction {
    pub fn call(&self, inputs: Vec<Lit>, env: &Environment) -> Lit {
        #[cfg(feature="bevy_tracing")]
        let my_span = info_span!("EnvironmentFunction::call").entered();

        match self {
            EnvironmentFunction::RustImpl { func, .. } => func.call(inputs),
            EnvironmentFunction::RustWgsl { func, .. } => func.call(inputs),
            EnvironmentFunction::Ssl(ssl_function) => {
                let mut scope = inputs.iter().zip(&ssl_function.inputs)
                    .map(|(lit, Binding(name, _))| (name, lit)).collect::<Vec<_>>().into();

                ssl_function.body.eval(&mut scope, env).unwrap()
            }
            EnvironmentFunction::SignatureOnly(_) => panic!("Cannot call a fn signature")
        }
    }

    pub fn output(&self, env: &Environment) -> Type {
        match self {
            EnvironmentFunction::RustImpl { func, .. } => func.output(),
            EnvironmentFunction::RustWgsl { func, .. } => func.output(),
            EnvironmentFunction::Ssl(func) => func.output.clone(),
            EnvironmentFunction::SignatureOnly(output) => output.clone()
        }
    }

    pub fn wgsl_name(&self, ssl_name: &String) -> String {
        match self {
            EnvironmentFunction::RustImpl { wgsl_name, .. } => {
                wgsl_name.as_ref().unwrap_or(ssl_name).clone()
            }
            _ => ssl_name.clone()
        }
    }
}

trait FunctionParams: 'static + Send + Sync {
    /// Assumes all types are registered in the environment
    fn input_types() -> Vec<Type>;

    fn type_ids() -> Vec<TypeId>;
}

pub(crate) struct SslCallableFnObj<F: SslCallable<Params, Out> + 'static + Send + Sync, Params: FunctionParams, Out: SslType> {
    pub(crate) f: F,
    pub(crate) params: PhantomData<(Params, Out)>
}

impl<Params: FunctionParams, Out: SslType, F: SslCallable<Params, Out> + 'static + Send + Sync> SslCallableFnObj<F, Params, Out> {
    pub(crate) fn new(f: F) -> Self {
        Self {
            f,
            params: PhantomData::default(),
        }
    }
}

// SslCallableFn ---------------------------------------
macro_rules! implCallSignatureN {
    ($($call:ident | ($($param:ident ),*)),*) => {
        $(
            fn $call (&self $(, $param: Lit)*) -> Lit;
        )*
    };
}

pub trait SslCallableFn: 'static + Send + Sync {
    fn call(&self, inputs: Vec<Lit>) -> Lit;

    implCallSignatureN!(
        call0 | (),
        call1 | (p1),
        call2 | (p1, p2),
        call3 | (p1, p2, p3),
        call4 | (p1, p2, p3, p4),
        call5 | (p1, p2, p3, p4, p5),
        call6 | (p1, p2, p3, p4, p5, p6),
        call7 | (p1, p2, p3, p4, p5, p6, p7),
        call8 | (p1, p2, p3, p4, p5, p6, p7, p8),
        call9 | (p1, p2, p3, p4, p5, p6, p7, p8, p9),
        call10 | (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10),
        call11 | (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11)
    );

    fn output(&self) -> Type;
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

macro_rules! implCallN {
    ($($call:ident | ($($param:ident ),*)),*) => {
        $(
            fn $call (&self $(, $param: Lit)*) -> Lit {
                SslCallable::$call(&self.f $(, $param)*)
            }
        )*
    };
}

impl<Params: FunctionParams, Out: SslType, F: SslCallable<Params, Out> + 'static + Send + Sync> SslCallableFn for SslCallableFnObj<F, Params, Out> {
    fn call(&self, inputs: Vec<Lit>) -> Lit {
        SslCallable::call(&self.f, inputs)
    }

    implCallN!(
        call0 | (),
        call1 | (p1),
        call2 | (p1, p2),
        call3 | (p1, p2, p3),
        call4 | (p1, p2, p3, p4),
        call5 | (p1, p2, p3, p4, p5),
        call6 | (p1, p2, p3, p4, p5, p6),
        call7 | (p1, p2, p3, p4, p5, p6, p7),
        call8 | (p1, p2, p3, p4, p5, p6, p7, p8),
        call9 | (p1, p2, p3, p4, p5, p6, p7, p8, p9),
        call10 | (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10),
        call11 | (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11)
    );

    fn output(&self) -> Type {
        Out::ssl_type()
    }
}


// SslUnaryOp ---------------------------------------
pub trait SslUnaryOp: 'static + Send + Sync {
    fn call(&self, input: Lit) -> Lit;

    fn output(&self) -> Type;
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
    fn call(&self, input: Lit) -> Lit {
        self.f.call(vec![input])
    }

    fn output(&self) -> Type {
        Out::ssl_type()
    }
}


// SslBinaryOp ---------------------------------------
pub trait SslBinaryOp: 'static + Send + Sync {
    fn call(&self, inputs: (Lit, Lit)) -> Lit;

    fn output(&self) -> Type;
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
    fn call(&self, inputs: (Lit, Lit)) -> Lit {
        self.f.call(vec![inputs.0, inputs.1])
    }

    fn output(&self) -> Type {
        Out::ssl_type()
    }
}

// Field -------------------------
pub trait SslField: 'static + Send + Sync {
    fn get(&self, var: Lit) -> Lit;

    fn set(&self, var: Lit, value: Lit) -> Lit;

    fn output(&self) -> Type;
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
    fn get(&self, var: Lit) -> Lit {
        self.0.f.call(vec![var])
    }

    fn set(&self, var: Lit, value: Lit) -> Lit {
        self.1.f.call(vec![var, value])
    }

    fn output(&self) -> Type {
        Value::ssl_type()
    }
}

// Everything else --------------------------------------
macro_rules! implDefaultCallN {
    ($($call:ident | ($($param:ident ),*)),*) => {
        $(
            fn $call (&self $(, $param: Lit)*) -> Lit {
                panic!("{} is not implemented for this impl of SslCallable", stringify!($call));

                Lit::Unit
            }
        )*
    };
}

trait SslCallable<Params: FunctionParams, OUT: SslType> {
    fn call(&self, inputs: Vec<Lit>) -> Lit;

    implDefaultCallN!(
        call0 | (),
        call1 | (_p1),
        call2 | (_p1, _p2),
        call3 | (_p1, _p2, _p3),
        call4 | (_p1, _p2, _p3, _p4),
        call5 | (_p1, _p2, _p3, _p4, _p5),
        call6 | (_p1, _p2, _p3, _p4, _p5, _p6),
        call7 | (_p1, _p2, _p3, _p4, _p5, _p6, _p7),
        call8 | (_p1, _p2, _p3, _p4, _p5, _p6, _p7, _p8),
        call9 | (_p1, _p2, _p3, _p4, _p5, _p6, _p7, _p8, _p9),
        call10 | (_p1, _p2, _p3, _p4, _p5, _p6, _p7, _p8, _p9, _p10),
        call11 | (_p1, _p2, _p3, _p4, _p5, _p6, _p7, _p8, _p9, _p10, _p11)
    );
}

pub trait SslType: 'static + Send + Sync + Clone + PartialEq + AnyValue + Sized {
    fn ssl_type() -> Type {
        Type::Custom(TypeId::of::<Self>())
    }

    fn lit(self) -> Lit {
        Lit::Custom(Box::new(self), TypeId::of::<Self>())
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
    fn ssl_type() -> Type {
        Type::Bool
    }

    fn lit(self) -> Lit {
        Lit::Bool(self)
    }

    fn from_lit(lit: Lit) -> Self {
        match lit { Lit::Bool(v) => v, _ => panic!() }
    }
}

impl SslType for f32 {
    fn ssl_type() -> Type {
        Type::F32
    }

    fn lit(self) -> Lit {
        Lit::F32(self)
    }

    fn from_lit(lit: Lit) -> Self {
        match lit { Lit::F32(v) => v, _ => panic!() }
    }
}

impl SslType for Vec4 {
    fn ssl_type() -> Type {
        Type::Vec4
    }

    fn lit(self) -> Lit {
        Lit::Vec4(self)
    }

    fn from_lit(lit: Lit) -> Self {
        match lit { Lit::Vec4(v) => v, _ => panic!() }
    }
}

impl SslType for Mat4 {
    fn ssl_type() -> Type {
        Type::Mat4x4
    }

    fn lit(self) -> Lit {
        Lit::Mat4x4(self)
    }

    fn from_lit(lit: Lit) -> Self {
        match lit { Lit::Mat4x4(v) => v, _ => panic!() }
    }
}

macro_rules! define_impls {
    ($call:ident | $n:literal | $(($param:ident, $index:literal)),*) => {
        impl<$($param : SslType),*> FunctionParams for ($($param,)*) {
            fn input_types() -> Vec<Type> {
                vec![$($param::ssl_type()),*]
            }

            fn type_ids() -> Vec<TypeId> {
                vec![$(TypeId::of::<$param>()),*]
            }
        }

        impl<$($param : SslType,)* OUT: SslType, FN: Fn($($param),*) -> OUT> SslCallable<($($param ,)*), OUT> for FN {
            fn call(&self, inputs: Vec<Lit>) -> Lit {
                #[cfg(feature="bevy_tracing")]
                let my_span = info_span!("SslCallable call", tys = stringify!(Fn($($param),*) -> OUT)).entered();

                debug_assert_eq!(inputs.len(), $n);
                let mut iter = inputs.into_iter();

                self($($param ::from_lit(iter.next().unwrap())),*).lit()
            }

            fn $call (&self $(, $param: Lit )*) -> Lit {
                #[cfg(feature="bevy_tracing")]
                let my_span = info_span!("SslCallable", call_ty = stringify!( $call ), tys = stringify!(Fn($($param),*) -> OUT)).entered();

                self($($param::from_lit($param)),*).lit()
            }
        }

        impl<$($param : SslType),*> SslType for ($($param,)*) {
            fn ssl_type() -> Type {
                Type::Tuple(vec![$($param ::ssl_type()),*])
            }

            fn lit(self) -> Lit {
                #[allow(non_snake_case)]
                let ($($param,)*) = self;

                Lit::Tuple(vec![$($param.lit()),*])
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

define_impls!(call0 | 0 |);
define_impls!(call1 | 1 | (P1, 0));
define_impls!(call2 | 2 | (P1, 0), (P2, 1));
define_impls!(call3 | 3 | (P1, 0), (P2, 1), (P3, 2));
define_impls!(call4 | 4 | (P1, 0), (P2, 1), (P3, 2), (P4, 3));
define_impls!(call5 | 5 | (P1, 0), (P2, 1), (P3, 2), (P4, 3), (P5, 4));
define_impls!(call6 | 6 | (P1, 0), (P2, 1), (P3, 2), (P4, 3), (P5, 4), (P6, 5));
define_impls!(call7 | 7 | (P1, 0), (P2, 1), (P3, 2), (P4, 3), (P5, 4), (P6, 5), (P7, 6));
define_impls!(call8 | 8 | (P1, 0), (P2, 1), (P3, 2), (P4, 3), (P5, 4), (P6, 5), (P7, 6), (P8, 7));
define_impls!(call9 | 9 | (P1, 0), (P2, 1), (P3, 2), (P4, 3), (P5, 4), (P6, 5), (P7, 6), (P8, 7), (P9, 8));
define_impls!(call10 | 10 | (P1, 0), (P2, 1), (P3, 2), (P4, 3), (P5, 4), (P6, 5), (P7, 6), (P8, 7), (P9, 8), (P10, 9));
define_impls!(call11 | 11 | (P1, 0), (P2, 1), (P3, 2), (P4, 3), (P5, 4), (P6, 5), (P7, 6), (P8, 7), (P9, 8), (P10, 9), (P11, 10));