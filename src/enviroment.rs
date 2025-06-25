use std::any::{type_name, Any, TypeId};
use std::borrow::Borrow;
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::marker::PhantomData;
use std::sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard};
use crate::parser::{Function, Lit, Type};
use anyhow::{anyhow, Context, Result as AnyResult};
use glam::Vec4;
use crate::any_value::{AnyValue, AsDynPartialEq};
use crate::enviroment::RegisterError::TypeNotRegistered;

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
    TypeNotRegistered(usize, String)
}

#[derive(Debug, Clone)]
/// Holds all registered data for Ssl: unary ops, binary ops, functions, consts, types, fields
pub struct Environment(Arc<RwLock<EnvironmentInner>>);

impl Environment {
    pub fn new() -> Self {
        Environment(Arc::new(RwLock::new(EnvironmentInner::new())))
    }

    fn inner(&self) -> RwLockReadGuard<EnvironmentInner> {
        self.0.read().unwrap()
    }

    fn inner_mut(&mut self) -> RwLockWriteGuard<EnvironmentInner> {
        self.0.write().unwrap()
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
            _ => None
        }
    }

    pub fn get_type_name(&self, type_id: &TypeId) -> Option<String> {
        self.inner().types.iter()
            .find(|(_, other_type_id, _)| other_type_id == type_id)
            .map(|(name, _, _)| name.clone())
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
}

#[derive(Debug)]
struct EnvironmentInner {
    // /// Map from registered unary ops defined by (symbol, input) to (wgsl_symbol, fn)
    // unary_ops: HashMap<(String, Type), (Option<String>, Box<dyn SslUnaryOp>)>,
    // /// Map from registered binary ops defined by (symbol, input1, input2) to (wgsl_symbol, fn)
    // binary_ops: HashMap<(String, Type, Type), (Option<String>, Box<dyn SslBinaryOp>)>,
    /// Map from registered functions defined by (name, inputs) to (wgsl_name, fn)
    functions: HashMap<(String, Vec<Type>), (Option<String>, Box<dyn SslCallableFn>)>,
    /// Map from registered constants to (wgsl_name, value)
    consts: HashMap<String, (Option<String>, Lit)>,
    /// List of registered types, (name, type_id, wgsl_name)
    types: Vec<(String, TypeId, Option<String>)>,
    // /// Map from registered fields to (wgsl_name, wgsl_index_opt, get_field)
    // field: HashMap<String, (Option<String>, Option<usize>)>
}

impl EnvironmentInner {
    fn new() -> Self {
        EnvironmentInner {
            // unary_ops: Default::default(),
            // binary_ops: Default::default(),
            functions: Default::default(),
            consts: Default::default(),
            types: vec![],
            // field: Default::default(),
        }
    }
}

trait FunctionParams: 'static {
    /// Assumes all types are registered in the environment
    fn input_types(env: &Environment) -> Vec<Type>;

    fn type_ids() -> Vec<TypeId>;
}

trait SslCallableFn: 'static {
    fn call(&self, inputs: &Vec<Lit>, env: &Environment) -> Lit;

    fn output(&self, env: &Environment) -> Type;
}

impl Debug for dyn SslCallableFn {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<Function>")
    }
}

trait IntoSslCallableFn<Params, Out>: 'static {
    type Function: SslCallableFn;

    fn into_callable_function(self) -> Self::Function;
}

impl<Params: FunctionParams, Out: SslType, F: SslCallable<Params, Out> + 'static> IntoSslCallableFn<Params, Out> for F {
    type Function = SslCallableFnObj<F, Params, Out>;

    fn into_callable_function(self) -> Self::Function {
        SslCallableFnObj {
            f: self,
            params: Default::default(),
        }
    }
}

struct SslCallableFnObj<F: SslCallable<Params, Out> + 'static, Params: FunctionParams, Out: SslType> {
    f: F,
    params: PhantomData<(Params, Out)>
}

impl<Params: FunctionParams, Out: SslType, F: SslCallable<Params, Out> + 'static> SslCallableFnObj<F, Params, Out> {
    fn new(f: F) -> Self {
        Self {
            f,
            params: Default::default(),
        }
    }
}

impl<Params: FunctionParams, Out: SslType, F: SslCallable<Params, Out> + 'static> SslCallableFn for SslCallableFnObj<F, Params, Out> {
    fn call(&self, inputs: &Vec<Lit>, env: &Environment) -> Lit {
        SslCallable::call(&self.f, inputs, env)
    }

    fn output(&self, env: &Environment) -> Type {
        Out::ssl_type(env)
    }
}

trait SslUnaryOp: 'static {
    fn call(&self, input: Lit, env: &Environment) -> Lit;

    fn output(&self, env: &Environment) -> Type;
}

impl<P1: SslType, Out: SslType, F: SslCallable<(P1,), Out> + 'static> SslUnaryOp for SslCallableFnObj<F, (P1,), Out> {
    fn call(&self, input: Lit, env: &Environment) -> Lit {
        self.f.call(&vec![input], env)
    }

    fn output(&self, env: &Environment) -> Type {
        Out::ssl_type(env)
    }
}

trait SslBinaryOp: 'static {
    fn call(&self, inputs: (Lit, Lit), env: &Environment) -> Lit;

    fn output(&self, env: &Environment) -> Type;
}

impl<P1: SslType, P2: SslType, Out: SslType, F: SslCallable<(P1, P2), Out> + 'static> SslBinaryOp for SslCallableFnObj<F, (P1, P2), Out> {
    fn call(&self, inputs: (Lit, Lit), env: &Environment) -> Lit {
        self.f.call(&vec![inputs.0, inputs.1], env)
    }

    fn output(&self, env: &Environment) -> Type {
        Out::ssl_type(env)
    }
}

trait SslCallable<Params: FunctionParams, OUT: SslType> {
    fn call(&self, inputs: &Vec<Lit>, env: &Environment) -> Lit;
}

pub trait SslType: 'static + Clone + PartialEq + AnyValue + Sized {
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