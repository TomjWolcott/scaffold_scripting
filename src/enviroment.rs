use std::any::{type_name, Any, TypeId};
use std::borrow::Borrow;
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::marker::PhantomData;
use std::sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard};
use crate::parser::{Function, FunctionSignature, Lit, Type};
use anyhow::{anyhow, Context, Result as AnyResult};
use glam::Vec4;
use crate::enviroment::RegisterError::TypeNotRegistered;

#[test]
fn test() {
    let mut env = SslEnvironment::new();

    env.register_type::<String>("String".to_string());
    env.register_fn(|a: f32| {2.0}, "hello", None);

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

#[derive(Debug)]
pub enum RegisterError {
    TypeNotRegistered(usize, String)
}

#[derive(Debug)]
/// Holds all registered data for Ssl: unary ops, binary ops, functions, consts, types, fields
pub struct SslEnvironment(Arc<RwLock<SslEnvironmentInner>>);

impl SslEnvironment {
    pub fn new() -> Self {
        SslEnvironment(Arc::new(RwLock::new(SslEnvironmentInner::new())))
    }

    fn inner(&self) -> RwLockReadGuard<SslEnvironmentInner> {
        self.0.read().unwrap()
    }

    fn inner_mut(&mut self) -> RwLockWriteGuard<SslEnvironmentInner> {
        self.0.write().unwrap()
    }

    pub fn register_type<T: Any>(&mut self, type_name: SslIdentifier) {
        self.inner_mut().types.push((type_name.name.clone(), TypeId::of::<T>(), type_name.wgsl_name))
    }

    pub fn registered_custom_type<T: Any>(&self) -> Option<Type> {
        let type_id = TypeId::of::<T>();

        self.inner().types.iter()
            .find(|(_, other_type_id, _)| *type_id == other_type_id)
            .map(|(name, _, _)| Type::Custom(name.clone()))
    }

    pub fn get_wgsl_name(&self, ty: &Type) -> Option<String> {
        match ty {
            Type::F32 => Some("f32".to_string()),
            Type::Bool => Some("bool".to_string()),
            Type::Vec4 => Some("vec4".to_string()),
            Type::Mat4x4 => Some("mat4x4".to_string()),
            Type::Unit => Some("unit".to_string()),
            Type::Custom(type_name) => {
                let Some((_, ident)) = self.inner().types.iter().find(|(_, ident)| ident.name() == type_name) else {
                    return None
                };

                Some(ident.wgsl_name().clone())
            }
            _ => None
        }
    }

    pub fn get_type_name(&self, type_id: &TypeId) -> Option<&String> {
        self.inner().types.get(type_id)
    }

    pub fn register_fn<Params: FunctionParams, Out: SslType, FN: SslCallable<Params, Out> + 'static>(
        &mut self,
        function: FN,
        name: impl AsRef<str>,
        wgsl_name: Option<String>
    ) -> Result<(), RegisterError> {
        let input_types = Params::type_ids().iter().enumerate().map(|(i, type_id)| {
            self.get_type_name(type_id)
                .ok_or(TypeNotRegistered(i, type_name::<Params>().to_string()))
                .map(|s| s.clone())
        }).collect::<Result<Vec<_>, RegisterError>>()?;

        let fn_signature = FunctionSignature::new(name.as_ref().to_string(), input_types);

        self.inner_mut().functions.insert(fn_signature, SslFunction::RustFn {
            func: Box::new(SslCallableFnObj::new(function)),
            name: SslIdentifier::new(name, wgsl_name)
        });
    }
}

#[derive(Debug)]
pub struct SslEnvironmentInner {
    /// Map from registered unary ops defined by (symbol, input) to (wgsl_symbol, fn)
    unary_ops: HashMap<(String, Type), (Option<String>, Box<dyn SslUnaryOp>)>,
    /// Map from registered binary ops defined by (symbol, input1, input2) to (wgsl_symbol, fn)
    binary_ops: HashMap<(String, Type, Type), (Option<String>, Box<dyn SslBinaryOp>)>,
    /// Map from registered functions defined by (name, inputs) to (wgsl_name, fn)
    functions: HashMap<(String, Vec<Type>), (Option<String>, Box<dyn SslCallableFn>)>,
    /// Map from registered constants to (wgsl_name, value)
    consts: HashMap<String, (Option<String>, Lit)>,
    /// List of registered types, (name, type_id, wgsl_name)
    types: Vec<(String, TypeId, Option<String>)>,
    /// Map from registered fields to (wgsl_name, wgsl_index_opt, get_field)
    field: HashMap<String, (Option<String>, Option<usize>)>
}

impl SslEnvironmentInner {
    fn new() -> Self {
        SslEnvironmentInner {
            functions: HashMap::new(),
            consts: HashMap::new(),
            types: Vec::new(),
        }
    }
}

pub enum SslFunction {
    RustFn {
        func: Box<dyn SslCallableFn>,
        name: Arc<SslIdentifier>
    },
    SslFn(Function)
}

impl Debug for SslFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            SslFunction::RustFn { .. } => write!(f, "RustFn {{ .. }}"),
            SslFunction::SslFn(_) => write!(f, "SslFn(_)"),
        }
    }
}


trait FunctionParams: 'static {
    fn input_types() -> Vec<Type>;

    fn type_ids() -> Vec<TypeId>;
}

trait SslCallableFn: 'static {
    fn signature(&self, name: String) -> FunctionSignature;

    fn call(&self, inputs: &Vec<Lit>, env: &SslEnvironment) -> Lit;

    fn output(&self, env: &SslEnvironment) -> Type;
}

struct SslCallableFnObj<F: SslCallable<Params, Out> + 'static, Params: FunctionParams, Out: SslType> {
    f: F,
    input_types: Vec<Type>,
    params: PhantomData<(Params, Out)>
}

impl<Params: FunctionParams, Out: SslType, F: SslCallable<Params, Out> + 'static> SslCallableFnObj<F, Params, Out> {
    fn new(f: F) -> Self {
        Self {
            f,
            input_types: Params::input_types(),
            params: Default::default(),
        }
    }
}

impl<Params: FunctionParams, Out: SslType, F: SslCallable<Params, Out> + 'static> SslCallableFn for SslCallableFnObj<F, Params, Out> {
    fn signature(&self, name: String) -> FunctionSignature {
        FunctionSignature::new(name, self.input_types.clone())
    }

    fn call(&self, inputs: &Vec<Lit>, env: &SslEnvironment) -> Lit {
        SslCallable::call(&self.f, inputs, env)
    }

    fn output(&self, env: &SslEnvironment) -> Type {
        Out::ssl_type(env)
    }
}

trait SslUnaryOp: 'static {
    fn call(&self, input: Lit, env: &SslEnvironment) -> Lit;

    fn output(&self, env: &SslEnvironment) -> Type;
}

impl<P1: SslType, Out: SslType, F: SslCallable<(P1,), Out> + 'static> SslUnaryOp for SslCallableFnObj<F, (P1,), Out> {
    fn call(&self, input: Lit, env: &SslEnvironment) -> Lit {
        self.f(P1::from_lit(input)).lit(env)
    }

    fn output(&self, env: &SslEnvironment) -> Type {
        Out::ssl_type(env)
    }
}

trait SslBinaryOp: 'static {
    fn call(&self, inputs: (Lit, Lit), env: &SslEnvironment) -> Lit;

    fn output(&self, env: &SslEnvironment) -> Type;
}

impl<P1: SslType, P2: SslType, Out: SslType, F: SslCallable<(P1, P2), Out> + 'static> SslUnaryOp for SslCallableFnObj<F, (P1, P2), Out> {
    fn call(&self, inputs: (Lit, Lit), env: &SslEnvironment) -> Lit {
        self.f(P1::from_lit(inputs.0), P2::from_lit(inputs.1)).lit(env)
    }

    fn output(&self, env: &SslEnvironment) -> Type {
        Out::ssl_type(env)
    }
}

trait SslCallable<Params: FunctionParams, OUT: SslType> {
    fn signature(&self, name: String) -> FunctionSignature {
        FunctionSignature::new(name, Params::input_types())
    }

    fn call(&self, inputs: &Vec<Lit>, env: &SslEnvironment) -> Lit;
}

pub trait SslType: 'static + Any + Sized {
    fn ssl_type(env: &SslEnvironment) -> Type {
        env.get_type_of::<Self>()
            .map(|s| Type::Custom(s.clone()))
            .unwrap_or_else(|| panic!("Type {} not registered", type_name::<Self>()))
    }

    fn lit(self, env: &SslEnvironment) -> Lit {
        Lit::Custom(Box::new(self), env.get_type_of::<Self>().unwrap().clone())
    }

    fn from_lit(lit: Lit) -> Self {
        match lit {
            Lit::Custom(boxed, _) => boxed.downcast::<Self>().expect("Failed to downcast Lit::Custom"),
            _ => panic!("Expected Lit::Custom, got {:?}", lit)
        }
    }
}

impl SslType for bool {
    fn ssl_type(_env: &SslEnvironment) -> Type {
        Type::Bool
    }

    fn lit(self, _env: &SslEnvironment) -> Lit {
        Lit::Bool(self)
    }

    fn from_lit(lit: Lit) -> Self {
        match lit { Lit::Bool(v) => v, _ => panic!() }
    }
}

impl SslType for f32 {
    fn ssl_type(_env: &SslEnvironment) -> Type {
        Type::F32
    }

    fn lit(self, _env: &SslEnvironment) -> Lit {
        Lit::F32(self)
    }

    fn from_lit(lit: Lit) -> Self {
        match lit { Lit::F32(v) => v, _ => panic!() }
    }
}

macro_rules! define_impls {
    ($n:literal | $($param:ident),*) => {
        impl<$($param : SslType),*> FunctionParams for ($($param,)*) {
            fn input_types() -> Vec<Type> {
                vec![$($param::ssl_type()),*]
            }

            fn type_ids() -> Vec<TypeId> {
                vec![$(TypeId::of::<$param>()),*]
            }
        }

        impl<$($param : SslType,)* OUT: SslType, FN: Fn($($param),*) -> OUT> SslCallable<($($param ,)*), OUT> for FN {
            fn call(&self, inputs: &Vec<Lit>, env: &SslEnvironment) -> Lit {
                debug_assert_eq!(inputs.len(), $n);
                let mut iter = inputs.iter();

                self($($param ::from_lit(iter.next().unwrap().clone())),*).lit(env)
            }
        }

        impl<$($param : SslType),*> SslType for ($($param,)*) {
            fn ssl_type(_env: &SslEnvironment) -> Type {
                Type::Tuple(vec![$($param ::ssl_type()),*])
            }

            fn lit(self, _env: &SslEnvironment) -> Lit {
                let ($($param,)*) = self;

                Lit::Tuple(vec![$($param),*])
            }

            fn from_lit(lit: Lit) -> Self {
                match lit {
                    Lit::Tuple(v) => {
                        let [$($param),*] = v[0..$n] else { panic!(); };

                        ($($param,)*)
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
define_impls!(13 | P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13);
define_impls!(14 | P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14);
define_impls!(15 | P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15);