use std::collections::HashMap;
use std::fmt;
use std::fmt::{Display, Formatter};
use crate::parser::{Binding, Bound, Document, Expr, ExprInner, Function, Instance, KeyVal, Lit, LvalueDeclare, Method, MethodKey, Stmt, Type, Value as ParseValue};
use crate::structure::{Field, Structure, TryFromRonValue};
use crate::tree_walk::{TreeNodeMut, WalkTreeMut};
use anyhow::{anyhow, Context, Result as AnyResult};
use ron::Value;
use crate::ast_operations::{AlphaConvert, AssignTypes, IdentScope};
use crate::enviroment::Environment;
use crate::interpreter::Eval;
use crate::scope::Scope;

#[cfg(feature="bevy_tracing")]
use bevy::log::info_span;
use crate::compiler::{CompiledEnv, CompiledFn};

const FIELD_PREFIX: &'static str = "_f__";

impl Structure {
    fn get_instance_structure(&self, document: &Document) -> AnyResult<Structure> {
        let class = document.get_class(&self.name)
            .with_context(|| format!("Couldn't find class {}", self.name))?;

        let instance = class.instance.as_ref()
            .with_context(|| format!("Couldn't find instance on class {}", self.name))?;

        Ok(self.create_instance(instance))
    }

    fn create_instance(&self, instance: &Instance) -> Structure {
        let fields = instance.key_vals.iter().map(|KeyVal { key, value }| {
            (key.clone(), match value {
                ParseValue::Expr(Expr(ExprInner::Var(var_name, _), _)) => {
                    if let Some(Field::Structure(structure)) = self.get_field(var_name) {
                        Field::Structure(structure.clone())
                    } else {
                        Field::Expr(ExprInner::Var(var_name.clone(), Type::Auto).into())
                    }
                },
                ParseValue::Expr(expr) => Field::Expr(expr.clone()),
                ParseValue::Instance(sub_instance) => Field::Structure(Box::new(self.create_instance(sub_instance)))
            })
        }).collect();

        Structure { name: instance.name.clone(), fields }
    }

    fn assemble_fields(self, document: &Document) -> AnyResult<Vec<(String, Expr, Type)>> {
        let mut fields = Vec::new();
        let class = document.get_class(&self.name)
            .with_context(|| format!("Couldn't find class {}", self.name))?;

        if class.fields.len() > self.fields.len() {
            return Err(anyhow!(
                "Could not find missing fields [{}] in class {}",
                class.fields.iter().filter_map(|Binding(name, _)| {
                    if self.fields.iter().all(|(field_name, _)| field_name != name) {
                        Some(format!("{name:?}"))
                    } else {
                        None
                    }
                }).collect::<Vec<_>>().join(", "),
                class.name
            ));
        }

        for (field_name, field) in self.fields {
            match field {
                Field::Expr(expr) => {
                    let Some(Binding(_, ty)) = class.get_field(&field_name) else {
                        return Err(anyhow!("Couldn't find field {} in class {}", field_name, &self.name));
                    };
                    fields.push((field_name, expr, ty.clone()))
                },
                Field::Structure(structure) => {
                    let structure_fields = structure.assemble_fields(document)?;

                    fields.append(&mut structure_fields.into_iter().map(
                        |(other_field_name, expr, ty)| (format!("{FIELD_PREFIX}{}__{}", field_name, other_field_name), expr, ty)
                    ).collect::<Vec<_>>())
                }
            }
        }

        Ok(fields)
    }

    fn assemble_methods(&self, document: &Document, env: &Environment) -> AnyResult<Vec<Method>> {
        let mut methods = Vec::new();
        let class = document.get_class(&self.name)
            .with_context(|| format!("Couldn't find class {}", self.name))?;

        for method in class.methods.iter() {
            methods.push(self.assemble_method(
                document,
                env,
                MethodKey::new(method.implementation.as_ref(), &method.name)
            ).context(format!("Could not assemble method: {}", method.name))?);
        }

        Ok(methods)
    }

    /// Assembles a method to inline trait fn calls and perform some small optimizations
    fn assemble_method(&self, document: &Document, env: &Environment, method_key: MethodKey) -> AnyResult<Method> {
        let mut method = self.assemble_method_rec(document, method_key.clone(), "".to_string(), env)
            .with_context(|| format!("Could not do assemble_method_rec on {method_key}"))?;;

        let mut type_scope = (&method.inputs).into();
        method.body.assign_types_rec(&mut type_scope, env).with_context(|| format!("Could not assign types on {method}"))?;
        method.body.alpha_convert(&mut IdentScope::new());
        method.body.inline_blocks(env).with_context(|| format!("Could not inline blocks on {method}"))?;;
        method.body.cull_single_use_vars();
        method.body.cull_noops();

        Ok(method)
    }

    fn assemble_method_rec(&self, document: &Document, method_key: MethodKey, id: String, env: &Environment) -> AnyResult<Method> {
        let mut method = document
            .get_method(&self.name, &method_key)
            .with_context(|| format!("Could not find method: {} in {}", method_key, &self.name))?
            .clone();

        let bounds = std::mem::replace(&mut method.bounds, Vec::new());

        // Traverses in search of __fieldName__.__methodName__(...) to replace with the method
        method.body.walk_tree_mut(&mut |node| {
            let TreeNodeMut::Expr(Expr(expr, span)) = node else { return Ok::<(), anyhow::Error>(()) };
            match expr {
                ExprInner::Var(var, _) => {
                    if self.get_field(&var).is_some() || var.starts_with(FIELD_PREFIX) {
                        *var = format!("{id}{var}");
                    }

                    Ok(())
                },
                ExprInner::Dot(field_name, method_name, args) => {
                    let bound = &bounds.iter()
                        .find(|Bound { name, .. }| field_name == name)
                        .with_context(|| format!("{}Couldn't find used method {method_name} in method bounds {bounds:?} used in {method_key} in {}", span.context(), &self.name))?;

                    let interface = bound.get_interface_with_method(document, &method_name)
                        .with_context(|| format!("{}Couldn't get interface with method {method_name:?} using bound {bound} used in {method_key} in {}", span.context(), &self.name))?;

                    let Some(Field::Structure(structure)) = self.get_field(&field_name) else {
                        return Err(anyhow!("{} Couldn't find field {field_name} used in {method_key} in {} -OR- The field is not a structure", span.context(), &self.name))
                    };

                    let Method { mut body, inputs, .. } = structure.assemble_method_rec(
                        document, MethodKey::new(Some(&interface.name), &method_name), format!("{FIELD_PREFIX}{}__", field_name), env
                    )?;

                    for (arg, binding) in args.iter().zip(inputs).rev() {
                        body.0.insert(0, Stmt::Declare(LvalueDeclare::Binding(binding), arg.clone()));
                    }

                    *expr = ExprInner::Block(Box::new(body)).into();

                    Ok(())
                },
                _ => Ok(())
            }
        })?;

        let class = document.get_class(&self.name)
            .with_context(|| format!("Couldn't find class {}", self.name))?;

        let mut ty_scope = class.fields.iter()
            .map(|Binding(name, ty)| (format!("{id}{name}"), ty.clone())).collect::<Vec<_>>().into();

        method.assign_types_rec(&mut ty_scope, env)?;

        Ok(method)
    }
}

#[derive(Debug, Clone)]
pub struct AssembledStructure {
    pub(crate) fields: Vec<(String, Expr, Type)>,
    pub evaluated_scope: Scope<Lit>,
    pub(crate) assembled_fns: Vec<Function>,
    pub(crate) compiled_fns: HashMap<String, CompiledFn>,
    pub env: Environment
}

impl AssembledStructure {
    pub fn empty() -> Self {
        Self {
            env: Environment::new(),
            fields: Vec::new(),
            evaluated_scope: Scope::new(),
            compiled_fns: HashMap::new(),
            assembled_fns: Vec::new()
        }
    }

    pub fn new_from_ron(document: &Document, ron: impl AsRef<str>, env: &Environment, compiled_env: &CompiledEnv) -> AnyResult<Self> {
        Self::new(document, Structure::from_ron_string(ron.as_ref(), env)?, env, compiled_env)
    }

    pub fn new_from_value(document: &Document, ron: Value, env: &Environment, compiled_env: &CompiledEnv) -> AnyResult<Self> {
        Self::new(document, Structure::try_from_ron_value(ron, env)?, env, compiled_env)
    }

    pub fn new(document: &Document, mut structure: Structure, env: &Environment, compiled_env: &CompiledEnv) -> AnyResult<Self> {
        let mut fields = Vec::new();

        if let Ok(instance_structure) = structure.get_instance_structure(document) {
            fields = structure.assemble_fields(document)?;
            structure = instance_structure;
        }

        let mut methods = structure.assemble_methods(document, env)?;
        fields.append(&mut structure.assemble_fields(document)?);

        let assembled_fns = methods.into_iter()
            .map(|Method { name, implementation, bounds, inputs, output, body }| {
                Function {
                    name,
                    inputs: inputs.into_iter().chain(fields.iter().map(|(name, _, ty)| Binding(name.clone(), ty.clone()))).collect(),
                    output,
                    body,
                }
            }).collect::<Vec<_>>();

        let compiled_fns = assembled_fns.iter()
            .try_fold::<_, _, AnyResult<_>>(HashMap::new(), |mut map, assembled_fns| {
                map.insert(assembled_fns.name.clone(), assembled_fns.compile(&env, compiled_env)?);

                Ok(map)
            })?;

        Ok(Self {
            env: env.clone(),
            fields,
            evaluated_scope: Scope::new(),
            assembled_fns,
            compiled_fns,
        })
    }

    pub fn evaluate_fields(&mut self, mut scope: Scope<Lit>) -> AnyResult<()> {
        #[cfg(feature="bevy_tracing")]
        let my_span = info_span!("evaluate_fields").entered();
        self.evaluated_scope = Scope::new();

        for (name, expr, _ty) in self.fields.iter() {
            let lit = expr.eval(&mut scope, &self.env)?;
            self.evaluated_scope.push(name.clone(), lit.clone());
            scope.push(name.clone(), lit);
        }

        Ok(())
    }

    pub(crate) fn get_assembled_fn(&self, name: impl AsRef<str>) -> Option<&Function> {
        #[cfg(feature="bevy_tracing")]
        let my_span = info_span!("get_method").entered();
        self.assembled_fns.iter().find(|assembled_fn| assembled_fn.name.as_str() == name.as_ref())
    }

    // TODO: Move away from using fn names, should be able to just say "does xyz implement trait?"
    pub fn has_assembled_fns<'a>(&self, methods: impl IntoIterator<Item=&'a str>) -> bool {
        methods.into_iter().all(|method_name| {
            self.get_assembled_fn(method_name).is_some()
        })
    }

    pub fn to_assembled_string(&self) -> String {
        format!("AssembledStructure {{{}\n}}", self.evaluated_scope.iter().map(|(name, lit)| format!("\n    {name}: {lit},")).collect::<String>())
    }
}

impl Display for AssembledStructure {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "Fields: {{ ")?;

        for (name, expr, _ty) in self.fields.iter() {
            write!(f, "{}: {}, ", name, expr)?;
        }

        //evaluated scope

        write!(f, " }} Evaluated Scope: {:?}", self.evaluated_scope)?;

        write!(f, " Methods: {{ ")?;

        for method in self.assembled_fns.iter() {
            write!(f, "{}, ", method)?;
        }

        write!(f, " }} Compiled Fns: {{ ")?;

        for (name, compiled_fn) in self.compiled_fns.iter() {
            write!(f, "{name}: {}, ", compiled_fn)?;
        }

        write!(f, " }}")
    }
}

#[cfg(test)]
mod tests {
    use crate::assemble::AssembledStructure;
    use crate::compiler::CompiledEnv;
    use crate::parser::{Lit, MethodKey};
    use crate::scope::Scope;
    use crate::test_helpers;
    use crate::test_helpers::{better_prettify, prettify_string};

    #[test]
    fn try_assemble_method() {
        let (env, document, structure) = test_helpers::get_test_stuff(0, 1);
        println!("Document: {document}\nStructure: {structure}");

        let assembled_method = structure.assemble_method(
            &document,
            &env,
            MethodKey::new(Some("Proj"), "proj")
        ).unwrap();

        let compiled_env = CompiledEnv::new();
        let assembled_structure = AssembledStructure::new(&document, structure, &env, &compiled_env).unwrap();

        println!("Assembled Method: {}\nAssembled Structure: {}", prettify_string(format!("{assembled_method}")), prettify_string(format!("{assembled_structure}")));
    }

    #[test]
    fn try_assemble_instance() {
        let (env, document, structure) = test_helpers::get_test_stuff(0, 2);
        println!("Document: {document}\nStructure: {structure}");

        let compiled_env = CompiledEnv::new();
        let mut assembled_structure = AssembledStructure::new(&document, structure, &env, &compiled_env).unwrap();

        // println!("Assembled not pretty: {assembled_structure}\nAssembled Structure: {}", better_prettify(format!("{assembled_structure}")));
        //
        assembled_structure.evaluate_fields(Scope::from_vars([("abc".to_string(), Lit::F32(1.0))])).unwrap();

        println!("Assembled Structure after fields are evaluated: {}", prettify_string(format!("{assembled_structure}")));
    }
}