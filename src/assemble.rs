use std::collections::HashMap;
use std::fmt;
use std::fmt::{Display, Formatter};
use crate::parser::{Binding, Bound, Document, Expr, ExprInner, Instance, KeyVal, Lit, LvalueDeclare, Method, MethodKey, Stmt, Type, Value as ParseValue};
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
const INSTANCE_PREFIX: &'static str = "_i__";

impl Structure {
    // fn into_expanded_instance_structures(mut self, mut id: String, document: &Document) -> AnyResult<(Structure, Vec<(String, Expr, Type)>)> {
    //     let mut fields = Vec::new();
    //     let mut class = document.get_class(&self.name)
    //         .with_context(|| format!("Couldn't find class {}", self.name))?;
    //
    //     while let Some(instance) = self.get_instance(document) {
    //         id = format!("{id}{INSTANCE_PREFIX}{}__", self.name);
    //
    //         for (name, field) in self.fields.iter() {
    //             if let Field::Expr(expr) = field {
    //                 let Some(Binding(_, ty)) = class.get_field(name) else {
    //                     return Err(anyhow!("Couldn't find field {} in class {}", name, &self.name));
    //                 };
    //                 fields.push((format!("{id}{name}"), expr.clone(), ty.clone()))
    //             }
    //         }
    //
    //         let map = self.fields.iter().fold(HashMap::new(), |mut map, (name, _)| {
    //             map.insert(name.clone(), format!("{id}{name}"));
    //             map
    //         });
    //
    //         self = self.create_instance(instance, &map);
    //         class = document.get_class(&self.name)
    //             .with_context(|| format!("Couldn't find class {}", self.name))?;
    //     }
    //
    //     for (name, field) in self.fields.iter_mut() {
    //         match field {
    //             Field::Structure(structure) => {
    //                 let owned_structure = std::mem::replace(structure, Box::new(Structure::empty()));
    //                 let (expanded_structure, mut expanded_fields) = owned_structure.into_expanded_instance_structures(
    //                     format!("{id}{FIELD_PREFIX}{}__", name),
    //                     document
    //                 )?;
    //
    //                 *structure = Box::new(expanded_structure);
    //                 fields.append(&mut expanded_fields);
    //             }
    //             Field::Expr(expr) => {
    //                 let Some(Binding(_, ty)) = class.get_field(&name) else {
    //                     return Err(anyhow!("Couldn't find field {} in class {}", name, &self.name));
    //                 };
    //
    //                 *name = format!("{id}{name}");
    //                 fields.push((name.clone(), expr.clone(), ty.clone()))
    //             }
    //         }
    //     }
    //
    //     Ok((self, fields))
    // }

    fn get_instance<'a>(&self, document: &'a Document) -> Option<&'a Instance> {
        let class = document.get_class(&self.name)?;

        class.instance.as_ref()
    }

    fn as_instance(&self, id: &String, document: &Document) -> Option<Structure> {
        let instance = self.get_instance(document)?;

        Some(self.create_instance(instance, id))
    }

    fn create_instance(&self, instance: &Instance, id: &String) -> Structure {
        let fields = instance.key_vals.iter().map(|KeyVal { key, value }| {
            (key.clone(), match value {
                ParseValue::Expr(Expr(ExprInner::Var(var_name, _), _)) => {
                    match self.get_field(var_name) {
                        Some(Field::Structure(structure)) => {
                            Field::Structure(structure.clone())
                        }
                        Some(Field::Expr(_)) => {
                            Field::Expr(ExprInner::Var(format!("{id}{var_name}"), Type::Auto).into())
                        }
                        None => {
                            Field::Expr(ExprInner::Var(var_name.clone(), Type::Auto).into())
                        }
                    }
                },
                ParseValue::Expr(expr) => {
                    let mut expr = expr.clone();
                    let mut ident_scope = self.fields.iter().fold(IdentScope::new(), |mut scope, (name, field)| {
                        if let Field::Expr(_) = field {
                            scope.push(name.clone(), format!("{id}{name}"))
                        }

                        scope
                    });

                    expr.alpha_convert(&mut ident_scope);

                    Field::Expr(expr)
                },
                ParseValue::Instance(sub_instance) => Field::Structure(Box::new(self.create_instance(sub_instance, id)))
            })
        }).collect();

        Structure { name: instance.name.clone(), fields }
    }

    fn assemble_fields(&self, id: String, document: &Document) -> AnyResult<Vec<(String, Expr, Type)>> {
        let mut fields = Vec::new();

        let class = document.get_class(&self.name)
            .with_context(|| format!("Couldn't find class {}", self.name))?;

        if let Some(instance_structure) = self.as_instance(&id, document) {
            for (name, field) in self.fields.iter() {
                if let Field::Expr(expr) = field {
                    let Some(Binding(_, ty)) = class.get_field(&name) else {
                        return Err(anyhow!("Couldn't find field {} in class {}", name, &self.name));
                    };

                    fields.push((format!("{id}{name}"), expr.clone(), ty.clone()));
                }
            }

            fields.append(&mut instance_structure.assemble_fields(format!("{id}{INSTANCE_PREFIX}{}__", self.name), document)?);

            return Ok(fields);
        }

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

        for (field_name, field) in self.fields.iter() {
            match field {
                Field::Expr(expr) => {
                    let Some(Binding(_, ty)) = class.get_field(&field_name) else {
                        return Err(anyhow!("Couldn't find field {} in class {}", field_name, &self.name));
                    };
                    fields.push((format!("{id}{field_name}"), expr.clone(), ty.clone()))
                },
                Field::Structure(structure) => {
                    let mut structure_fields = structure.assemble_fields(format!("{id}{FIELD_PREFIX}{}__", field_name), document)?;

                    fields.append(&mut structure_fields)
                }
            }
        }

        Ok(fields)
    }

    fn assemble_methods(&self, document: &Document, env: &Environment) -> AnyResult<Vec<Method>> {
        let mut methods = Vec::new();
        let class = if let Some(instance_structure) = self.as_instance(&String::new(), document) {
            document.get_class(&instance_structure.name)
                .with_context(|| format!("Couldn't find class {}", instance_structure.name))?
        } else {
            document.get_class(&self.name)
                .with_context(|| format!("Couldn't find class {}", self.name))?
        };

        for method in class.methods.iter() {
            match self.assemble_method(
                document,
                env,
                MethodKey::new(method.implementation.as_ref(), &method.name)
            ) {
                Ok(method) => { methods.push(method); },
                Err(err) => { eprintln!("{err:?}"); }
            }
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
        // println!("({}) START", self.name);
        if let Some(instance_structure) = self.as_instance(&id, document) {
            let result = instance_structure.assemble_method_rec(
                document,
                method_key,
                format!("{id}{INSTANCE_PREFIX}{}__", self.name),
                env
            );

            // println!("({}) END", self.name);
            return result;
        }

        let mut method = document
            .get_method(&self.name, &method_key)
            .with_context(|| format!("Could not find method: {} in {}", method_key, &self.name))?
            .clone();

        // println!("method: {method}");

        let bounds = std::mem::replace(&mut method.bounds, Vec::new());

        // Traverses in search of __fieldName__.__methodName__(...) to replace with the method
        method.body.walk_tree_mut(&mut |node| {
            let TreeNodeMut::Expr(Expr(expr, span)) = node else { return Ok::<(), anyhow::Error>(()) };
            match expr {
                ExprInner::Var(var, _) => {
                    // println!("({}) id: {id}, var: {var}, field names: [{}]", self.name, self.fields.iter().filter_map(|(name, field)| {
                    //     match field { Field::Structure(_) => None, Field::Expr(_) => Some(name.clone()) }
                    // }).collect::<Vec<_>>().join(", "));

                    if self.get_field(&var).is_some() {
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
                        document, MethodKey::new(Some(&interface.name), &method_name), format!("{id}{FIELD_PREFIX}{}__", field_name), env
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

        // println!("({}) ty_scope: {ty_scope:?}", self.name);
        method.assign_types_rec(&mut ty_scope, env)?;
        // println!("({}) END", self.name);
        Ok(method)
    }
}

#[derive(Debug, Clone)]
pub struct AssembledStructure {
    pub(crate) fields: Vec<(String, Expr, Type)>,
    pub evaluated_scope: Scope<Lit>,
    pub(crate) methods: Vec<Method>,
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
            methods: Vec::new()
        }
    }

    pub fn new_from_ron(document: &Document, ron: impl AsRef<str>, env: &Environment, compiled_env: &CompiledEnv) -> AnyResult<Self> {
        Self::new(document, Structure::from_ron_string(ron.as_ref(), env)?, env, compiled_env)
    }

    pub fn new_from_value(document: &Document, ron: Value, env: &Environment, compiled_env: &CompiledEnv) -> AnyResult<Self> {
        Self::new(document, Structure::try_from_ron_value(ron, env)?, env, compiled_env)
    }

    pub fn new(document: &Document, mut structure: Structure, env: &Environment, compiled_env: &CompiledEnv) -> AnyResult<Self> {
        let fields = structure.assemble_fields(String::new(), document)?;

        // println!("structure: {structure}\n\nfields: {:#?}", fields.iter().map(|(name, expr, ty)| format!("({name}: {expr}  ({ty}))")).collect::<Vec<_>>());

        let methods = structure.assemble_methods(document, env)?;

        let field_names = fields.iter().map(|(field_name, _, ty)| {
            Ok((field_name.clone(), ty.clone()))
        }).collect::<AnyResult<Vec<_>>>()?;

        // println!("{}\n\nfield_names: {field_names:?}", methods[0]);

        let compiled_fns = methods.iter()
            .try_fold::<_, _, AnyResult<_>>(HashMap::new(), |mut map, method| {
                map.insert(method.name.clone(), method.compile(&env, compiled_env, &field_names)?);

                Ok(map)
            })?;

        Ok(Self {
            env: env.clone(),
            fields,
            evaluated_scope: Scope::new(),
            methods,
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

    pub fn get_method(&self, name: impl AsRef<str>) -> Option<&Method> {
        #[cfg(feature="bevy_tracing")]
        let my_span = info_span!("get_method").entered();
        self.methods.iter().find(|method| method.name.as_str() == name.as_ref())
    }

    // TODO: Move away from using fn names, should be able to just say "does xyz implement trait?"
    pub fn has_methods<'a>(&self, methods: impl IntoIterator<Item=&'a str>) -> bool {
        methods.into_iter().all(|method_name| {
            self.get_method(method_name).is_some()
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

        for method in self.methods.iter() {
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
    fn try_assemble() {
        let (env, document, structure) = test_helpers::get_test_stuff(1, 4);
        println!("Document: {document}\nStructure: {structure}");

        let compiled_env = CompiledEnv::new();
        let mut assembled_structure = AssembledStructure::new(&document, structure, &env, &compiled_env).unwrap();

        assembled_structure.evaluate_fields(Scope::new()).unwrap();

        println!("eval'd scope: {:?}", assembled_structure.evaluated_scope);
        use glam::Vec4;

        let v: Vec4 = assembled_structure.eval_method("abc", Vec4::new(1.0, 1.0, 1.0, 1.0)).unwrap();

        println!("output: {v}");

        println!("Assembled Structure: {}", prettify_string(format!("{assembled_structure}")));
    }

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