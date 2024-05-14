use std::fmt;
use std::fmt::{Display, Formatter};
use crate::parser::{Bound, Document, Expr, Instance, KeyVal, Method, MethodKey, Stmt, Value as ParseValue};
use crate::structure::{Field, Structure, TryFromRonValue};
use crate::tree_walk::{TreeNodeMut, WalkTreeMut};

use anyhow::{anyhow, Context, Result as AnyResult};
use ron::Value;
use crate::ast_operations::{AlphaConvert, IdentScope};
use crate::interpreter::{Eval, Scope};


impl Structure {
    fn get_instance_structure(&self, document: &Document) -> AnyResult<Structure> {
        let class = document.get_class(&self.name).context("Couldn't find class")?;

        let instance = class.instance.as_ref().context("No instance found")?;

        Ok(self.create_instance(instance))
    }

    fn create_instance(&self, instance: &Instance) -> Structure {
        let fields = instance.key_vals.iter().map(|KeyVal { key, value }| {
            (key.clone(), match value {
                ParseValue::Expr(Expr::Var(var_name)) => {
                    if let Some(Field::Structure(structure)) = self.get_field(var_name) {
                        Field::Structure(structure.clone())
                    } else {
                        Field::Expr(Expr::Var(var_name.clone()))
                    }
                },
                ParseValue::Expr(expr) => Field::Expr(expr.clone()),
                ParseValue::Instance(sub_instance) => Field::Structure(Box::new(self.create_instance(sub_instance)))
            })
        }).collect();

        Structure { name: instance.name.clone(), fields }
    }

    fn assemble_fields(self) -> AnyResult<Vec<(String, Expr)>> {
        let mut fields = Vec::new();

        for (field_name, field) in self.fields {
            match field {
                Field::Expr(expr) => fields.push((field_name, expr)),
                Field::Structure(structure) => {
                    let structure_fields = structure.assemble_fields()?;

                    fields.append(&mut structure_fields.into_iter().map(
                        |(other_field_name, expr)| (format!("__{}__{}", field_name, other_field_name), expr)
                    ).collect::<Vec<_>>())
                }
            }
        }

        Ok(fields)
    }

    fn assemble_methods(&self, document: &Document) -> AnyResult<Vec<Method>> {
        let mut methods = Vec::new();
        let class = document.get_class(&self.name).context("Couldn't find class")?;

        for method in class.methods.iter() {
            methods.push(self.assemble_method(
                document,
                MethodKey::new(method.implementation.as_ref(), &method.name)
            )?);
        }

        Ok(methods)
    }

    /// Assembles a method to inline trait fn calls and perform some small optimizations
    fn assemble_method(&self, document: &Document, method_key: MethodKey) -> AnyResult<Method> {
        self.assemble_method_rec(document, method_key, "".to_string()).map(|mut method| {
            method.body.alpha_convert(&mut IdentScope::new());
            method.body.inline_blocks();
            method.body.cull_single_use_vars();
            method.body.cull_noops();

            method
        })
    }

    fn assemble_method_rec(&self, document: &Document, method_key: MethodKey, id: String) -> AnyResult<Method> {
        let mut method = document
            .get_method(&self.name, &method_key)
            .context("Could not find method")?
            .clone();

        let bounds = std::mem::replace(&mut method.bounds, Vec::new());

        // Traverses in search of __fieldName__.__methodName__(...) to replace with the method
        method.body.walk_tree_mut(&mut |node| {
            let TreeNodeMut::Expr(expr) = node else { return Ok(()) };
            match expr {
                Expr::Var(var) => {
                    if self.get_field(&var).is_some() || var.starts_with("__") {
                        *var = format!("{id}{var}");
                    }

                    Ok(())
                },
                Expr::Dot(field_name, method_name, args) => {
                    let bound = &bounds.iter().find(|Bound { name, .. }| field_name == name).context("Couldn't find bound")?;
                    let interface = bound.get_interface_with_method(document, &method_name).context("Couldn't get interface")?;
                    let Some(Field::Structure(structure)) = self.get_field(&field_name) else {
                        return Err(anyhow!("Couldn't get field"));
                    };

                    let Method { mut body, inputs, .. } = structure.assemble_method_rec(
                        document, MethodKey::new(Some(&interface.name), &method_name), format!("__{}__", field_name)
                    )?;

                    for (arg, binding) in args.iter().zip(inputs).rev() {
                        body.0.insert(0, Stmt::Declare(binding, arg.clone()));
                    }

                    *expr = Expr::Block(Box::new(body));

                    Ok(())
                },
                _ => Ok(())
            }
        })?;

        Ok(method)
    }
}

#[derive(Debug, Clone)]
pub struct AssembledStructure {
    pub(crate) fields: Vec<(String, Expr)>,
    pub evaluated_scope: Scope,
    pub(crate) methods: Vec<Method>
}

impl AssembledStructure {
    pub fn empty() -> Self {
        Self {
            fields: Vec::new(),
            evaluated_scope: Scope::new(),
            methods: Vec::new()
        }
    }

    pub fn new_from_ron(document: &Document, ron: impl AsRef<str>) -> AnyResult<Self> {
        Self::new(document, Structure::from_ron_string(ron.as_ref())?)
    }

    pub fn new_from_value(document: &Document, ron: Value) -> AnyResult<Self> {
        Self::new(document, Structure::try_from_ron_value(ron)?)
    }

    pub fn new(document: &Document, mut structure: Structure) -> AnyResult<Self> {
        let mut fields = Vec::new();

        if let Ok(instance_structure) = structure.get_instance_structure(document) {
            fields = structure.assemble_fields()?;
            structure = instance_structure;
        }

        let methods = structure.assemble_methods(document)?;
        fields.append(&mut structure.assemble_fields()?);

        Ok(Self {
            fields,
            evaluated_scope: Scope::new(),
            methods
        })
    }

    pub fn evaluate_fields(&mut self, mut scope: Scope) -> AnyResult<()> {
        self.evaluated_scope = Scope::new();

        for (name, expr) in self.fields.iter() {
            self.evaluated_scope.push(name.clone(), expr.eval(&mut scope)?);
        }

        Ok(())
    }

    pub fn get_method(&self, name: impl AsRef<str>) -> Option<&Method> {
        self.methods.iter().find(|method| method.name.as_str() == name.as_ref())
    }

    // TODO: Move away from using fn names, should be able to just say "does xyz implement trait?"
    pub fn has_methods<'a>(&self, methods: impl IntoIterator<Item=&'a str>) -> bool {
        methods.into_iter().all(|method_name| {
            self.get_method(method_name).is_some()
        })
    }
}

impl Display for AssembledStructure {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "Fields: {{ ")?;

        for (name, expr) in self.fields.iter() {
            write!(f, "{}: {}, ", name, expr)?;
        }

        //evaluated scope

        write!(f, " }} Evaluated Scope: {}", self.evaluated_scope)?;

        write!(f, " Methods: {{ ")?;

        for method in self.methods.iter() {
            write!(f, "{}, ", method)?;
        }

        write!(f, " }}")
    }
}

#[cfg(test)]
mod tests {
    use crate::assemble::AssembledStructure;
    use crate::parser::MethodKey;
    use crate::test_helpers;
    use crate::test_helpers::{better_prettify, prettify_string};

    #[test]
    fn try_assemble_method() {
        let (document, structure) = test_helpers::get_test_stuff(0, 1);
        println!("Document: {document}\nStructure: {structure}");

        let assembled_method = structure.assemble_method(
            &document,
            MethodKey::new(Some("Proj"), "proj")
        ).unwrap();

        let assembled_structure = AssembledStructure::new(&document, structure).unwrap();

        println!("Assembled Method: {}\nAssembled Structure: {}", prettify_string(format!("{assembled_method}")), prettify_string(format!("{assembled_structure}")));
    }

    #[test]
    fn try_assemble_instance() {
        let (document, structure) = test_helpers::get_test_stuff(0, 2);
        println!("Document: {document}\nStructure: {structure}");

        let assembled_structure = AssembledStructure::new(&document, structure).unwrap();

        println!("Assembled not pretty: {assembled_structure}\nAssembled Structure: {}", better_prettify(format!("{assembled_structure}")));
    }
}