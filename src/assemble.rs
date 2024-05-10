use std::fmt;
use std::fmt::{Display, Formatter};
use crate::parser::{Bound, Document, Expr, Lit, Method, MethodKey, parse_document, prettify_string, Stmt};
use crate::structure::{Field, Structure};
use crate::tree_walk::{TreeNodeMut, WalkTreeMut};

use anyhow::{anyhow, Context, Result as AnyResult};
use crate::ast_operations::{AlphaConvert, IdentScope};


impl Structure {
    pub fn convert_into_instance(&mut self, document: &Document) -> AnyResult<()> {
        let class = document.get_class(&self.name)?;

        let Some(instance) = &class.instance else { return Ok(()) };



        Ok(())
    }

    pub fn assemble_fields(&self, dynamic_collector: &mut impl FnMut(&String, &Field)) -> AnyResult<Vec<(String, Field)>> {
        let mut fields = Vec::new();

        for (field_name, field) in self.fields.iter() {
            match field {
                Field::Dynamic(id, real_field) => {
                    dynamic_collector(id, real_field);

                    fields.push((field_name.clone(), *real_field.clone()))
                }
                Field::Structure(structure) => {
                    let structure_fields = structure.assemble_fields(dynamic_collector)?;

                    fields.append(&mut structure_fields.into_iter().map(
                        |(other_field_name, field)| (format!("__{}__{}", field_name, other_field_name), field)
                    ).collect::<Vec<_>>())
                }
                _ => fields.push((field_name.clone(), field.clone()))
            }
        }

        Ok(fields)
    }

    pub fn assemble_methods(&self, document: &Document) -> AnyResult<Vec<Method>> {
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

    pub fn assemble_method(&self, document: &Document, method_key: MethodKey) -> AnyResult<Method> {
        self.assemble_method_rec(document, method_key, "".to_string())
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

pub struct AssembledStructure {
    pub fields: Vec<(String, Lit)>,
    pub evaluated_fields: Vec<(String, Expr)>,
    pub methods: Vec<Method>
}

impl AssembledStructure {
    pub fn new(document: &Document, structure: Structure) -> AnyResult<Self> {
        Ok(Self {
            fields: structure
                .assemble_fields(&mut |_, _| {})?
                .into_iter()
                .map(|(name, field)| Ok((name, Lit::try_from(field).context("Couldn't convert field")?)))
                .collect::<AnyResult<Vec<_>>>()?,
            evaluated_fields: Vec::new(),
            methods: structure.assemble_methods(document)?.into_iter().map(|mut method| {
                method.body.alpha_convert(&mut IdentScope::new());
                method.body.inline_blocks();

                println!("Body: {}", method.body);
                method.body.cull_single_use_vars();

                method
            }).collect()
        })
    }
}

impl Display for AssembledStructure {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "Fields: {{ ")?;

        for (name, field) in self.fields.iter() {
            write!(f, "{}: {:?}, ", name, field)?;
        }

        //evaluated fields
        write!(f, " }} Evaluated Fields: {{ ")?;

        for (name, field) in self.evaluated_fields.iter() {
            write!(f, "{}: {}, ", name, field)?;
        }

        write!(f, " }} Methods: {{ ")?;

        for method in self.methods.iter() {
            write!(f, "{}, ", method)?;
        }

        write!(f, " }}")
    }
}

impl TryInto<Lit> for Field {
    type Error = anyhow::Error;

    fn try_into(self) -> Result<Lit, Self::Error> {
        match self {
            Field::F32(f) => Ok(Lit::F32(f)),
            Field::Bool(b) => Ok(Lit::Bool(b)),
            Field::Vec4(v) => Ok(Lit::Vec4(v)),
            Field::Mat4x4(m) => Ok(Lit::Mat4x4(m)),
            field => Err(anyhow!("{} not supported", field))
        }
    }
}

// #[derive(Debug, Clone, PartialEq)]
// pub enum AssembledField {
//     F32(f32),
//     Bool(bool),
//     Vec4(Vec4),
//     Mat4x4(Mat4),
//     Unit,
// }
//
// impl AssembledField {
//     pub fn type_string(&self) -> String {
//         match self {
//             AssembledField::F32(_) => "f32",
//             AssembledField::Bool(_) => "bool",
//             AssembledField::Vec4(_) => "Vec4",
//             AssembledField::Mat4x4(_) => "Mat4",
//             AssembledField::Unit => "()"
//         }.to_string()
//     }
//
//     pub fn types_match(&self, other: &AssembledField) -> bool {
//         self.type_string() == other.type_string()
//     }
// }
//
// impl TryFrom<Field> for AssembledField {
//     type Error = anyhow::Error;
//
//     fn try_from(field: Field) -> AnyResult<Self> {
//         match field {
//             Field::F32(f) => Ok(Self::F32(f)),
//             Field::Bool(b) => Ok(Self::Bool(b)),
//             Field::Vec4(v) => Ok(Self::Vec4(v)),
//             Field::Mat4x4(m) => Ok(Self::Mat4x4(m)),
//             _ => Err(anyhow!("Field not supported"))
//         }
//     }
// }
//
// impl Display for AssembledField {
//     fn fmt(&self, f: &mut Formatter) -> fmt::Result {
//         match self {
//             AssembledField::F32(float) => write!(f, "{}", float),
//             AssembledField::Bool(b) => write!(f, "{}", b),
//             AssembledField::Vec4(v) => write!(f, "{:?}", v),
//             AssembledField::Mat4x4(m) => write!(f, "{:?}", m),
//             AssembledField::Unit => write!(f, "()")
//         }
//     }
// }

#[test]
fn try_assemble_method() {
    let (document, structure) = get_test_stuff(0, 1);
    println!("Document: {document}\nStructure: {structure}");

    let assembled_method = structure.assemble_method(
        &document,
        MethodKey::new(Some("Proj"), "proj")
    ).unwrap();

    let assembled_structure = AssembledStructure::new(&document, structure).unwrap();

    println!("Assembled Method: {}\nAssembled Structure: {}", prettify_string(format!("{assembled_method}")), prettify_string(format!("{assembled_structure}")));
}

pub fn get_test_stuff(opt1: usize, opt2: usize) -> (Document, Structure) {
    let doc_str = match opt1 {
        0 => r#"
            interface Proj {
                proj(vector: Vec4) -> Vec4
            }

            interface Sdf {
                sdf(vector: Vec4) -> f32
            }

            class Sphere4D {
                radius: f32,
                Proj::proj(vector: Vec4) -> Vec4 {
                    radius * normalize(vector)
                },
                Sdf::sdf(vector: Vec4) -> f32 {
                    length(vector) - radius
                }
            }

            class Plane4D {
                normal: Vec4,
                Proj::proj(vector: Vec4) -> Vec4 {
                    vector - dot(vector, normal) * normal
                },
                Sdf::sdf(vector: Vec4) -> f32 {
                    dot(vector, normal)
                }
            }

            class Shift {
                shift: Vec4,
                shape: Class,
                Proj::proj<shape: Proj>(vector: Vec4) -> Vec4 {
                    shape.proj(vector - shift) + shift
                },
                Sdf::sdf<shape: Sdf>(vector: Vec4) -> f32 {
                    shape.sdf(vector - shift)
                }
            }

            class Union {
                shape1: Class,
                shape2: Class,
                Proj::proj<shape1: Proj + Sdf, shape2: Proj + Sdf>(vector: Vec4) -> Vec4 {
                    shape1.proj((5 + shape2.sdf(vector)) * shape2.proj(vector)) * shape1.sdf(vector)
                }
            }
        "#,
        _ => ""
    }.to_string();

    let document = parse_document(&doc_str).unwrap();

    let structure_str = match opt2 {
        0 => r#"
            Union(
                shape1: Sphere4D( radius: 4 ),
                shape2: Shift(
                    shift: [4, 0, 0, 0],
                    shape: Plane4D( normal: [1, 0, 0, 0] )
                )
            )
        "#,
        1 => r#"
            Shift(
                shift: [4, 0, 0, 0],
                shape: Sphere4D( radius: 2 )
            )
        "#,
        _ => ""
    };

    let structure = Structure::from_ron_string(structure_str).unwrap();

    (document, structure)
}