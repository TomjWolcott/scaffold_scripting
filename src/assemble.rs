use crate::parser::{Bound, Document, Expr, Method, MethodKey, parse_document, prettify_string, Stmt};
use crate::structure::{Field, Structure};
use crate::tree_walk::{TreeNodeMut, WalkTreeMut};

use anyhow::{anyhow, Context, Result as AnyResult};

impl Structure {
    pub fn assemble_method(&self, document: &Document, method_key: MethodKey) -> AnyResult<Method> {
        self.assemble_method_rec(document, method_key, "head".to_string())
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
                        *var = format!("__{id}__{var}");
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
                        document, MethodKey::new(Some(&interface.name), &method_name), field_name.clone()
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

#[test]
fn assemble_method() {
    let (document, structure) = get_test_stuff(0, 1);
    println!("Document: {document}\nStructure: {structure}");

    let assembled_method = structure.assemble_method(
        &document,
        MethodKey::new(Some("Sdf"), "sdf")
    ).unwrap();

    println!("Assembled Method: {}", prettify_string(format!("{assembled_method}")));
}

fn get_test_stuff(opt1: usize, opt2: usize) -> (Document, Structure) {
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
                    shape: Plane4D( normal: [1, 0, 0, 0.1] )
                )
            )
        "#,
        1 => r#"
            Shift(
                shift: [4, 0, 0, 0],
                shape: Plane4D( normal: [1, 0, 0, 0.1] )
            )
        "#,
        _ => ""
    };

    let structure = Structure::from_ron_string(structure_str).unwrap();

    (document, structure)
}