use glam::Vec4;
use std::fmt::{Display, Formatter};
use std::fmt;
use crate::parser::{Bound, Document, Expr, Interface, Method, MethodKey, parse_document, prettify_string, Stmt};
use crate::structure::{Field, Structure};
use crate::tree_walk::{TreeNodeMut, WalkTreeMut};

use anyhow::{anyhow, Context, Result as AnyResult};


// EXAMPLE:
//  Union.proj {
//      shape1: Sphere4D::Proj {},
//      shape1: Sphere4D::Sdf {},
//      shape2: Shift::Proj {
//          shape: Plane4D::Proj {}
//      },
//      shape2: Shift::Sdf {
//          shape: Plane4D::Sdf {},
//      }
//  }
// A BETTER VERSION???
//  Union.proj {
//      shape1: Sphere4D::Proj {},
//      shape1: Sphere4D::Sdf {},
//      shape2: Shift::Proj {
//          shape: Plane4D::Proj {}
//      },
//      shape2: Shift::Sdf {
//          shape: Plane4D::Sdf {},
//      }
//  }

// pub struct BoundsTree1<'a> {
//     method: &'a Method,
//     nodes: Vec<BoundsTreeNode<'a>>
// }

// Sdf::sdf<shape1: Proj + Sdf, shape2: Proj + Sdf>

pub struct BoundsTreeNode<'a> {
    interface: &'a Interface,
    field_name: String,
    children: Vec<BoundsTreeNode<'a>>
}

impl<'a> Display for BoundsTreeNode<'a> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}.{} {{ ", self.interface.name, self.field_name)?;

        for child in self.children.iter() {
            write!(f, "{}, ", child)?;
        }

        write!(f, "}}")?;

        Ok(())
    }

}

#[derive(Debug)]
pub struct BoundsTree {
    class_name: String,
    method_key: MethodKey,
    children: Vec<(String, BoundsTree)>
}

impl BoundsTree {
    pub fn new(class_name: String, method_key: MethodKey) -> Self {
        BoundsTree {
            class_name,
            method_key,
            children: vec![],
        }
    }

    pub fn get_child(&self, name: impl AsRef<str>, method_key: MethodKey) -> Option<&Self> {
        self.children.iter()
            .find(|(field_name, _)| field_name.as_str() == name.as_ref())
            .map(|(_, child)| child)
    }

    // pub fn assemble_method(&self, document: &Document) -> Result<Method, AssembleMethodError> {
    //     let Some(class) = document.get_class(&self.class_name) else {
    //         return Err(AssembleMethodError::ClassNotFound(self.class_name.clone()))
    //     };
    //
    //     class.get_implementations()
    //
    //     let Some(mut method) = document.get_method(&self.class_name, &self.method_key).cloned() else {
    //         return Err(AssembleMethodError::MethodNotFound(self.class_name.clone(), self.method_key.clone()));
    //     };
    //
    //     method.body.walk_tree_mut(&mut |node| {
    //         let TreeNodeMut::Expr(expr) = node else { return Ok(()) };
    //
    //         let Expr::Dot(field_name, _, args) = expr;
    //
    //         let Some(bounds_tree) = self.get_child(field_name) else {
    //             return Err(AssembleMethodError::ChildNotFound(field_name))
    //         };
    //
    //         let Method {
    //             inputs,
    //             mut body,
    //             ..
    //         } = bounds_tree.assemble_method(document)?;
    //
    //         for (input, arg) in inputs.iter().zip(args).rev() {
    //             body.0.insert(0, Stmt::Declare(input.clone(), arg.clone()));
    //         }
    //
    //
    //
    //         Ok(())
    //     })?;
    //
    //     Ok(method)
    // }
}

// impl Display for BoundsTree {
//     fn fmt(&self, f: &mut Formatter) -> fmt::Result {
//         write!(f, "{}", self.class_name)?;
//
//         match &self.method_key {
//             MethodKey::Impl(impl_name) => write!(f, "::{} {{ ", impl_name)?,
//             MethodKey::Name(name) => write!(f, ".{} {{ ", name)?
//         };
//
//         for (name, child) in self.children.iter() {
//             write!(f, "{}: {}, ", name, child)?;
//         }
//
//         write!(f, "}}")?;
//
//         Ok(())
//     }
// }

#[derive(Debug)]
enum AssembleMethodError {
    MethodNotFound(String, MethodKey),
    ChildNotFound(String),
    ClassNotFound(String),
}

#[derive(Debug)]
pub enum GetBoundsTreeError {
    NoStructureField(String),
    NoClass(String),
    NoMethod(MethodKey),
    NoInterface(String),
}

impl Structure {
    fn assemble_method2(&self, document: &Document, method_key: MethodKey) -> AnyResult<Method> {
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
                    *var = format!("__{}__{var}", self.name);

                    Ok(())
                },
                Expr::Dot(field_name, method_name, args) => {
                    let bound = &bounds.iter().find(|Bound { name, .. }| field_name == name).context("Couldn't find bound")?;
                    let interface = bound.get_interface_with_method(document, &method_name).context("Couldn't get interface")?;
                    let Some(Field::Structure(structure)) = self.get_field(field_name) else {
                        return Err(anyhow!("Couldn't get field"));
                    };

                    let Method { mut body, inputs, .. } = structure.assemble_method2(
                        document, MethodKey(Some(interface.name.clone()), method_name.clone())
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

    // fn assemble_method(&self, document: &Document, method_key: MethodKey) -> AnyResult<Method> {
    //     let mut method = document
    //         .get_method(&self.name, &method_key)
    //         .context("Could not find method")?
    //         .clone();
    //
    //     let class = document.get_class(&self.name)?;
    //
    //     for Bound { name, impls } in method.bounds.iter() {
    //         let Some(Field::Structure(bound_structure)) = self.get_field(name) else {
    //             return Err(anyhow!("Could not get structure field {} in {}", name, self.name));
    //         };
    //
    //         let bound_class = document.get_class(&bound_structure.name)?;
    //
    //         for impl_name in impls {
    //             let interface = document
    //                 .get_interface(impl_name)
    //                 .with_context(|| format!("Could not find interface {}", impl_name))?;
    //
    //             for Method { mut body, .. } in bound_class.get_implementations(interface)? {
    //                 body.walk_tree_mut(&mut |node| {
    //                     let TreeNodeMut::Expr(expr) = node else { return Ok(()) };
    //
    //                     let Expr::Dot(field_name, method_name, args) = expr;
    //
    //                     self.get_field(field_name)
    //
    //                     let Method {
    //                         inputs,
    //                         mut body,
    //                         ..
    //                     } = bounds_tree.assemble_method(document)?;
    //
    //                     for (input, arg) in inputs.iter().zip(args).rev() {
    //                         body.0.insert(0, Stmt::Declare(input.clone(), arg.clone()));
    //                     }
    //
    //
    //
    //                     Ok(())
    //                 })?;
    //             }
    //
    //         }
    //     }
    //
    //     Ok(method)
    // }
    //
    // fn get_bounds_tree(
    //     &self,
    //     document: &Document,
    //     method_key: MethodKey
    // ) -> Result<BoundsTree, GetBoundsTreeError> {
    //     let Some(method) = document.get_method(&self.name, &method_key) else {
    //         return Err(GetBoundsTreeError::NoMethod(method_key));
    //     };
    //
    //     let mut bounds_tree = BoundsTree::new(self.name.clone(), method_key);
    //
    //     for Bound { name, impls } in method.bounds.iter() {
    //         let Some(Field::Structure(structure)) = self.get_field(name) else {
    //             return Err(GetBoundsTreeError::NoStructureField(name.clone()));
    //         };
    //
    //         for impl_name in impls {
    //             for implemen
    //
    //             bounds_tree.children.push((
    //                 name.clone(),
    //                 structure.get_bounds_tree(document, MethodKey::Impl(impl_name.clone()))?
    //             ));
    //         }
    //     }
    //
    //     Ok(bounds_tree)
    // }

    // fn assemble_method(&self, document: &Document, method: &Method) -> Result<Method, AssembleMethodError> {
    //
    //
    //     for Bound { name, impls } in method.bounds.iter() {
    //         let Some(Field::Structure(structure)) = self.get_field(name) else {
    //             return Err(AssembleMethodError::CouldntFindBoundedStructure(name.clone()));
    //         };
    //     }
    //
    //     Ok(Method {  })
    // }
}

#[test]
fn assemble_method() {
    let (document, structure) = get_test_stuff(0, 1);
    println!("Document: {document}\nStructure: {structure}");

    let assembled_method = structure.assemble_method2(
        &document,
        MethodKey::new(Some("Sdf"), "sdf")
    ).unwrap();

    println!("Assembled Method: {}", prettify_string(format!("{assembled_method}")));
}

// #[test]
// fn test_get_bounds_tree() {
//     let (document, structure) = get_test_stuff(0, 0);
//
//     println!("BoundsTree: {}", structure.get_bounds_tree(&document, MethodKey::Name("proj".to_string())).unwrap());
// }

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