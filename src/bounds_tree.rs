use glam::Vec4;
use std::fmt::{Display, Formatter};
use std::fmt;
use crate::parser::{Bound, Document, Method, MethodKey, parse_document};
use crate::structure::{Field, Structure};

#[derive(Debug)]
pub struct BoundsTree {
    class_name: String,
    method_key: MethodKey,
    children: Vec<BoundsTree>
}

impl<'a> BoundsTree {
    pub fn new(class_name: String, method_key: MethodKey) -> Self {
        BoundsTree {
            class_name,
            method_key,
            children: vec![],
        }
    }
}

impl Display for BoundsTree {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.class_name)?;

        match &self.method_key {
            MethodKey::Impl(impl_name) => write!(f, "::{} {{ ", impl_name)?,
            MethodKey::Name(name) => write!(f, ".{} {{ ", name)?
        };

        for child in self.children.iter() {
            write!(f, "{}, ", child)?;
        }

        write!(f, "}}")?;

        Ok(())
    }
}

#[derive(Debug)]
enum AssembleMethodError {
    BoundsNotSatisfied(String, String),
    CouldntFindBoundedStructure(String)
}

#[derive(Debug)]
pub enum GetBoundsTreeError {
    NoStructureField(String),
    NoClass(String),
    NoMethod(MethodKey)
}

impl Structure {
    fn get_bounds_tree(
        &self,
        document: &Document,
        method_key: MethodKey
    ) -> Result<BoundsTree, GetBoundsTreeError> {
        let Some(method) = document.get_method(&self.name, &method_key) else {
            return Err(GetBoundsTreeError::NoMethod(method_key));
        };

        let mut bounds_tree = BoundsTree::new(self.name.clone(), method_key);

        for Bound { name, impls } in method.bounds.iter() {
            let Some(Field::Structure(structure)) = self.get_field(name) else {
                return Err(GetBoundsTreeError::NoStructureField(name.clone()));
            };

            for impl_name in impls {
                bounds_tree.children.push(
                    structure.get_bounds_tree(document, MethodKey::Impl(impl_name.clone()))?
                );
            }
        }

        Ok(bounds_tree)
    }

    fn check_bounds(&self, document: &Document, method: &Method) -> bool {
        let mut satisfied_bounds = true;

        for Bound { name, impls } in method.bounds.iter() {
            let Some(Field::Structure(structure)) = self.get_field(name) else { return false };

            let Some(class) = document.get_class(&structure.name) else { return false };

            for impl_name in impls {
                let Some(method_impl) = class.get_impl(impl_name) else { return false };

                satisfied_bounds &= structure.check_bounds(document, method_impl);
            }
        }

        satisfied_bounds
    }

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
fn test_get_bounds_tree() {
    let doc_str = r#"
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
                abc
            }
        }
    "#.to_string();

    let document = parse_document(&doc_str).unwrap();

    println!("Document: {document}");

    let structure_str = r#"
        Union(
            shape1: Sphere4D( radius: 4 ),
            shape2: Shift(
                shift: [4, 0, 0, 0],
                shape: Plane4D( normal: [1, 0, 0, 0.1] )
            )
        )
    "#;

    let structure = Structure::from_ron_string(structure_str).unwrap();

    println!("Structure: {structure}");

    println!("BoundsTree: {}", structure.get_bounds_tree(&document, MethodKey::Name("proj".to_string())).unwrap());
}

#[test]
fn test_check_bounds() {
    let doc_str1 = r#"
        class Sphere4D {
            radius: f32,
            Proj::proj(vector: Vec4) -> Vec4 {
                radius * normalize(vector)
            }
        }

        class Shift {
            shift: Vec4,
            shape: Class,
            Proj::proj<shape: Proj>(vector: Vec4) -> Vec4 {
                shape.proj(vector - shift) + shift
            }
        }
    "#.to_string();

    let doc_str2 = r#"
        class Sphere4D {
            radius: f32,
            Proj::proj(vector: Vec4) -> Vec4 {
                radius * normalize(vector)
            }
        }

        class Shift {
            shift: Vec4,
            shape: Class,
            Proj::proj<shape: Proj + Sdf>(vector: Vec4) -> Vec4 {
                shape.proj(vector - shift) + shift
            }
        }
    "#.to_string();

    let document1 = parse_document(&doc_str1).unwrap();
    let document2 = parse_document(&doc_str2).unwrap();

    let structure = Structure {
        name: "Shift".to_string(),
        fields: vec![
            ("shift".to_string(), Field::Vec4(Vec4::X)),
            ("shape".to_string(), Field::Structure(Box::new(Structure {
                name: "Sphere4D".to_string(),
                fields: vec![
                    ("radius".to_string(), Field::F32(6.0))
                ],
            })))
        ],
    };

    let class1 = document1.get_class("Shift").unwrap();
    let method1 = &class1.methods[0];

    let class2 = document2.get_class("Shift").unwrap();
    let method2 = &class2.methods[0];

    assert!(structure.check_bounds(&document1, method1));
    assert!(!structure.check_bounds(&document2, method2));
}
