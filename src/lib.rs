mod parser;
mod structure;
mod bounds_tree;
mod tree_walk;

use std::fmt::Display;
use parser::*;
use structure::*;

#[derive(Debug)]
pub enum FromStructureError {
    ClassNotFound(String),
    FieldNotFound(String),
    IncorrectFieldType(String, Type)
}

pub struct AssembledStructure {
    fields: Vec<(String, Field)>,
    methods: Vec<Method>
}

// impl AssembledStructure {
//     fn from_structure(document: &Document, structure: Structure) -> Result<Self, FromStructureError> {
//         let mut structures = VecDeque::from([Box::new(structure)]);
//         let mut fields = Vec::new();
//         let mut methods = Vec::new();
//
//         while structures.len() > 0 {
//             let Structure { name, fields: structure_fields } = structures.pop_front().unwrap();
//
//             let Some(class) = document.classes.iter().find(
//                 |Class { name, .. }| name == &structure.name
//             ) else { return Err(FromStructureError::ClassNotFound(name)); };
//
//             for method in class.methods.iter() {
//                 let bounds_satisfied = method.bounds
//                     .iter()
//                     .all(|Bound { name, impls }| {
//                         true
//                     });
//
//                 if bounds_satisfied {
//                     methods.push(method.clone());
//                 }
//             }
//
//             for (key, field) in structure_fields {
//                 let Some(Binding(_, ty)) = class.fields.iter().find(
//                     |Binding(key2, _)| key2 == &key
//                 ) else { return Err(FromStructureError::FieldNotFound(key.clone())) };
//
//                 match (field, ty) {
//                     (Field::Structure(structure2), Type::Class) => {
//                         structures.push_back(structure2);
//                     },
//                     (Field::Bool(b), Type::Bool) => {
//                         fields.push((key, Field::Bool(b)))
//                     },
//                     (Field::F32(f), Type::F32) => {
//                         fields.push((key, Field::F32(f)))
//                     },
//                     (Field::Vec4(v), Type::Vec4) => {
//                         fields.push((key, Field::Vec4(v)))
//                     },
//                     (Field::Mat4(m), Type::Mat4) => {
//                         fields.push((key, Field::Mat4(m)))
//                     },
//                     (field, ty) => {
//                         return Err(FromStructureError::IncorrectFieldType(format!("{}", field), ty.clone()))
//                     }
//                 }
//             }
//
//         }
//
//         Self { fields }
//     }
// }
