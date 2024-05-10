use anyhow::{anyhow, Context, Result as AnyResult};
use crate::parser::*;

// pub trait ToWgsl {
//     fn to_wgsl(&self, replace_free_vars: &mut Vec<(String, String)>) -> AnyResult<String>;
// }
//
// impl ToWgsl for Method {
//     fn to_wgsl(&self, replace_free_vars: &mut Vec<(String, String)>) -> AnyResult<String> {
//         todo!()
//     }
// }
//
// impl ToWgsl for Block {
//     fn to_wgsl(&self, replace_free_vars: &mut Vec<(String, String)>) -> AnyResult<String> {
//         todo!()
//     }
// }
//
// impl ToWgsl for Stmt {
//     fn to_wgsl(&self, replace_free_vars: &mut Vec<(String, String)>) -> AnyResult<String> {
//         todo!()
//     }
// }
//
// impl ToWgsl for Expr {
//     fn to_wgsl(&self, replace_free_vars: &mut Vec<(String, String)>) -> AnyResult<String> {
//         todo!()
//     }
// }