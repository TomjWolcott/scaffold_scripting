mod parser;
mod structure;
mod assemble;
mod tree_walk;
mod ast_operations;
mod interpreter;
mod to_wgsl;
pub mod test_helpers;
mod utils;
mod enviroment;
mod any_value;
mod scope;
mod parser_span;
mod compiler;

pub mod prelude {
    pub use crate::parser::*;
    pub use crate::structure::*;
    pub use crate::assemble::*;
    pub use crate::tree_walk::*;
    pub use crate::ast_operations::*;
    pub use crate::interpreter::*;
    pub use crate::to_wgsl::*;
    pub use crate::utils::*;
    pub use crate::enviroment::*;
    pub use crate::any_value::*;
    pub use crate::scope::*;
    pub use crate::parser_span::*;
}