mod parser;
mod structure;
mod assemble;
mod tree_walk;
mod ast_operations;
mod interpreter;
mod to_wgsl;
pub mod test_helpers;

pub mod prelude {
    pub use crate::parser::*;
    pub use crate::structure::*;
    pub use crate::assemble::*;
    pub use crate::tree_walk::*;
    pub use crate::ast_operations::*;
    pub use crate::interpreter::*;
    pub use crate::to_wgsl::*;
}