use crate::enviroment::{Environment, SslCallableFn};
use crate::parser::{ExprInner, Lit};

/// This is meant to act as a pseudo-assembly representation of the function
pub struct CompiledFn {
    registers: Vec<Lit>,
    instructions: Vec<Instruction>
}

impl CompiledFn {
    pub fn eval(&mut self, mut inputs: Vec<Lit>, env: &CompiledEnv) -> Lit {
        let CompiledFn { registers, instructions } = self;
        let mut i = 0;

        while i < instructions.len() {
            match &mut instructions[i] {
                Instruction::Assign { output_index, function_index, input_indices, inputs_vec } => {
                    for (i, input_index) in input_indices.iter().enumerate() {
                        inputs_vec[*i] = registers[*input_index].clone();
                    }

                    let output = match env.functions[*function_index]
                    {

                    }
                    registers[*output_index] = output;

                    i += 1;
                }
                Instruction::JumpCondition { instruction_index, register_index } => {
                    match &*registers[register_index] {
                        Lit::Bool(b) if b => {
                            i = *instruction_index;
                        }
                        _ => {} // All other values are treated as false
                    }
                }
                Instruction::Jump { stmt_index: instruction_index } => {
                    i = *instruction_index;
                }
                Instruction::Return { register_index } => {
                    return std::mem::replace(&mut registers[register_index], Lit::Unit);
                }
            }
        }

        Lit::Unit
    }
}

pub enum Instruction {
    Assign { output_index: usize, function_index: usize, input_indices: Vec<usize>, inputs_vec: Vec<Lit> },
    JumpCondition { instruction_index: usize, register_index: usize },
    Jump { stmt_index: usize },
    Return { register_index: usize }
}

pub enum CompiledExpr {
    Application(usize, Vec<usize>),

}

// pub trait Compile {
//     fn compile(&self)
// }

impl ExprInner {
    fn compile(&self) -> () {
        match self {
            ExprInner::BinExpr(_, _, _) => {}
            ExprInner::UnaryExpr(_, _) => {}
            ExprInner::Application(_, _) => {}
            ExprInner::Dot(_, _, _) => {}
            ExprInner::Field(_, _) => {}
            ExprInner::TupleAccess(_, _) => {}
            ExprInner::Tuple(_) => {}
            ExprInner::Var(_, _) => {}
            ExprInner::Lit(_) => {}
            ExprInner::Block(_) => {}
        }
        ()
    }
}

pub struct CompiledEnv {
    functions: Vec<CompiledEnvFunction>,
    consts: Vec<Lit>
}

pub enum CompiledEnvFunction {
    RustFn(Box<dyn SslCallableFn>),
    CompiledFn(CompiledFn)
}