use std::fmt::{Display, Formatter};
use std::sync::Arc;
use anyhow::{anyhow, Context, Result as AnyResult};
use crate::enviroment::{Environment, SslCallableFn};
use crate::parser::{parse_fn, Binding, Block, Expr, ExprInner, Function, Lit, Lvalue, LvalueDeclare, Stmt};
use crate::prelude::Scope;


/// This is meant to act as a pseudo-assembly representation of the function
pub struct CompiledFn {
    num_registers_needed: usize,
    instructions: Vec<Instruction>,
    env: Arc<CompiledEnv>
}

impl CompiledFn {
    pub fn eval(&self, inputs: Vec<Lit>) -> Lit {
        let mut registers = inputs;
        registers.resize(self.num_registers_needed, Lit::Unit);

        let mut i = 0;

        while i < self.instructions.len() {
            match &self.instructions[i] {
                Instruction::Assign { output_register: output_index, function_index, input_registers: input_indices } => {
                    let mut inputs = Vec::with_capacity(input_indices.len());

                    for input_index in input_indices.iter() {
                        inputs.push(registers[*input_index].clone());
                    }

                    let output = self.env.functions[*function_index].call(inputs);
                    registers[*output_index] = output;

                    i += 1;
                }
                Instruction::Move { output_register: output_index, input_register: input_index } => {
                    registers[*output_index] = registers[*input_index].clone();
                }
                Instruction::Set { register, lit } => {
                    registers[*register] = lit.clone();
                }
                Instruction::JumpCondition { instruction_index, register } => {
                    match &registers[*register] {
                        Lit::Bool(b) if *b => {
                            i = *instruction_index;
                        }
                        _ => {} // All other values are treated as false
                    }
                }
                Instruction::Jump { instruction_index } => {
                    i = *instruction_index;
                }
                Instruction::Return { register } => {
                    return std::mem::replace(&mut registers[*register], Lit::Unit);
                }
            }
        }

        Lit::Unit
    }
}

impl Display for CompiledFn {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "#Registers: {}\n[{}]\n\nInstructions:{}",
            self.num_registers_needed,
            &", - ".repeat(self.num_registers_needed)[1..],
            self.instructions.iter().enumerate()
                .map(|(i, instr)| format!("\n  [{i:0>4}]: {instr}")).collect::<Vec<_>>().join("")
        )
    }
}

pub enum Instruction {
    Assign { output_register: usize, function_index: usize, input_registers: Vec<usize> },
    Move { output_register: usize, input_register: usize },
    Set { lit: Lit, register: usize },
    JumpCondition { instruction_index: usize, register: usize },
    Jump { instruction_index: usize },
    Return { register: usize }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Assign {
                output_register,
                function_index,
                input_registers
            } => write!(f, "f{function_index}({}) -> r{output_register}", input_registers.iter().map(|reg| {
                format!("r{reg}")
            }).collect::<Vec<_>>().join(", ")),
            Instruction::Move { output_register, input_register } => write!(f, "r{input_register} -> r{output_register}"),
            Instruction::Set { lit, register } => write!(f, "{lit} -> r{register}"),
            Instruction::JumpCondition { instruction_index, register } => write!(f, "If not r{register} goto {instruction_index}"),
            Instruction::Jump { instruction_index } => write!(f, "Goto {instruction_index}"),
            Instruction::Return { register } => write!(f, "Return r{}", register)
        }
    }
}

// pub trait Compile {
//     fn compile(&self)
// }

pub struct RegistersInUse(Vec<bool>);

impl RegistersInUse {
    fn new() -> Self {
        Self(Vec::new())
    }

    fn use_first_unused_register(&mut self) -> usize {
        if let Some(i) = self.0.iter().position(|b| !*b) {
            self.0[i] = true;
            i
        } else {
            self.0.push(true);
            self.0.len() - 1
        }
    }

    fn free_register(&mut self, i: usize) {
        self.0[i] = false;
    }

    fn num_registers(&self) -> usize {
        self.0.len()
    }
}

impl Function {
    fn compile(&self, env: Arc<CompiledEnv>) -> AnyResult<CompiledFn> {
        let mut scope = Scope::new();
        let mut instructions = Vec::new();
        let mut registers_in_use = RegistersInUse::new();

        for Binding(name, _) in self.inputs.iter() {
            let reg = registers_in_use.use_first_unused_register();

            scope.push(name.clone(), reg);
        }

        if let Some(reg) = self.body.compile(&mut scope, &mut instructions, &mut registers_in_use)? {
            instructions.push(Instruction::Return { register: reg });
        }

        Ok(CompiledFn {
            num_registers_needed: registers_in_use.num_registers(),
            instructions,
            env
        })
    }
}

impl Block {
    fn compile(
        &self,
        scope: &mut Scope<usize>,
        instructions: &mut Vec<Instruction>,
        registers_in_use: &mut RegistersInUse
    ) -> AnyResult<Option<usize>> {
        for stmt in self.0.iter() {
            stmt.compile(scope, instructions, registers_in_use)?;
        }

        if let Some(expr) = &self.1 {
            let reg = expr.compile(scope, instructions, registers_in_use)?;

            Ok(Some(reg))
        } else {
            Ok(None)
        }
    }
}

impl Stmt {
    fn compile(
        &self,
        scope: &mut Scope<usize>,
        instructions: &mut Vec<Instruction>,
        registers_in_use: &mut RegistersInUse
    ) -> AnyResult<()> {
        match self {
            Stmt::Declare(lvalue, expr) => {
                let LvalueDeclare::Binding(Binding(name, _)) = lvalue else {
                    return Err(anyhow!("lvalue needs to have been converted to a simple binding by this point"))
                };

                let input_register = expr.compile(scope, instructions, registers_in_use)?;
                let output_register = registers_in_use.use_first_unused_register();

                instructions.push(Instruction::Move {
                    output_register,
                    input_register
                });

                scope.push(name.clone(), input_register);
            }
            Stmt::Assign(lvalue, expr) => {
                match lvalue {
                    Lvalue::Var(name) => {
                        let input_register = expr.compile(scope, instructions, registers_in_use)?;
                        let output_register = *scope.get(&name)
                            .context(format!("Could not find var \"{}\" in assign", name))?;

                        instructions.push(Instruction::Move {
                            output_register,
                            input_register
                        });
                    }
                    Lvalue::Fields(name, fields) => { unimplemented!() }
                    Lvalue::TupleDestructure(_) => { unimplemented!() }
                }
            }
            Stmt::IfElse(if_part, if_elses, else_block) => {
                let mut iter = [if_part].into_iter().chain(if_elses.iter());
                let mut jump_instrs = Vec::new();

                for (cond, block) in iter {
                    let reg = cond.compile(scope, instructions, registers_in_use)?;

                    let mut cond_index = instructions.len();

                    instructions.push(Instruction::JumpCondition {
                        instruction_index: 0,
                        register: reg,
                    });

                    if let Some(useless_reg) = block.compile(scope, instructions, registers_in_use)? {
                        registers_in_use.free_register(useless_reg);
                    }

                    jump_instrs.push(instructions.len());

                    // these should all be replaced
                    instructions.push(Instruction::Jump {
                        instruction_index: 123456
                    });

                    let after_block_index = instructions.len();

                    match &mut instructions[cond_index] {
                        Instruction::JumpCondition { instruction_index, .. } => *instruction_index = after_block_index,
                        _ => unreachable!()
                    }
                }

                if let Some(block) = else_block {
                    if let Some(useless_reg) = block.compile(scope, instructions, registers_in_use)? {
                        registers_in_use.free_register(useless_reg);
                    }
                }

                let end_ifs_index = instructions.len();

                for index in jump_instrs {
                    instructions[index] = Instruction::Jump {
                        instruction_index: end_ifs_index
                    }
                }
            }
            Stmt::Expr(_) => {}
            Stmt::Noop => {}
        }

        Ok(())
    }
}

impl Expr {
    fn compile(&self, scope: &Scope<usize>, instructions: &mut Vec<Instruction>, registers_in_use: &mut RegistersInUse) -> AnyResult<usize> {
        self.0.compile(scope, instructions, registers_in_use).context(self.span())
    }
}

impl ExprInner {
    fn compile(
        &self,
        scope: &Scope<usize>,
        instructions: &mut Vec<Instruction>,
        registers_in_use: &mut RegistersInUse
    ) -> AnyResult<usize> {
        match self {
            ExprInner::BinExpr(expr1, op, expr2) => {
                let reg1 = expr1.0.compile(scope, instructions, registers_in_use)?;
                let reg2 = expr2.0.compile(scope, instructions, registers_in_use)?;

                let output_reg = registers_in_use.use_first_unused_register();
                registers_in_use.free_register(output_reg);

                instructions.push(Instruction::Assign {
                    output_register: output_reg,
                    function_index: 0,
                    input_registers: vec![reg1, reg2],
                });

                Ok(output_reg)
            },
            ExprInner::UnaryExpr(op, expr) => {
                let reg = expr.0.compile(scope, instructions, registers_in_use)?;
                let output_reg = registers_in_use.use_first_unused_register();
                registers_in_use.free_register(output_reg);

                instructions.push(Instruction::Assign {
                    output_register: output_reg,
                    function_index: 0,
                    input_registers: vec![reg],
                });

                Ok(output_reg)
            }
            ExprInner::Application(fn_name, exprs) => {
                let regs = exprs.iter().map(|expr| {
                    expr.compile(scope, instructions, registers_in_use)
                }).collect::<AnyResult<Vec<_>>>()?;

                let output_reg = registers_in_use.use_first_unused_register();
                registers_in_use.free_register(output_reg);

                instructions.push(Instruction::Assign {
                    output_register: output_reg,
                    function_index: 0,
                    input_registers: regs,
                });

                Ok(output_reg)
            }
            ExprInner::Field(expr, field_name) => {
                let reg = expr.0.compile(scope, instructions, registers_in_use)?;
                let output_reg = registers_in_use.use_first_unused_register();
                registers_in_use.free_register(output_reg);

                instructions.push(Instruction::Assign {
                    output_register: output_reg,
                    function_index: 0,
                    input_registers: vec![reg],
                });

                Ok(output_reg)
            }
            ExprInner::TupleAccess(expr, tuple_index) => {
                // TODO: Something special for these two

                Err(anyhow!("Not yet implemented"))
            }
            ExprInner::Tuple(exprs) => {
                // TODO: Something special for these two

                Err(anyhow!("Not yet implemented"))
            }
            ExprInner::Var(name, _) => {
                scope.get(name)
                    .context(format!("Could not find {} in register scope", name))
                    .cloned()
            }
            ExprInner::Lit(lit) => {
                let register = registers_in_use.use_first_unused_register();
                registers_in_use.free_register(register);

                instructions.push(Instruction::Set {
                    lit: lit.clone(),
                    register
                });

                Ok(register)
            }
            ExprInner::Dot(_, _, _) => Err(anyhow!("Dot expressions should have been inlined before this point")),
            ExprInner::Block(_) => Err(anyhow!("Blocks should have been inlined before this point"))
        }
    }
}

// lets assume everything is inlined for now...
pub struct CompiledEnv {
    functions: Vec<Box<dyn SslCallableFn>>,
    consts: Vec<Lit>
}

pub enum CompiledEnvFunction {
    RustFn(Box<dyn SslCallableFn>),
    CompiledFn(CompiledFn)
}

#[test]
fn test_compilation() {
    let mut script =
        r#"fn abc(x: f32) -> f32 {
            let y = 3 * x;
            let z = true;
            if (z) {
                y = y + 3;
            } else if (z && true) {
                y = y - 3;
            } else {
                y = y + y + y + y;
            }
            x + y
        }"#;

    let env = Environment::new();

    let function = parse_fn(&script, &env).unwrap();

    println!("{}", function.compile().unwrap())
}