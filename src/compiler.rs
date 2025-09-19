use std::cell::LazyCell;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::sync::Arc;
use anyhow::{anyhow, bail, Context, Result as AnyResult};
use glam::Vec4;
use lazy_static::lazy::Lazy;
use crate::enviroment::{Environment, EnvironmentFunction, SslCallableFn, SslCallableFnObj};
use crate::parser::{parse_block, parse_document, parse_fn, Binding, Block, Expr, ExprInner, Function, Lit, Lvalue, LvalueDeclare, LvalueField, Method, Stmt, Type};
use crate::prelude::Scope;
use parking_lot::{MappedRwLockReadGuard, RwLock, RwLockReadGuard, RwLockWriteGuard};
use crate::assemble::AssembledStructure;
use crate::ast_operations::AssignTypes;
use crate::interpreter::{Eval, IntoArgs};
use crate::test_helpers::{get_test_stuff, prettify_string};

pub const DUD_FUNCTION: fn(f32) -> f32 = |x| {println!("DUD!!!"); x};

/// This is meant to act as a pseudo-assembly representation of the function
#[derive(Debug, Clone)]
pub struct CompiledFn {
    num_registers_needed: usize,
    instructions: Vec<Instruction>,
    output: Type,
    env: CompiledEnv
}

impl CompiledFn {
    pub fn pretty_print(&self, num_tabs: usize) -> String {
        const TAB: &'static str = "  ";
        format!(
            "{}CompiledFn {{ \n{}registers ({}): [ {}],\n{}instructions: {},\n{}output: {}\n{}}}",
            TAB.repeat(num_tabs),

            TAB.repeat(num_tabs+1),
            self.num_registers_needed,
            &"- ".repeat(self.num_registers_needed),

            TAB.repeat(num_tabs+1),
            self.instructions.iter().enumerate().map(|(i, instr)| {
                format!("\n{}[{i:0>4}]: {}", TAB.repeat(num_tabs + 2), instr.pretty_print(&self.env))
            }).collect::<Vec<_>>().join(""),

            TAB.repeat(num_tabs+1),
            self.output,


            TAB.repeat(num_tabs),
        )
    }
}

impl Display for CompiledFn {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "#Registers: {} [ {}]\nInstructions:{}\n",
            self.num_registers_needed,
            &"- ".repeat(self.num_registers_needed),
            self.instructions.iter().enumerate()
                .map(|(i, instr)| format!("\n  [{i:0>4}]: {instr}")).collect::<Vec<_>>().join("")
        )
    }
}

impl AssembledStructure {
    pub fn eval_compiled_method<OUT: TryFrom<Lit, Error=anyhow::Error>>(
        &self, method_name: impl AsRef<str>, args: impl IntoArgs
    ) -> AnyResult<OUT> {
        let method = self.compiled_fns.get(&method_name.as_ref().to_string())
            .ok_or(anyhow!("Could not find compiled method with name: {:?}", method_name.as_ref()))?;
        let mut inputs: Vec<Lit> = self.evaluated_scope.iter().map(|(_, lit)| lit.clone()).collect();
        inputs.append(&mut args.into_args());
        method.call(inputs).try_into()
    }
}

macro_rules! implCallNForCompiledFn {
    ($($call:ident | ($($param:ident ),*)),*) => {
        $(
            fn $call (&self $(, $param: Lit)*) -> Lit {
                self.call(vec![$($param),*])
            }
        )*
    };
}

impl SslCallableFn for CompiledFn {
    fn call(&self, inputs: Vec<Lit>) -> Lit {
        let env_inner = self.env.0.read();
        let mut registers = inputs;
        registers.resize(self.num_registers_needed, Lit::Unit);

        let mut i = 0;

        // println!();
        // println!("{}[{}]", " ".repeat(40), registers.iter().map(|r| format!("{:^7}", r.to_string())).collect::<Vec<_>>().join(", "));
        while i < self.instructions.len() {
            println!("{:<40}", format!("Instruction #{i}: {}", self.instructions[i].pretty_print(&self.env)));
            match &self.instructions[i] {
                Instruction::Assign { output_register: output_index, function_index, input_registers: input_indices } => {
                    let mut inputs = Vec::with_capacity(input_indices.len());

                    for input_index in input_indices.iter() {
                        inputs.push(registers[*input_index].clone());
                    }

                    let output = env_inner.functions[*function_index].call(inputs);
                    registers[*output_index] = output;

                    i += 1;
                }
                Instruction::Move { output_register: output_index, input_register: input_index } => {
                    registers[*output_index] = registers[*input_index].clone();

                    i += 1;
                }
                Instruction::Set { register, lit } => {
                    registers[*register] = lit.clone();

                    i += 1;
                }
                Instruction::JumpCondition { instruction_index, register } => {
                    match &registers[*register] {
                        Lit::Bool(b) if *b => { i += 1; }
                        _ => { i = *instruction_index; } // All other values are treated as false
                    }
                }
                Instruction::Jump { instruction_index } => {
                    i = *instruction_index;
                }
                Instruction::CreateTuple { element_registers, output_register } => {
                    registers[*output_register] = Lit::Tuple(element_registers.iter().map(|reg| registers[*reg].clone()).collect());

                    i += 1;
                }
                Instruction::TupleGet { output_register, tuple_register, index } => {
                    let Lit::Tuple(elements) = &registers[*tuple_register] else {
                        panic!("Expected tuple in register {}", tuple_register);
                    };

                    let Some(element) = elements.get(*index).cloned() else {
                        panic!("Tuple in register {} does not have index {}", tuple_register, index);
                    };

                    registers[*output_register] = element;

                    i += 1;
                }
                Instruction::TupleSet { tuple_register, index, set_register } => {
                    let element = registers[*set_register].clone();

                    let Lit::Tuple(elements) = &mut registers[*tuple_register] else {
                        panic!("Expected tuple in register {}", tuple_register);
                    };

                    elements[*index] = element;

                    i += 1;
                }
                Instruction::Return { register } => {
                    return std::mem::replace(&mut registers[*register], Lit::Unit);
                }
            }

            println!("[{}]", registers.iter().map(|r| format!("{}", r.to_string())).collect::<Vec<_>>().join(", "));
        }

        Lit::Unit
    }

    implCallNForCompiledFn!(
        call0 | (),
        call1 | (p1),
        call2 | (p1, p2),
        call3 | (p1, p2, p3),
        call4 | (p1, p2, p3, p4),
        call5 | (p1, p2, p3, p4, p5),
        call6 | (p1, p2, p3, p4, p5, p6),
        call7 | (p1, p2, p3, p4, p5, p6, p7),
        call8 | (p1, p2, p3, p4, p5, p6, p7, p8),
        call9 | (p1, p2, p3, p4, p5, p6, p7, p8, p9),
        call10 | (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10),
        call11 | (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11)
    );

    fn output(&self) -> Type {
        self.output.clone()
    }
}

#[derive(Debug, Clone)]
pub enum Instruction {
    Assign { output_register: usize, function_index: usize, input_registers: Vec<usize> },
    Move { output_register: usize, input_register: usize },
    Set { lit: Lit, register: usize },
    JumpCondition { instruction_index: usize, register: usize },
    Jump { instruction_index: usize },
    CreateTuple { output_register: usize, element_registers: Vec<usize> },
    TupleGet { output_register: usize, tuple_register: usize, index: usize },
    TupleSet { tuple_register: usize, index: usize, set_register: usize },
    Return { register: usize }
}

impl Instruction {
    fn pretty_print(&self, env: &CompiledEnv) -> String {
        match self {
            Self::Assign {
                output_register,
                function_index,
                input_registers
            } => format!(
                "f{function_index}({}) -> r{output_register} ({})",
                input_registers.iter().map(|reg| format!("r{reg}")).collect::<Vec<_>>().join(", "),
                env.0.read().fn_map.iter().find_map(|((_, name, input_types), fn_index)| {
                    if fn_index == function_index {
                        Some(format!("{name}({})", input_types.iter().map(|ty| ty.to_string()).collect::<Vec<_>>().join(", ")))
                    } else {
                        None
                    }
                }).unwrap_or("???".to_string())
            ),
            _ => format!("{self}")
        }
    }
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
            Instruction::CreateTuple { output_register: output_reg, element_registers: registers } => write!(f, "({}) -> r{output_reg}", registers.iter().map(|reg| format!("r{reg}")).collect::<Vec<_>>().join(", ")),
            Instruction::TupleGet { output_register, tuple_register, index } => write!(f, "r{tuple_register}.{index} -> r{output_register}"),
            Instruction::TupleSet { tuple_register, index, set_register } => write!(f, "r{set_register} -> r{tuple_register}.{index}"),
            Instruction::Return { register } => write!(f, "Return r{}", register)
        }
    }
}

// pub trait Compile {
//     fn compile(&self)
// }

#[derive(Clone, PartialEq, Debug)]
pub enum RegisterState {
    Free,
    Temp,
    Var(usize) // scope size (counted by number of nested blocks, 0 = scope of immediate block)
}

pub struct RegistersInUse(Vec<RegisterState>);

impl RegistersInUse {
    fn new() -> Self {
        Self(Vec::new())
    }

    fn get_free_register(&mut self) -> usize {
        if let Some(i) = self.0.iter().position(|b| *b == RegisterState::Free) {
            i
        } else {
            self.0.push(RegisterState::Free);
            self.0.len() - 1
        }
    }

    fn use_var(&mut self) -> usize {
        let i = self.get_free_register();

        self.0[i] = RegisterState::Var(0);
        i
    }

    /// Sets the first available register to Temp and returns its index.  Pushes a new register if none are available.
    fn use_temp(&mut self) -> usize {
        let i = self.get_free_register();

        self.0[i] = RegisterState::Temp;
        i
    }

    /// Frees the register at the index if it is a Temp register.
    fn free_temp(&mut self, i: usize) {
        if let Some(RegisterState::Temp) = self.0.get(i) {
            self.0[i] = RegisterState::Free;
        }
    }

    fn enter_block(&mut self) {
        for state in self.0.iter_mut() {
            if let RegisterState::Var(depth) = state {
                *depth += 1;
            }
        }
    }

    fn exit_block(&mut self) {
        for state in self.0.iter_mut() {
            if let RegisterState::Var(depth) = state {
                if *depth == 0 {
                    *state = RegisterState::Free;
                } else {
                    *depth -= 1;
                }
            }
        }
    }

    fn num_registers(&self) -> usize {
        self.0.len()
    }
}

impl Method {
    pub fn compile(&self, env: &Environment, compiled_env: &CompiledEnv, field_names: &Vec<(String, Type)>) -> AnyResult<CompiledFn> {
        let mut scope = Scope::new();
        let mut instructions = Vec::new();
        let mut registers_in_use = RegistersInUse::new();

        for (field_name, ty) in field_names {
            let reg = registers_in_use.use_var();

            scope.push(field_name.clone(), (reg, ty.clone()));
        }

        for Binding(name, ty) in self.inputs.iter() {
            let reg = registers_in_use.use_var();

            scope.push(name.clone(), (reg, ty.clone()));
        }

        if let Some(reg) = self.body.compile(env, &mut scope, &mut instructions, &mut registers_in_use, compiled_env)? {
            instructions.push(Instruction::Return { register: reg });
        }

        Ok(CompiledFn {
            num_registers_needed: registers_in_use.num_registers(),
            instructions,
            output: self.output.clone(),
            env: compiled_env.clone()
        })
    }
}

impl Function {
    fn compile(&self, env: &Environment, compiled_env: &CompiledEnv) -> AnyResult<CompiledFn> {
        let mut scope = Scope::new();
        let mut instructions = Vec::new();
        let mut registers_in_use = RegistersInUse::new();

        for Binding(name, ty) in self.inputs.iter() {
            let reg = registers_in_use.use_var();

            scope.push(name.clone(), (reg, ty.clone()));
        }

        if let Some(reg) = self.body.compile(env, &mut scope, &mut instructions, &mut registers_in_use, compiled_env)? {
            instructions.push(Instruction::Return { register: reg });
        }

        Ok(CompiledFn {
            num_registers_needed: registers_in_use.num_registers(),
            instructions,
            output: self.output.clone(),
            env: compiled_env.clone()
        })
    }
}

impl Block {
    fn compile(
        &self,
        env: &Environment,
        scope: &mut Scope<(usize, Type)>,
        instructions: &mut Vec<Instruction>,
        registers_in_use: &mut RegistersInUse,
        compiled_env: &CompiledEnv
    ) -> AnyResult<Option<usize>> {
        registers_in_use.enter_block();
        for stmt in self.0.iter() {
            stmt.compile(env, scope, instructions, registers_in_use, compiled_env)?;
        }

        let output = if let Some(expr) = &self.1 {
            let reg = expr.compile(env, scope, instructions, registers_in_use, compiled_env)?;

            Some(reg)
        } else {
            None
        };

        registers_in_use.exit_block();

        Ok(output)
    }
}

impl Stmt {
    fn compile(
        &self,
        env: &Environment,
        scope: &mut Scope<(usize, Type)>,
        instructions: &mut Vec<Instruction>,
        registers_in_use: &mut RegistersInUse,
        compiled_env: &CompiledEnv
    ) -> AnyResult<()> {
        match self {
            Stmt::Declare(lvalue, expr) => {
                let LvalueDeclare::Binding(Binding(name, ty)) = lvalue else {
                    return Err(anyhow!("lvalue needs to have been converted to a simple binding by this point"))
                };

                let input_reg = expr.compile(env, scope, instructions, registers_in_use, compiled_env)?;
                registers_in_use.free_temp(input_reg);
                let output_reg = registers_in_use.use_var();

                instructions.push(Instruction::Move {
                    output_register: output_reg,
                    input_register: input_reg
                });

                scope.push(name.clone(), (input_reg, ty.clone()));
            }
            Stmt::Assign(lvalue, expr) => {
                let input_reg = expr.compile(env, scope, instructions, registers_in_use, compiled_env)?;
                let name = match lvalue {
                    Lvalue::Var(name) | Lvalue::Fields(name, _) => name,
                    _ => panic!("Tuple destructures should have been removed by now")
                };

                let (var_reg, var_ty) = scope.get(&name)
                    .context(format!("Could not find var \"{}\" in assign", name))?;

                match lvalue {
                    Lvalue::Var(name) => {
                        registers_in_use.free_temp(input_reg);

                        instructions.push(Instruction::Move {
                            output_register: *var_reg,
                            input_register: input_reg
                        });
                    }
                    Lvalue::Fields(_, fields) => {
                        let mut tys = vec![var_ty.clone()];
                        let mut registers = vec![*var_reg];

                        for lvalue_field in fields.iter() {
                            let output_reg = registers_in_use.use_temp();

                            match lvalue_field {
                                LvalueField::Field(field_name) => {
                                    let (_, _, getter, _) = &*env.get_field(field_name, tys.last().unwrap().clone())
                                        .ok_or(anyhow!("Can't find field"))?;

                                    let getter_index = compiled_env.0.write().get_or_insert_fn(
                                        env.id(),
                                        format!("[get]{field_name}"),
                                        vec![tys.last().unwrap().clone()],
                                        getter.clone()
                                    );

                                    tys.push(getter.output());

                                    instructions.push(Instruction::Assign {
                                        output_register: output_reg,
                                        function_index: getter_index,
                                        input_registers: vec![*registers.last().unwrap()],
                                    });
                                }
                                LvalueField::TupleAccess(index) => {
                                    let Type::Tuple(tuple_tys) = tys.last().unwrap().clone() else {
                                        panic!("Tuple access should only happen on tuples");
                                    };

                                    tys.push(tuple_tys[*index].clone());

                                    instructions.push(Instruction::TupleGet {
                                        tuple_register: *registers.last().unwrap(),
                                        index: *index,
                                        output_register: output_reg
                                    })
                                }
                            }

                            registers.push(output_reg);
                        }

                        instructions.push(Instruction::Move {
                            input_register: input_reg,
                            output_register: *registers.last().unwrap()
                        });

                        registers_in_use.free_temp(input_reg);

                        for lvalue_field in fields.iter().rev() {
                            let (set_reg, set_ty) = (registers.pop().unwrap(), tys.pop().unwrap());
                            registers_in_use.free_temp(set_reg);
                            match lvalue_field {
                                LvalueField::Field(field_name) => {
                                    let (_, _, _, setter) = &*env.get_field(field_name, tys.last().unwrap().clone())
                                        .ok_or(anyhow!("Can't find field"))?;

                                    let setter_index = compiled_env.0.write().get_or_insert_fn(
                                        env.id(),
                                        format!("[set]{field_name}"),
                                        vec![tys.last().unwrap().clone(), set_ty],
                                        setter.clone()
                                    );

                                    instructions.push(Instruction::Assign {
                                        output_register: *registers.last().unwrap(),
                                        function_index: setter_index,
                                        input_registers: vec![*registers.last().unwrap(), set_reg],
                                    });
                                }
                                LvalueField::TupleAccess(index) => {
                                    instructions.push(Instruction::TupleSet {
                                        tuple_register: *registers.last().unwrap(),
                                        index: *index,
                                        set_register: set_reg,
                                    })
                                }
                            }
                        }

                        registers_in_use.free_temp(*registers.last().unwrap());
                    }
                    Lvalue::TupleDestructure(_) => { panic!("Tuple destructures should have been removed by now") }
                }
            }
            Stmt::IfElse(if_part, if_elses, else_block) => {
                let mut iter = [if_part].into_iter().chain(if_elses.iter());
                let mut jump_instrs = Vec::new();

                for (cond, block) in iter {
                    let reg = cond.compile(env, scope, instructions, registers_in_use, compiled_env)?;
                    registers_in_use.free_temp(reg);

                    let mut cond_index = instructions.len();

                    instructions.push(Instruction::JumpCondition {
                        instruction_index: 0,
                        register: reg,
                    });

                    if let Some(useless_reg) = block.compile(env, scope, instructions, registers_in_use, compiled_env)? {
                        registers_in_use.free_temp(useless_reg);
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
                    if let Some(useless_reg) = block.compile(env, scope, instructions, registers_in_use, compiled_env)? {
                        registers_in_use.free_temp(useless_reg);
                    }
                }

                let end_ifs_index = instructions.len();

                for index in jump_instrs {
                    instructions[index] = Instruction::Jump {
                        instruction_index: end_ifs_index
                    }
                }
            }
            Stmt::Expr(expr) => {
                let reg = expr.compile(env, scope, instructions, registers_in_use, compiled_env)?;
                registers_in_use.free_temp(reg);
            }
            Stmt::Noop => {}
        }

        Ok(())
    }
}

impl Expr {
    fn compile(
        &self,
        env: &Environment,
        scope: &Scope<(usize, Type)>,
        instructions: &mut Vec<Instruction>,
        registers_in_use: &mut RegistersInUse,
        compiled_env: &CompiledEnv
    ) -> AnyResult<usize> {
        self.0.compile(env, scope, instructions, registers_in_use, compiled_env).context(self.span())
    }
}

impl ExprInner {
    fn compile(
        &self,
        env: &Environment,
        scope: &Scope<(usize, Type)>,
        instructions: &mut Vec<Instruction>,
        registers_in_use: &mut RegistersInUse,
        compiled_env: &CompiledEnv
    ) -> AnyResult<usize> {
        match self {
            ExprInner::BinExpr(expr1, op, expr2) => {
                let reg1 = expr1.0.compile(env, scope, instructions, registers_in_use, compiled_env)?;
                let reg2 = expr2.0.compile(env, scope, instructions, registers_in_use, compiled_env)?;
                registers_in_use.free_temp(reg1);
                registers_in_use.free_temp(reg2);

                let output_reg = registers_in_use.use_temp();

                let ty1 = expr1.eval_type(env)?;
                let ty2 = expr2.eval_type(env)?;
                let (_, func) = &*env.get_binary_op(op, ty1.clone(), ty2.clone())
                    .ok_or(anyhow!("Cannot find binary op: {} {} {}", ty1, op, ty2))?;

                let fn_index = compiled_env.0.write().get_or_insert_fn(env.id(), format!("{op}"), vec![ty1, ty2], func.clone());

                instructions.push(Instruction::Assign {
                    output_register: output_reg,
                    function_index: fn_index,
                    input_registers: vec![reg1, reg2],
                });

                Ok(output_reg)
            },
            ExprInner::UnaryExpr(op, expr) => {
                let reg = expr.0.compile(env, scope, instructions, registers_in_use, compiled_env)?;
                registers_in_use.free_temp(reg);

                let output_reg = registers_in_use.use_temp();

                let ty = expr.eval_type(env)?;
                let (_, func) = &*env.get_unary_op(op, ty.clone())
                    .ok_or(anyhow!("Cannot find unary op: {} {}", op, ty))?;

                let fn_index = compiled_env.0.write().get_or_insert_fn(env.id(), format!("{op}"), vec![ty], func.clone());

                instructions.push(Instruction::Assign {
                    output_register: output_reg,
                    function_index: fn_index,
                    input_registers: vec![reg],
                });

                Ok(output_reg)
            }
            ExprInner::Application(fn_name, exprs) => {
                let inputs = exprs.iter().map(|expr| expr.eval_type(env)).collect::<AnyResult<Vec<_>>>()?;

                let regs = exprs.iter().map(|expr| {
                    expr.compile(env, scope, instructions, registers_in_use, compiled_env)
                }).collect::<AnyResult<Vec<_>>>()?;

                for reg in regs.iter() {
                    registers_in_use.free_temp(*reg);
                }

                let fn_index = match &*(env.get_env_fn(fn_name, inputs.clone()).ok_or(anyhow!("Cannot find function"))?) {
                    EnvironmentFunction::RustImpl { func, .. } | EnvironmentFunction::RustWgsl { func, .. } => {
                        compiled_env.0.write().get_or_insert_fn(env.id(), fn_name.clone(), inputs.clone(), func.clone())
                    },
                    EnvironmentFunction::Ssl(func) => {
                        // Must be placed outside to prevent Rwlock poisoning
                        let fn_index_opt = compiled_env.0.read().get_fn_index(0, func.name.clone(), func.input_types());
                        if let Some(index) = fn_index_opt {
                            index
                        } else {
                            // The dud fn is temporary so recursive functions will properly compile
                            compiled_env.0.write().insert_fn(0, func.name.clone(), func.input_types(), Arc::new(SslCallableFnObj::new(DUD_FUNCTION)));
                            let compiled_fn = func.compile(env, compiled_env)?;
                            compiled_env.0.write().replace_fn(0, func.name.clone(), func.input_types(), Arc::new(compiled_fn))
                                .expect("The dud function should've been replaced by this call")
                        }
                    },
                    EnvironmentFunction::SignatureOnly(_) => bail!("SignatureOnly fns should exist here"),
                };

                let output_reg = registers_in_use.use_temp();

                instructions.push(Instruction::Assign {
                    output_register: output_reg,
                    function_index: fn_index,
                    input_registers: regs,
                });

                Ok(output_reg)
            }
            ExprInner::Field(expr, field_name) => {
                let reg = expr.0.compile(env, scope, instructions, registers_in_use, compiled_env)?;
                registers_in_use.free_temp(reg);
                let output_reg = registers_in_use.use_temp();

                let ty = expr.eval_type(env)?;
                let (_, _, getter, _) = &*env.get_field(field_name, ty.clone())
                    .ok_or(anyhow!("Can't find field"))?;

                let getter_index = compiled_env.0.write().get_or_insert_fn(env.id(), format!("[get]{field_name}"), vec![ty], getter.clone());

                instructions.push(Instruction::Assign {
                    output_register: output_reg,
                    function_index: getter_index,
                    input_registers: vec![reg],
                });

                Ok(output_reg)
            }
            ExprInner::TupleAccess(expr, tuple_index) => {
                let reg = expr.0.compile(env, scope, instructions, registers_in_use, compiled_env)?;
                registers_in_use.free_temp(reg);

                let output_reg = registers_in_use.use_temp();

                instructions.push(Instruction::TupleGet {
                    output_register: output_reg,
                    tuple_register: reg,
                    index: *tuple_index,
                });

                Ok(output_reg)
            }
            ExprInner::Tuple(exprs) => {
                let regs = exprs.iter().map(|expr| {
                    expr.compile(env, scope, instructions, registers_in_use, compiled_env)
                }).collect::<AnyResult<Vec<_>>>()?;

                for reg in regs.iter() {
                    registers_in_use.free_temp(*reg);
                }

                let output_reg = registers_in_use.use_temp();

                instructions.push(Instruction::CreateTuple {
                    element_registers: regs,
                    output_register: output_reg,
                });

                Ok(output_reg)
            }
            ExprInner::Var(name, _) => {
                if let Some((_, lit)) = env.get_env_const(name).as_deref().as_ref() {
                    let index = registers_in_use.use_temp();

                    instructions.push(Instruction::Set {
                        lit: lit.clone(),
                        register: index,
                    });

                    Ok(index)
                } else {
                    scope.get(name)
                        .context(format!("Could not find {} in register scope", name))
                        .cloned().map(|(reg, _)| reg)
                }
            }
            ExprInner::Lit(lit) => {
                let register = registers_in_use.use_temp();

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

#[derive(Clone)]
pub struct CompiledEnv(Arc<RwLock<CompiledEnvInner>>);

impl CompiledEnv {
    pub fn new() -> Self {
        Self(Arc::new(RwLock::new(CompiledEnvInner {
            fn_map: HashMap::new(),
            // const_map: HashMap::new(),
            functions: vec![],
            // consts: vec![],
        })))
    }
}


impl Debug for CompiledEnv {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let inner = self.0.read();
        let mut functions = inner.fn_map.iter().map(|((_, name, inputs), index)| {
            let inputs_string = inputs.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(", ");

            format!("({name}({inputs_string}) -> {}): {index}", inner.functions[*index].output())
        }).collect::<Vec<_>>();

        write!(f, "CompiledEnvInner[\n    {}\n]", functions.join(",\n    "))
    }
}

pub struct CompiledEnvInner {
    fn_map: HashMap<(u32, String, Vec<Type>), usize>, // (env_id, fn_name, inputs) -> fn_index
    functions: Vec<Arc<dyn SslCallableFn>>,
}

// TODO: Make env_id work properly, GLOBAL_ENV being accessible through every env complicates things
impl CompiledEnvInner {
    fn get_fn_index(&self, _env_id: u32, fn_name: String, inputs: Vec<Type>) -> Option<usize> {
        self.fn_map.get(&(0, fn_name, inputs)).cloned()
    }

    /// Inserts the function and returns the index.  If the function already exists, it is inserted again and the old index is overwritten.
    fn insert_fn(&mut self, _env_id: u32, fn_name: String, inputs: Vec<Type>, func: Arc<dyn SslCallableFn>) -> usize {
        let key = (0, fn_name, inputs);

        let index = self.functions.len();
        self.fn_map.insert(key, index);
        self.functions.push(func);
        index
    }

    fn replace_fn(&mut self, _env_id: u32, fn_name: String, inputs: Vec<Type>, func: Arc<dyn SslCallableFn>) -> AnyResult<usize> {
        let key = (0, fn_name, inputs);

        if let Some(index) = self.fn_map.get(&key) {
            self.functions[*index] = func;
            Ok(*index)
        } else {
            Err(anyhow!("Function to replace does not exist"))
        }
    }

    /// If the function is already exists, the index is returned.  Otherwise, the function is inserted.
    fn get_or_insert_fn(&mut self, _env_id: u32, fn_name: String, inputs: Vec<Type>, func: Arc<dyn SslCallableFn>) -> usize {
        let key = (0, fn_name, inputs);

        if let Some(index) = self.fn_map.get(&key) {
            *index
        } else {
            let index = self.functions.len();
            self.fn_map.insert(key, index);
            self.functions.push(func);
            index
        }
    }
}

#[test]
fn test_compilation() {
    let mut script =
        r#"fn abc(x: f32) -> f32 {
            let v = (1, 1, ((0, 0, 0, vec4(1, x, 2, x*3)), 2));
            v.2.0.3.z = 50;
            v.2.0.3.z
        }"#;

    let mut env = Environment::new();

    let doc = parse_document(&script, None, &env).unwrap();
    doc.add_to_environment(&mut env).unwrap();
    let mut function = doc.functions[0].clone();
    function.assign_types(&env).unwrap();
    let compiled_env = CompiledEnv::new();
    let compiled_fn = function.compile(&env, &compiled_env).unwrap();
    println!("CompiledEnv:\n{compiled_env:#?}\nCompiledFn:\n{}", compiled_fn.pretty_print(1));
    let output = compiled_fn.call1(Lit::F32(5.0));

    println!("output: {output}");
}

#[test]
fn try_eval_block_tuple_compl() {
    let env = Environment::new();
    let block = parse_block(r#"{
            let a = (2, 2, vec4(1, 2, 3, 4), (1, (vec4(5, 6, 7, 8), 1)));
            a.0 = 4;
            a.2.z = 100;
            a.3.1.0.w = 2 - 5;
            a.1 = (((a.3).1).0).z;
            a
        }"#, &env).unwrap();

    let a = block.eval(&mut Scope::new(), &env).unwrap();

    println!("a: {a}");
}

#[test]
fn try_eval_compl() {
    let (env, document, structure) = get_test_stuff(0, 1);
    println!("Document: {document}\nStructure: {structure}");

    let compiled_env = CompiledEnv::new();
    let mut assembled_structure = AssembledStructure::new(&document, structure, &env, &compiled_env).unwrap();
    assembled_structure.evaluate_fields(Scope::new()).unwrap();

    // println!("Assembled Structure: {}", prettify_string(format!("{assembled_structure}")));

    let interpret = assembled_structure.eval_method::<Vec4>("proj", 5.0 * Vec4::X + Vec4::Y).unwrap();
    let compiled  = assembled_structure.eval_compiled_method::<Vec4>("proj", 5.0 * Vec4::X + Vec4::Y).unwrap();
    println!("result: {interpret} vs. {compiled}", )
}
