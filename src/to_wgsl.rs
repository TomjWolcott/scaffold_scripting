use anyhow::{anyhow, Context, Result as AnyResult};
use crate::enviroment::Environment;
use crate::interpreter::Eval;
use crate::parser::*;
use crate::scope::Scope;

const TAB: &'static str = "    ";

/// Holds all type info that must be defined in wgsl
pub struct WgslDefinitions {
    /// Tuple types that will be converted to structs in wgsl
    tuples: Vec<Vec<Type>>
}

impl WgslDefinitions {
    pub fn add_tuple_type(&mut self, ty: Vec<Type>) {
        if !self.tuples.iter().any(|other_ty| &ty == other_ty) {
            self.tuples.push(ty);
        }
    }
}

impl WgslDefinitions {
    pub fn new() -> Self {
        WgslDefinitions {
            tuples: Vec::new()
        }
    }

    pub fn merge(&mut self, mut other: Self) {
        self.tuples.retain(|tuple_type| {
            other.tuples.iter().all(|other_tuple_type| tuple_type != other_tuple_type)
        });

        self.tuples.append(&mut other.tuples);
    }

    pub fn to_wgsl_definition_code(&self, env: &Environment) -> AnyResult<String> {
        self.tuples.iter()
            .map(|tuple_type| Ok(format!(
                "struct {} {{{}\n}}\n",
                env.get_wgsl_name(&Type::Tuple(tuple_type.clone())).ok_or(anyhow!("Type not found in {tuple_type:?}"))?,
                tuple_type.iter().enumerate().map(|(i, ty)| {
                    Ok(format!("\n{TAB}item{i}: {}{}", env.get_wgsl_name(ty).ok_or(anyhow!("Type not found: {ty:?}"))?, if i == tuple_type.len() - 1 { "" } else { "," }))
                }).collect::<AnyResult<String>>()?
            )))
            .collect::<AnyResult<String>>()
    }
}

pub struct WgslOutput {
    pub wgsl_code: String,
    pub definitions: WgslDefinitions
}

#[derive(Debug)]
pub enum ToWgslError {
    TypeNotFound(String)
}

pub trait ToWgsl {
    fn to_wgsl(&self, ident_scope: &mut Vec<(String, String)>, env: &Environment) -> AnyResult<WgslOutput> {
        let mut defs = WgslDefinitions::new();

        let wgsl_code = self.to_wgsl_rec(ident_scope, 0, &mut defs, env)?;

        Ok(WgslOutput {
            wgsl_code,
            definitions: defs
        })
    }

    fn to_wgsl_rec(&self, ident_scope: &mut Vec<(String, String)>, tabs: usize, defs: &mut WgslDefinitions, env: &Environment) -> AnyResult<String>;
}

impl Method {
    pub fn to_wgsl_with_fn_name(&self, fn_name: impl AsRef<str>, ident_scope: &mut Vec<(String, String)>, env: &Environment) -> AnyResult<WgslOutput> {
        let mut defs = WgslDefinitions::new();
        let mut inputs = Vec::new();

        for Binding(name, ty) in self.inputs.iter() {
            ident_scope.push((name.clone(), name.clone()));

            inputs.push(format!("{}: {}", name.clone(), env.get_wgsl_name(ty).unwrap()));
        }

        let wgsl_code = format!(
            "fn {}({}) -> {} {}",
            fn_name.as_ref(),
            inputs.join(", "),
            env.get_wgsl_name(&self.output).with_context(|| format!("Could not find type {} in env", self.output))?,
            self.body.to_wgsl_rec(ident_scope, 0, &mut defs, env)?
        );

        Ok(WgslOutput {
            wgsl_code,
            definitions: defs
        })
    }
}

impl ToWgsl for Method {
    fn to_wgsl_rec(&self, ident_scope: &mut Vec<(String, String)>, tabs: usize, defs: &mut WgslDefinitions, env: &Environment) -> AnyResult<String> {
        let mut inputs = Vec::new();

        for Binding(name, ty) in self.inputs.iter() {
            ident_scope.push((name.clone(), name.clone()));

            inputs.push(format!(
                "{}: {}",
                name.clone(),
                env.get_wgsl_name(&ty).with_context(|| format!("Could not find type {} in env", ty))?
            ))
        }

        Ok(format!(
            "{}fn {}({}) -> {} {}",
            TAB.repeat(tabs),
            self.name,
            inputs.join(", "),
            env.get_wgsl_name(&self.output).with_context(|| format!("Could not find type {} in env", self.output))?,
            self.body.to_wgsl_rec(ident_scope, tabs, defs, env)?
        ))
    }
}

impl ToWgsl for Function {
    fn to_wgsl_rec(&self, ident_scope: &mut Vec<(String, String)>, tabs: usize, defs: &mut WgslDefinitions, env: &Environment) -> AnyResult<String> {
        let mut inputs = Vec::new();

        for Binding(name, ty) in self.inputs.iter() {
            ident_scope.push((name.clone(), name.clone()));

            inputs.push(format!(
                "{}: {}",
                name.clone(),
                env.get_wgsl_name(&ty).with_context(|| format!("Could not find type {} in env", ty))?
            ))
        }

        Ok(format!(
            "{}fn {}({}) -> {} {}",
            TAB.repeat(tabs),
            self.name,
            inputs.join(", "),
            env.get_wgsl_name(&self.output).with_context(|| format!("Could not find type {} in env", self.output))?,
            self.body.to_wgsl_rec(ident_scope, tabs, defs, env)?
        ))
    }
}

impl ToWgsl for Block {
    fn to_wgsl_rec(&self, ident_scope: &mut Vec<(String, String)>, tabs: usize, defs: &mut WgslDefinitions, env: &Environment) -> AnyResult<String> {
        let mut string = "{\n".to_string();

        for stmt in self.0.iter() {
            string.push_str(format!("{}{}", TAB.repeat(tabs+1), stmt.to_wgsl_rec(ident_scope, tabs + 1, defs, env)?).as_str());
        }

        for expr in &self.1 {
            string.push_str(format!("{}return {};", TAB.repeat(tabs+1), expr.to_wgsl_rec(ident_scope, tabs + 1, defs, env)?).as_str());
        }

        string.push_str(format!("\n{}}}", TAB.repeat(tabs)).as_str());

        Ok(string)
    }
}

impl ToWgsl for Stmt {
    fn to_wgsl_rec(&self, ident_scope: &mut Vec<(String, String)>, tabs: usize, defs: &mut WgslDefinitions, env: &Environment) -> AnyResult<String> {
        match self {
            Stmt::Declare(lvalue, expr) => {
                let Binding(var_name, ty) = lvalue.get_binding();
                ident_scope.push((var_name.clone(), var_name.clone()));
                Ok(format!(
                    "var {}: {} = {};\n",
                    var_name,
                    // env.get_wgsl_name(ty).with_context(|| format!("Could not find type {} in env", ty))?,
                    env.get_wgsl_name(&ty).with_context(|| format!("Could not find type {} in env", ty))?,
                    expr.to_wgsl_rec(ident_scope, tabs, defs, env)?
                ))
            }
            Stmt::Assign(lvalue, expr) => {
                Ok(format!("{} = {};\n", lvalue.to_wgsl(), expr.to_wgsl_rec(ident_scope, tabs, defs, env)?))
            }
            Stmt::IfElse((if_expr, if_block), else_ifs, else_block) => {
                Ok(format!(
                    "if ({}) {} {} {}",
                    if_expr.to_wgsl_rec(ident_scope, tabs, defs, env)?,
                    if_block.to_wgsl_rec(ident_scope, tabs, defs, env)?,
                    else_ifs.iter().map(|(expr, block)| Ok(format!(
                        "else if ({}) {}",
                        expr.to_wgsl_rec(ident_scope, tabs, defs, env)?,
                        block.to_wgsl_rec(ident_scope, tabs, defs, env)?,
                    ))).collect::<AnyResult<Vec<_>>>()?.join(" "),
                    if let Some(block) = else_block {
                        format!(" else {}", block.to_wgsl_rec(ident_scope, tabs, defs, env)?)
                    } else {
                        "".to_string()
                    }
                ))
            }
            Stmt::Expr(expr) => Ok(format!("{};\n", expr.to_wgsl_rec(ident_scope, tabs, defs, env)?)),
            Stmt::Noop => Ok("".to_string())
        }
    }
}

impl ToWgsl for Expr {
    fn to_wgsl_rec(&self, ident_scope: &mut Vec<(String, String)>, tabs: usize, defs: &mut WgslDefinitions, env: &Environment) -> AnyResult<String> {
        self.0.to_wgsl_rec(ident_scope, tabs, defs, env).context(self.span())
    }
}

impl ToWgsl for ExprInner {
    fn to_wgsl_rec(&self, ident_scope: &mut Vec<(String, String)>, tabs: usize, defs: &mut WgslDefinitions, env: &Environment) -> AnyResult<String> {

        match self {
            ExprInner::BinExpr(expr1, op, expr2) => {
                Ok(format!("({} {op} {})", expr1.to_wgsl_rec(ident_scope, tabs, defs, env)?, expr2.to_wgsl_rec(ident_scope, tabs, defs, env)?))
            }
            ExprInner::UnaryExpr(op, expr) => {
                Ok(format!("({op}{})", expr.to_wgsl_rec(ident_scope, tabs, defs, env)?))
            }
            ExprInner::Application(fn_name, args) => {
                let args = args.iter()
                    .map(|arg| arg.to_wgsl_rec(ident_scope, tabs, defs, env))
                    .collect::<AnyResult<Vec<String>>>()?;

                Ok(format!("{}({})", fn_name, args.join(", ")))
            }
            ExprInner::Dot(_, _, _) => Err(anyhow!("I'm not supporting dot expressions yet")),
            ExprInner::Field(expr, field) => {
                let ty = expr.eval_type(env)?;
                let (_, wgsl_index, _) = &*env.get_field(field, ty.clone())
                    .ok_or(anyhow!("Couldn't find field {} on type {}", field, ty))?;

                let var = expr.to_wgsl_rec(ident_scope, tabs, defs, env)?;

                if let Some(index) = wgsl_index {
                    Ok(format!("{}[{}]", var, index))
                } else {
                    Ok(format!("{}.{}", var, field))
                }
            }
            ExprInner::TupleAccess(expr, index) => {
                Ok(format!("{}.item{}", expr.to_wgsl_rec(ident_scope, tabs, defs, env)?, index))
            },
            ExprInner::Tuple(exprs) => {
                let items = exprs.iter()
                    .map(|item| Ok(item.to_wgsl_rec(ident_scope, tabs, defs, env)?))
                    .collect::<AnyResult<Vec<String>>>()?;

                let ty: Type = ExprInner::Tuple(exprs.clone()).eval_type(env)?;
                let Type::Tuple(tuple_type) = ty.clone() else { unreachable!() };

                defs.add_tuple_type(tuple_type);

                Ok(format!("{}({})", env.get_wgsl_name(&ty).unwrap(), items.join(", ")))
            },
            ExprInner::Var(var_name, _) => {
                if let Some((_, ident)) = ident_scope.iter().rev().find(|(name, _)| name == var_name) {
                    Ok(ident.clone())
                } else {
                    Ok(var_name.clone())
                }
            }
            ExprInner::Lit(lit) => {
                lit.to_wgsl_rec(ident_scope, tabs, defs, env)
            }
            ExprInner::Block(_) => Err(anyhow!("Block expressions are not supported in WGSL")),
        }
    }
}

impl ToWgsl for Lit {
    fn to_wgsl_rec(&self, ident_scope: &mut Vec<(String, String)>, tabs: usize, defs: &mut WgslDefinitions, env: &Environment) -> AnyResult<String> {
        match self {
            Lit::F32(n) => Ok(format!("{:?}", n)),
            Lit::Bool(b) => Ok(format!("{}", b)),
            Lit::Vec4(v) => Ok(format!("vec4{}", &format!("{:?}", v)[4..])),
            Lit::Mat4x4(m) => Ok(format!(
                "mat4x4({},{},{},{})",
                Lit::Vec4(m.x_axis).to_wgsl_rec(ident_scope, tabs, defs, env)?,
                Lit::Vec4(m.y_axis).to_wgsl_rec(ident_scope, tabs, defs, env)?,
                Lit::Vec4(m.z_axis).to_wgsl_rec(ident_scope, tabs, defs, env)?,
                Lit::Vec4(m.w_axis).to_wgsl_rec(ident_scope, tabs, defs, env)?,
            )),
            Lit::Tuple(lits) => {
                let items = lits.iter()
                    .map(|item| Ok(item.to_wgsl_rec(ident_scope, tabs, defs, env)?))
                    .collect::<AnyResult<Vec<String>>>()?;

                let ty: Type = ExprInner::Lit(Lit::Tuple(lits.clone())).eval_type(env)?;
                let Type::Tuple(tuple_type) = ty.clone() else { unreachable!() };
                defs.add_tuple_type(tuple_type);

                Ok(format!("{}({})", env.get_wgsl_name(&ty).unwrap(), items.join(", ")))
            }
            lit => Err(anyhow!("Unsupported literal: {:?}", lit))
        }
    }
}

impl Lvalue {
    fn to_wgsl(&self) -> String {
        let var_name = self.get_var_name();
        let fields_string = if let Lvalue::Fields(_, fields) = self {
            fields.iter().map(|field| match field {
                LvalueField::Field(field) => format!(".{}", field),
                LvalueField::TupleAccess(i) => format!(".item{}", i)
            }).collect::<String>()
        } else {
            String::new()
        };

        format!("{var_name}{fields_string}")
    }
}

#[cfg(test)]
mod tests {
    use crate::assemble::AssembledStructure;
    use crate::ast_operations::AssignTypes;
    use crate::interpreter::Eval;
    use crate::scope::Scope;
    use super::*;
    use crate::test_helpers::*;

    #[test]
    fn play_with_to_wgsl() {
        let env = Environment::new();
        let script = r#"{
            let (a, d) = (1.0, 2.0);
            let b = (a, (1 + 3, -.1 + 2), 3 * mat4x4(X, Z, Y, W));
            let z = 4;
            if (true) {
                z = 3;
            } else if (false) {
                z = 2;
            } else {
                z = 1;
            }
            b.1.1 = 2.43;
            let c = (z, vec4(1, 3, 2, b.1.1) / 8, 3.0, 4.0);
            b
        }"#;

        let mut block = parse_block(script, &env).unwrap();
        block.assign_types(&env);
        let string = prettify_string(format!("{block}"));

        println!("{}\nwhich returns: {:?}", string, block.eval(&mut Scope::new(), &env));

        let output = block.to_wgsl(&mut Vec::new(), &env).unwrap();
        println!("WGSL DEF:\n{}\n\nCODE:\n{}", output.definitions.to_wgsl_definition_code(&env).unwrap(), output.wgsl_code);
    }

    #[test]
    fn test_to_wgsl() {
        let (env, document, structure) = get_test_stuff(0, 2);
        let assembled_structure = AssembledStructure::new(&document, structure, &env).unwrap();
        let method = assembled_structure.get_method("proj").unwrap();

        assert_eq!(method.to_wgsl(&mut Vec::new(), &env).unwrap().wgsl_code, "fn proj(vector: vec4) -> vec4 {\n    let vector_00004: vec4 = ((5 + (length((vector - __shape2__shift)) - __shape2____shape__radius)) * ((__shape2____shape__radius * normalize((vector - __shape2__shift))) + __shape2__shift));\n    return ((vector_00004 - (dot(vector_00004, __shape1__normal) * __shape1__normal)) * dot(vector, __shape1__normal));\n}");
    }

    #[test]
    fn test_to_wgsl_with_data_arrays() {
        let (env, document, structure) = get_test_stuff(0, 2);
        let assembled_structure = AssembledStructure::new(&document, structure, &env).unwrap();
        let method = assembled_structure.get_method("proj").unwrap();

        let string = method.to_wgsl(&mut vec![
                    ("_shape2__shift".to_string(), "vec4_data_array[1]".to_string()),
                    ("_shape1__normal".to_string(), "vec4_data_array[0]".to_string()),
                    ("_shape2___shape__radius".to_string(), "f32_data_array[1]".to_string()),
                ], &env).unwrap();

        assert_eq!(&string.wgsl_code[..], "fn proj(vector: vec4<f32>) -> vec4<f32> {\n    var vector_00004: vec4<f32> = ((5.0 + (length((vector - vec4_data_array[1])) - f32_data_array[1])) * ((f32_data_array[1] * normalize((vector - vec4_data_array[1]))) + vec4_data_array[1]));\n    return ((vector_00004 - (dot(vector_00004, vec4_data_array[0]) * vec4_data_array[0])) * dot(vector, vec4_data_array[0]));\n}");
    }
}