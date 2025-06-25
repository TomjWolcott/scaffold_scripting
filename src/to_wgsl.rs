use anyhow::{anyhow, Context, Result as AnyResult};
use crate::enviroment::Environment;
use crate::parser::*;

const TAB: &'static str = "    ";

pub trait ToWgsl {
    fn to_wgsl(&self, ident_scope: &mut Vec<(String, String)>, env: &Environment) -> AnyResult<String> {
        self.to_wgsl_rec(ident_scope, 0, env)
    }

    fn to_wgsl_rec(&self, ident_scope: &mut Vec<(String, String)>, tabs: usize, env: &Environment) -> AnyResult<String>;
}

impl Method {
    pub fn to_wgsl_with_fn_name(&self, fn_name: impl AsRef<str>, ident_scope: &mut Vec<(String, String)>, env: &Environment) -> AnyResult<String> {
        let mut inputs = Vec::new();

        for Binding(name, ty) in self.inputs.iter() {
            ident_scope.push((name.clone(), name.clone()));

            inputs.push(format!("{}: {}", name.clone(), env.get_wgsl_name(ty).unwrap()));
        }

        Ok(format!(
            "fn {}({}) -> {} {}",
            fn_name.as_ref(),
            inputs.join(", "),
            env.get_wgsl_name(&self.output).with_context(|| format!("Could not find type {} in env", self.output))?,
            self.body.to_wgsl_rec(ident_scope, 0, env)?
        ))
    }
}

impl ToWgsl for Method {
    fn to_wgsl_rec(&self, ident_scope: &mut Vec<(String, String)>, tabs: usize, env: &Environment) -> AnyResult<String> {
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
            self.body.to_wgsl_rec(ident_scope, tabs, env)?
        ))
    }
}

impl ToWgsl for Block {
    fn to_wgsl_rec(&self, ident_scope: &mut Vec<(String, String)>, tabs: usize, env: &Environment) -> AnyResult<String> {
        let mut string = "{\n".to_string();

        for stmt in self.0.iter() {
            string.push_str(format!("{}{}", TAB.repeat(tabs+1), stmt.to_wgsl_rec(ident_scope, tabs + 1, env)?).as_str());
        }

        for expr in &self.1 {
            string.push_str(format!("{}return {};", TAB.repeat(tabs+1), expr.to_wgsl_rec(ident_scope, tabs + 1, env)?).as_str());
        }

        string.push_str(format!("\n{}}}", TAB.repeat(tabs)).as_str());

        Ok(string)
    }
}

impl ToWgsl for Stmt {
    fn to_wgsl_rec(&self, ident_scope: &mut Vec<(String, String)>, tabs: usize, env: &Environment) -> AnyResult<String> {
        match self {
            Stmt::Declare(Binding(var_name, ty), expr) => {
                ident_scope.push((var_name.clone(), var_name.clone()));
                Ok(format!(
                    "var {}: {} = {};\n",
                    var_name,
                    env.get_wgsl_name(ty).with_context(|| format!("Could not find type {} in env", ty))?,
                    expr.to_wgsl_rec(ident_scope, tabs, env)?
                ))
            }
            Stmt::Assign(var_name, expr) => {
                Ok(format!("{} = {};\n", var_name, expr.to_wgsl_rec(ident_scope, tabs, env)?))
            }
            Stmt::IfElse((if_expr, if_block), else_ifs, else_block) => {
                Ok(format!(
                    "if {} {} {} {}",
                    if_expr.to_wgsl_rec(ident_scope, tabs, env)?,
                    if_block.to_wgsl_rec(ident_scope, tabs, env)?,
                    else_ifs.iter().map(|(expr, block)| Ok(format!(
                        "else if {} {}",
                        expr.to_wgsl_rec(ident_scope, tabs, env)?,
                        block.to_wgsl_rec(ident_scope, tabs, env)?,
                    ))).collect::<AnyResult<Vec<_>>>()?.join(" "),
                    else_block.as_ref().map(|block| block.to_wgsl_rec(ident_scope, tabs, env)).unwrap_or(Ok("".to_string()))?
                ))
            }
            Stmt::Expr(expr) => Ok(format!("{};\n", expr.to_wgsl_rec(ident_scope, tabs, env)?)),
            Stmt::Noop => Ok("".to_string())
        }
    }
}

impl ToWgsl for Expr {
    fn to_wgsl_rec(&self, ident_scope: &mut Vec<(String, String)>, tabs: usize, env: &Environment) -> AnyResult<String> {
        match self {
            Expr::BinExpr(expr1, op, expr2) => {
                Ok(format!("({} {op} {})", expr1.to_wgsl_rec(ident_scope, tabs, env)?, expr2.to_wgsl_rec(ident_scope, tabs, env)?))
            }
            Expr::UnaryExpr(op, expr) => {
                Ok(format!("({op}{})", expr.to_wgsl_rec(ident_scope, tabs, env)?))
            }
            Expr::Application(fn_name, args) => {
                let args = args.iter()
                    .map(|arg| arg.to_wgsl_rec(ident_scope, tabs, env))
                    .collect::<AnyResult<Vec<String>>>()?;

                Ok(format!("{}({})", fn_name, args.join(", ")))
            }
            Expr::Dot(_, _, _) => Err(anyhow!("I'm not supporting dot expressions yet")),
            Expr::Field(expr, field) => {
                Ok(format!("{}.{}", expr.to_wgsl_rec(ident_scope, tabs, env)?, field))
            }
            Expr::TupleAccess(_, _) => Err(anyhow!("Tuple access is not supported in WGSL")),
            Expr::Tuple(_) => Err(anyhow!("Tuples are not supported in WGSL")),
            Expr::Var(var_name) => {
                if let Some((_, ident)) = ident_scope.iter().rev().find(|(name, _)| name == var_name) {
                    Ok(ident.clone())
                } else {
                    Ok(var_name.clone())
                }
            }
            Expr::Lit(lit) => {
                lit.to_wgsl_rec(ident_scope, tabs, env)
            }
            Expr::Block(_) => Err(anyhow!("Block expressions are not supported in WGSL")),
        }
    }
}

impl ToWgsl for Lit {
    fn to_wgsl_rec(&self, ident_scope: &mut Vec<(String, String)>, tabs: usize, env: &Environment) -> AnyResult<String> {
        match self {
            Lit::F32(n) => Ok(format!("{:?}", n)),
            Lit::Bool(b) => Ok(format!("{}", b)),
            Lit::Vec4(v) => Ok(format!("vec4{}", &format!("{:?}", v)[4..])),
            Lit::Mat4x4(m) => Ok(format!(
                "mat4x4({},{},{},{})",
                Lit::Vec4(m.x_axis).to_wgsl_rec(ident_scope, tabs, env)?,
                Lit::Vec4(m.y_axis).to_wgsl_rec(ident_scope, tabs, env)?,
                Lit::Vec4(m.z_axis).to_wgsl_rec(ident_scope, tabs, env)?,
                Lit::Vec4(m.w_axis).to_wgsl_rec(ident_scope, tabs, env)?,
            )),
            lit => Err(anyhow!("Unsupported literal: {:?}", lit))
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::assemble::AssembledStructure;
    use super::*;
    use crate::test_helpers::*;

    #[test]
    fn test_to_wgsl() {
        let (env, document, structure) = get_test_stuff(0, 2);
        let assembled_structure = AssembledStructure::new(&document, structure, &env).unwrap();
        let method = assembled_structure.get_method("proj").unwrap();

        assert_eq!(method.to_wgsl(&mut Vec::new(), &env).unwrap(), "fn proj(vector: vec4) -> vec4 {\n    let vector_00004: vec4 = ((5 + (length((vector - __shape2__shift)) - __shape2____shape__radius)) * ((__shape2____shape__radius * normalize((vector - __shape2__shift))) + __shape2__shift));\n    return ((vector_00004 - (dot(vector_00004, __shape1__normal) * __shape1__normal)) * dot(vector, __shape1__normal));\n}");
    }

    #[test]
    fn test_to_wgsl_with_data_arrays() {
        let (env, document, structure) = get_test_stuff(0, 2);
        let assembled_structure = AssembledStructure::new(&document, structure, &env).unwrap();
        let method = assembled_structure.get_method("proj").unwrap();

        let string = method.to_wgsl(&mut vec![
                    ("__shape2__shift".to_string(), "vec4_data_array[1]".to_string()),
                    ("__shape1__normal".to_string(), "vec4_data_array[0]".to_string()),
                    ("__shape2____shape__radius".to_string(), "f32_data_array[1]".to_string()),
                ], &env).unwrap();

        assert_eq!(&string[..], "fn proj(vector: vec4) -> vec4 {\n    var vector_00004: vec4 = ((5.0 + (length((vector - vec4_data_array[1])) - f32_data_array[1])) * ((f32_data_array[1] * normalize((vector - vec4_data_array[1]))) + vec4_data_array[1]));\n    return ((vector_00004 - (dot(vector_00004, vec4_data_array[0]) * vec4_data_array[0])) * dot(vector, vec4_data_array[0]));\n}");
    }
}