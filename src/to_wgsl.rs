use anyhow::{anyhow, Context, Result as AnyResult};
use crate::parser::*;

const TAB: &'static str = "    ";

pub trait ToWgsl {
    fn to_wgsl(&self, ident_scope: &mut Vec<(String, String)>) -> AnyResult<String> {
        self.to_wgsl_rec(ident_scope, 0)
    }

    fn to_wgsl_rec(&self, ident_scope: &mut Vec<(String, String)>, tabs: usize) -> AnyResult<String>;
}

impl ToWgsl for Method {
    fn to_wgsl_rec(&self, ident_scope: &mut Vec<(String, String)>, tabs: usize) -> AnyResult<String> {
        let mut inputs = Vec::new();

        for Binding(name, ty) in self.inputs.iter() {
            ident_scope.push((name.clone(), name.clone()));

            inputs.push(format!("{}: {}", name.clone(), ty.wgsl_type()))
        }

        Ok(format!(
            "{}fn {}({}) -> {} {}",
            TAB.repeat(tabs),
            self.name,
            inputs.join(", "),
            self.output.wgsl_type(),
            self.body.to_wgsl_rec(ident_scope, tabs)?
        ))
    }
}

impl ToWgsl for Block {
    fn to_wgsl_rec(&self, ident_scope: &mut Vec<(String, String)>, tabs: usize) -> AnyResult<String> {
        let mut string = "{\n".to_string();

        for stmt in self.0.iter() {
            string.push_str(format!("{}{}", TAB.repeat(tabs+1), stmt.to_wgsl_rec(ident_scope, tabs + 1)?).as_str());
        }

        for expr in &self.1 {
            string.push_str(format!("{}{}", TAB.repeat(tabs+1), expr.to_wgsl_rec(ident_scope, tabs + 1)?).as_str());
        }

        string.push_str(format!("\n{}}}", TAB.repeat(tabs)).as_str());

        Ok(string)
    }
}

impl ToWgsl for Stmt {
    fn to_wgsl_rec(&self, ident_scope: &mut Vec<(String, String)>, tabs: usize) -> AnyResult<String> {
        match self {
            Stmt::Declare(Binding(var_name, ty), expr) => {
                ident_scope.push((var_name.clone(), var_name.clone()));
                Ok(format!("let {}: {} = {};\n", var_name, ty.wgsl_type(), expr.to_wgsl_rec(ident_scope, tabs)?))
            }
            Stmt::Assign(var_name, expr) => {
                Ok(format!("{} = {};\n", var_name, expr.to_wgsl_rec(ident_scope, tabs)?))
            }
            Stmt::Expr(expr) => Ok(format!("{};\n", expr.to_wgsl_rec(ident_scope, tabs)?)),
            Stmt::Noop => Ok("".to_string())
        }
    }
}

impl ToWgsl for Expr {
    fn to_wgsl_rec(&self, ident_scope: &mut Vec<(String, String)>, tabs: usize) -> AnyResult<String> {
        match self {
            Expr::BinExpr(expr1, op, expr2) => {
                Ok(format!("({} {op} {})", expr1.to_wgsl_rec(ident_scope, tabs)?, expr2.to_wgsl_rec(ident_scope, tabs)?))
            }
            Expr::UnaryExpr(op, expr) => {
                Ok(format!("({op}{})", expr.to_wgsl_rec(ident_scope, tabs)?))
            }
            Expr::Application(fn_name, args) => {
                let args = args.iter()
                    .map(|arg| arg.to_wgsl_rec(ident_scope, tabs))
                    .collect::<AnyResult<Vec<String>>>()?;

                Ok(format!("{}({})", fn_name, args.join(", ")))
            }
            Expr::Dot(_, _, _) => Err(anyhow!("I'm not supporting dot expressions yet")),
            Expr::Var(var_name) => {
                if let Some((_, ident)) = ident_scope.iter().rev().find(|(name, _)| name == var_name) {
                    Ok(ident.clone())
                } else {
                    Ok(var_name.clone())
                }
            }
            Expr::Lit(lit) => {
                Ok(format!("{lit}"))
            }
            Expr::Block(block) => block.to_wgsl_rec(ident_scope, tabs),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::assemble::AssembledStructure;
    use crate::test_helpers;
    use super::*;
    use crate::test_helpers::*;

    #[test]
    fn test_to_wgsl() {
        let (document, structure) = get_test_stuff(0, 2);
        let assembled_structure = AssembledStructure::new(&document, structure).unwrap();
        let method = assembled_structure.get_method("proj").unwrap();

        println!("{}", method.to_wgsl(&mut Vec::new()).unwrap());
    }
}