use std::sync::atomic::{AtomicUsize, Ordering};
use anyhow::{anyhow, Context, Result as AnyResult};
use crate::enviroment::Environment;
use crate::interpreter::Eval;
use crate::parser::*;
use crate::scope::Scope;
use crate::test_helpers::prettify_string;
use crate::tree_walk::{Options, RecOrdering, TreeNodeMut, WalkTreeMut};

pub trait AssignTypes {
    fn assign_types(&mut self, env: &Environment) {
        self.assign_types_rec(&mut Scope::new(), env);
    }

    fn assign_types_rec(&mut self, scope: &mut Scope<Type>, env: &Environment) -> AnyResult<Type>;
}

impl AssignTypes for Method {
    fn assign_types_rec(&mut self, scope: &mut Scope<Type>, env: &Environment) -> AnyResult<Type> {
        let scope_size = scope.size();

        for Binding(name, ty) in self.inputs.iter() {
            scope.push(name.clone(), ty.clone());
        }

        self.body.assign_types_rec(scope, env)?;

        scope.resize(scope_size);

        Ok(self.output.clone())
    }
}

impl AssignTypes for Function {
    fn assign_types_rec(&mut self, _: &mut Scope<Type>, env: &Environment) -> AnyResult<Type> {
        let mut new_scope = (&self.inputs).into();

        self.body.assign_types_rec(&mut new_scope, env)?;

        Ok(self.output.clone())
    }
}

impl AssignTypes for Block {
    fn assign_types_rec(&mut self, scope: &mut Scope<Type>, env: &Environment) -> AnyResult<Type> {
        let scope_size = scope.size();

        for stmt in self.0.iter_mut() {
            stmt.assign_types_rec(scope, env)?;
        }

        let return_value = if let Some(expr) = &mut self.1 {
            Ok(expr.assign_types_rec(scope, env)?)
        } else {
            Ok(Type::Unit)
        };

        scope.resize(scope_size);

        return_value
    }
}

impl AssignTypes for Stmt {
    fn assign_types_rec(&mut self, scope: &mut Scope<Type>, env: &Environment) -> AnyResult<Type> {
        match self {
            Stmt::Declare(Binding(var, ty), expr) => {
                let new_ty = expr.assign_types_rec(scope, env)?;
                scope.push(var.clone(), new_ty.clone());
                println!("Decl | ty: {ty}, expr: {new_ty}");
                *ty = new_ty;
            }
            Stmt::Assign(_, expr) => {
                expr.assign_types_rec(&mut scope.clone(), env)?;
            }
            Stmt::IfElse((if_expr, if_block), else_ifs, else_block) => {
                if_expr.assign_types_rec(&mut scope.clone(), env)?;
                if_block.assign_types_rec(&mut scope.clone(), env)?;

                for (expr, block) in else_ifs.iter_mut() {
                    expr.assign_types_rec(&mut scope.clone(), env)?;
                    block.assign_types_rec(&mut scope.clone(), env)?;
                }

                if let Some(block) = else_block {
                    block.assign_types_rec(&mut scope.clone(), env)?;
                }
            }
            Stmt::Expr(expr) => {
                expr.assign_types_rec(&mut scope.clone(), env)?;
            }
            Stmt::Noop => {}
        }

        Ok(Type::Unit)
    }
}

impl AssignTypes for Expr {
    fn assign_types_rec(&mut self, scope: &mut Scope<Type>, env: &Environment) -> AnyResult<Type> {
        match self {
            Expr::BinExpr(left, symbol, right) => {
                let (left, sym, right) = (left.assign_types_rec(scope, env)?, symbol.as_str(), right.assign_types_rec(scope, env)?);

                if sym == "==" || sym == "!=" {
                    return Ok(Type::Bool);
                }

                let (_, op) = &*env.get_binary_op(sym, left.clone(), right.clone())
                    .ok_or(anyhow!(
                        "Could not find binary operation with signature {} {} {}",
                        left, symbol, right
                    ))?;

                Ok(op.output(env))
            }
            Expr::UnaryExpr(symbol, right) => {
                let (sym, right) = (symbol.as_str(), right.assign_types_rec(scope, env)?);

                let (_, op) = &*env.get_unary_op(sym, right.clone())
                    .ok_or(anyhow!(
                        "Could not find unary operation with signature {} {}",
                        symbol, right
                    ))?;

                Ok(op.output(env))
            }
            Expr::Application(fn_name, args) => {
                let (input_types) = args.iter_mut()
                    .map(|arg| arg.assign_types_rec(scope, env))
                    .collect::<AnyResult<Vec<_>>>()?;

                if let ("select", [x_false, x_true, Type::Bool]) = (fn_name.as_str(), &input_types[..]) {
                    return if x_false != x_true {
                        Err(anyhow!("Types do not match in select for {} and {}", x_false, x_true))
                    } else {
                        Ok(x_false.clone())
                    };
                }

                let (_, function) = &*env.get_fn(&fn_name, input_types.clone())
                    .ok_or(anyhow!(
                        "Could not find signature {fn_name}({})",
                        input_types.iter().map(|ty| ty.to_string()).collect::<Vec<_>>().join(", ")
                    ))?;

                Ok(function.output(env))
            },
            Expr::Dot(_, _, _) => Err(anyhow!("EVAL NOT SUPPORTED FOR DOT")),
            Expr::Field(expr, field_name) => {
                let ty = expr.assign_types_rec(scope, env)?;

                let (_, _, get_field) = &*env.get_field(&field_name, ty.clone())
                    .ok_or(anyhow!("Field {field_name} not found in {ty}"))?;

                Ok(get_field.output(env))
            }
            Expr::TupleAccess(expr, index) => match expr.assign_types_rec(scope, env)? {
                Type::Tuple(fields) => {
                    let index = *index as usize;
                    if index < fields.len() {
                        Ok(fields[index].clone())
                    } else {
                        Err(anyhow!("Index out of bounds for tuple, tried to access index {index} in a {}-tuple", fields.len()))
                    }
                },
                ty => Err(anyhow!("Tuple access not supported for {}", ty))
            }
            Expr::Tuple(elements) => {
                Ok(Type::Tuple(elements.iter_mut().map(|expr| expr.assign_types_rec(scope, env)).collect::<AnyResult<Vec<_>>>()?))
            },
            Expr::Var(var, ty) => {
                if let Some(value) = scope.get(&var) {
                    return Ok(value.clone());
                }

                let (_, value) = &*env.get_const(&var)
                    .ok_or(anyhow!("var {var} not found in scope"))?;

                *ty = value.get_type();

                Ok(value.get_type())
            },
            Expr::Lit(lit) => Ok(lit.get_type()),
            Expr::Block(block) => block.assign_types_rec(scope, env),
        }
    }
}

//   I know I'm doing A LOT of cloning by using Vec, but the scope won't ever really get that big,
// so I'll put up with it for right now.
#[derive(Clone)]
pub struct IdentScope(pub Vec<(String, String)>);

impl IdentScope {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn push(&mut self, old_name: String, new_name: String) {
        self.0.push((old_name, new_name));
    }

    pub fn get(&self, old_name: impl AsRef<str>) -> Option<&String> {
        self.0.iter().rev()
            .find(|(other_old_name, _)| other_old_name.as_str() == old_name.as_ref())
            .map(|(_, new_name)| new_name)
    }
}

fn gen_ident(str: impl AsRef<str>) -> String {
    static COUNTER: AtomicUsize = AtomicUsize::new(0);

    format!("{}_{:05}", str.as_ref(), COUNTER.fetch_add(1, Ordering::Relaxed))
}

pub trait AlphaConvert {
    fn alpha_convert(&mut self, scope: &mut IdentScope);
}

impl AlphaConvert for Block {
    fn alpha_convert(&mut self, scope: &mut IdentScope) {
        for stmt in self.0.iter_mut() {
            stmt.alpha_convert(scope);
        }

        if let Some(expr) = &mut self.1 {
            expr.alpha_convert(scope);
        }
    }
}

impl AlphaConvert for Stmt {
    fn alpha_convert(&mut self, scope: &mut IdentScope) {
        match self {
            Stmt::Declare(Binding(var, _), expr) => {
                expr.alpha_convert(&mut scope.clone());
                let new_var = gen_ident(&var);
                scope.push(var.clone(), new_var.clone());

                *var = new_var;
            }
            Stmt::Assign(var, expr) => {
                expr.alpha_convert(&mut scope.clone());

                if let Some(new_var) = scope.get(&var) {
                    *var = new_var.clone();
                }
            }
            Stmt::IfElse((if_expr, if_block), else_ifs, else_block) => {
                if_expr.alpha_convert(&mut scope.clone());
                if_block.alpha_convert(&mut scope.clone());

                for (expr, block) in else_ifs.iter_mut() {
                    expr.alpha_convert(&mut scope.clone());
                    block.alpha_convert(&mut scope.clone());
                }

                if let Some(block) = else_block {
                    block.alpha_convert(&mut scope.clone());
                }
            }
            Stmt::Expr(expr) => {
                expr.alpha_convert(&mut scope.clone());
            }
            Stmt::Noop => {}
        }
    }
}

impl AlphaConvert for Expr {
    fn alpha_convert(&mut self, scope: &mut IdentScope) {
        match self {
            Expr::BinExpr(expr1, _, expr2) => {
                expr1.alpha_convert(&mut scope.clone());
                expr2.alpha_convert(&mut scope.clone());
            }
            Expr::UnaryExpr(_, expr) => {
                expr.alpha_convert(&mut scope.clone());
            }
            Expr::Application(_, exprs) => {
                for expr in exprs.iter_mut() {
                    expr.alpha_convert(&mut scope.clone());
                }
            }
            Expr::Dot(var, _, exprs) => {
                if let Some(new_var) = scope.get(&var) {
                    *var = new_var.clone();
                }

                for expr in exprs.iter_mut() {
                    expr.alpha_convert(&mut scope.clone());
                }
            }
            Expr::Field(expr, _) => {
                expr.alpha_convert(&mut scope.clone());
            }
            Expr::TupleAccess(expr, _) => {
                expr.alpha_convert(&mut scope.clone());
            }
            Expr::Tuple(exprs) => {
                for expr in exprs.iter_mut() {
                    expr.alpha_convert(&mut scope.clone());
                }
            }
            Expr::Var(var, _) => {
                if let Some(new_var) = scope.get(&var) {
                    *var = new_var.clone();
                }
            }
            Expr::Lit(_) => {}
            Expr::Block(block) => {
                block.alpha_convert(&mut scope.clone());
            }
        }
    }
}

impl Block {
    /// Assumes everything has already been alpha converted and types to be assigned
    pub fn inline_blocks(&mut self, env: &Environment) -> AnyResult<()> {
        let mut i = 0;

        if let Some(expr) = self.1.take() {
            let ty = expr.eval_type(env)?;
            let return_expr = gen_ident("return_expr");
            self.0.push(Stmt::Declare(Binding(return_expr.clone(), ty.clone()), expr));

            self.1 = Some(Expr::Var(return_expr, ty))
        }

        while i < self.0.len() {
            let mut do_increment = true;

            let exprs = match &mut self.0[i] {
                Stmt::Declare(_, expr) |
                Stmt::Assign(_, expr) |
                Stmt::Expr(expr) => {
                    vec![expr]
                }
                Stmt::IfElse((if_expr, if_block), else_ifs, else_block) => {
                    if_block.inline_blocks(env)?;

                    let mut exprs = else_ifs.iter_mut().map(|(expr, block)| {
                        block.inline_blocks(env)?;

                        Ok(expr)
                    }).collect::<AnyResult<Vec<_>>>()?;

                    exprs.insert(0, if_expr);

                    else_block.as_mut().map(|block| block.inline_blocks(env));

                    exprs
                }
                Stmt::Noop => Vec::new()
            };

            let mut promoted_stmts = Vec::new();

            for expr in exprs.into_iter().rev() {
                let blocks = expr.promote_blocks(env)?;
                do_increment = blocks.len() == 0;

                for (Block(mut stmts, expr_opt), new_var) in blocks.into_iter().rev() {
                    if let Some(expr) = expr_opt {
                        stmts.push(Stmt::Declare(Binding(new_var, Type::Auto), expr));
                    }

                    promoted_stmts.append(&mut stmts);
                }
            }

            self.0.splice(i..i, promoted_stmts);

            if do_increment { i += 1 };
        }

        Ok(())
    }
}

impl Expr {
    pub fn promote_blocks(&mut self, env: &Environment) -> AnyResult<Vec<(Block, String)>> {
        Ok(match self {
            Expr::BinExpr(expr1, _, expr2) => {
                vec![expr1.promote_blocks(env)?, expr2.promote_blocks(env)?].into_iter().flatten().collect()
            }
            Expr::Field(expr, _) |
            Expr::TupleAccess(expr, _) |
            Expr::UnaryExpr(_, expr) => {
                expr.promote_blocks(env)?
            }
            Expr::Tuple(exprs) |
            Expr::Application(_, exprs) |
            Expr::Dot(_, _, exprs) => {
                exprs.iter_mut().map(|expr| {
                    expr.promote_blocks(env)
                }).collect::<AnyResult<Vec<_>>>()?.into_iter().flatten().collect()
            },
            Expr::Var(_, _) => vec![],
            Expr::Lit(_) => vec![],
            Expr::Block(block) => {
                let ty = block.eval_type(env)?;

                let new_var = gen_ident("block");
                let Expr::Block(block) = std::mem::replace(self, Expr::Var(new_var.clone(), ty)) else { unreachable!() };

                vec![(*block, new_var)]
            }
        })
    }
}

impl Block {
    /// Requires types to have been assigned
    pub fn cull_single_use_vars(&mut self) {
        let mut deletable_vars = Vec::new();

        let _: Result<(), ()> = self.walk_tree_mut(&mut |node| {
            match &node {
                TreeNodeMut::Stmt(stmt) => match stmt {
                    Stmt::Declare(Binding(var_name, _), _) => {
                        deletable_vars.push((var_name.clone(), 0));
                    }
                    Stmt::Assign(var_name, _) => {
                        deletable_vars.retain(|(other_var_name, _)| var_name != other_var_name)
                    }
                    _ => {}
                }
                TreeNodeMut::Expr(expr) => match expr {
                    Expr::Dot(var_name, _, _) |
                    Expr::Var(var_name, _) => {
                        deletable_vars.retain_mut(|(other_var_name, num_usages)| {
                            if other_var_name == var_name {
                                *num_usages += 1;
                            }

                            *num_usages <= 1
                        });
                    }
                    _ => {}
                }
                _ => {}
            };

            Ok(())
        });

        let mut var_replacements = Vec::new();

        let _: Result<(), ()> = self.walk_tree_mut_with_options(Options {
            ordering: RecOrdering::Postorder, ..Default::default()
        }, &mut |node| {
            match node {
                TreeNodeMut::Stmt(stmt) => match stmt {
                    Stmt::Declare(Binding(var_name, _), _) => {
                        if deletable_vars.contains(&(var_name.clone(), 0)) {
                            *stmt = Stmt::Noop
                        } else if deletable_vars.contains(&(var_name.clone(), 1)) {
                            let Stmt::Declare(
                                Binding(var_name, _),
                                expr
                            ) = std::mem::replace(stmt, Stmt::Noop) else { unreachable!() };
                            var_replacements.push((var_name, expr))
                        }
                    }
                    _ => {}
                }
                TreeNodeMut::Expr(expr) => match expr {
                    Expr::Var(var_name, _) => {
                        if let Some(index) = var_replacements.iter().position(
                            |(other_var_name, _)| var_name == other_var_name
                        ) {
                            let (_, replacement_expr) = var_replacements.remove(index);

                            *expr = replacement_expr;
                        }
                    }
                    _ => {}
                }
                _ => {}
            }

            Ok(())
        });
    }

    pub fn cull_noops(&mut self) {
        let _: Result<(), ()> = self.walk_tree_mut(&mut |node| {
            if let TreeNodeMut::Block(block) = node {
                block.0.retain_mut(|stmt| *stmt != Stmt::Noop);
            }

            Ok(())
        });
    }
}

#[test]
fn try_out_ops() {
    let env = Environment::new();

    let mut block = parse_block(r#"{
        let x: f32 = 4;
        let y: Vec4 = 2 * {
            let x: Vec4 = x * vector;
            x = 0.5 * x;
            let y: f32 = { let a: f32 = 2; a + 4 } / { let x: f32 = 8; x = 3; x + 2 };
            y * x
        };
        let x: Vec4 = x * y;
        (x + y, 5)
    }"#, &env).unwrap();

    let before = prettify_string(format!("{}", block.clone()));

    block.alpha_convert(&mut IdentScope::new());

    let after_alpha = prettify_string(format!("{}", block.clone()));

    block.inline_blocks(&env).unwrap();

    let after_inline = prettify_string(format!("{}", block.clone()));

    block.cull_single_use_vars();

    let after_cull = prettify_string(format!("{}", block.clone()));

    println!("Before:\n{}\n\nAfter Alpha:\n{}\n\nAfter Inline:\n{}\n\nAfter Cull:\n{}", before, after_alpha, after_inline, after_cull);
}