use std::sync::atomic::{AtomicUsize, Ordering};
use crate::parser::*;
use crate::test_helpers::prettify_string;
use crate::tree_walk::{Options, RecOrdering, TreeNodeMut, WalkTreeMut};

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
            Expr::Var(var) => {
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
    /// Assumes everything has already been alpha converted
    pub fn inline_blocks(&mut self) {
        let mut i = 0;

        if let Some(expr) = self.1.take() {
            let return_expr = gen_ident("return_expr");
            self.0.push(Stmt::Declare(Binding(return_expr.clone(), Type::Auto), expr));

            self.1 = Some(Expr::Var(return_expr))
        }

        while i < self.0.len() {
            let mut do_increment = true;

            match &mut self.0[i] {
                Stmt::Declare(_, expr) |
                Stmt::Assign(_, expr) |
                Stmt::Expr(expr) => {
                    let blocks = expr.promote_blocks();
                    do_increment = blocks.len() == 0;

                    for (Block(stmts, expr_opt), new_var) in blocks {
                        if let Some(expr) = expr_opt {
                            self.0.insert(i, Stmt::Declare(Binding(new_var, Type::Auto), expr))
                        }

                        self.0.splice(i..i, stmts);
                    }
                }
                Stmt::Noop => {}
            }

            if do_increment { i += 1 };
        }
    }
}

impl Expr {
    pub fn promote_blocks(&mut self) -> Vec<(Block, String)> {
        match self {
            Expr::BinExpr(expr1, _, expr2) => {
                vec![expr1.promote_blocks(), expr2.promote_blocks()].into_iter().flatten().collect()
            }
            Expr::Field(expr, _) |
            Expr::TupleAccess(expr, _) |
            Expr::UnaryExpr(_, expr) => {
                expr.promote_blocks()
            }
            Expr::Tuple(exprs) |
            Expr::Application(_, exprs) |
            Expr::Dot(_, _, exprs) => {
                exprs.iter_mut().map(|expr| {
                    expr.promote_blocks()
                }).flatten().collect()
            },
            Expr::Var(_) => vec![],
            Expr::Lit(_) => vec![],
            Expr::Block(_) => {
                let new_var = gen_ident("block");
                let Expr::Block(block) = std::mem::replace(self, Expr::Var(new_var.clone())) else { unreachable!() };

                vec![(*block, new_var)]
            }
        }
    }
}

impl Block {
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
                    Expr::Var(var_name) => {
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
                    Expr::Var(var_name) => {
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
    }"#).unwrap();

    let before = prettify_string(format!("{}", block.clone()));

    block.alpha_convert(&mut IdentScope::new());

    let after_alpha = prettify_string(format!("{}", block.clone()));

    block.inline_blocks();

    let after_inline = prettify_string(format!("{}", block.clone()));

    block.cull_single_use_vars();

    let after_cull = prettify_string(format!("{}", block.clone()));

    println!("Before:\n{}\n\nAfter Alpha:\n{}\n\nAfter Inline:\n{}\n\nAfter Cull:\n{}", before, after_alpha, after_inline, after_cull);
}