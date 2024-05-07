use std::fmt::Display;
use crate::parser::*;

pub trait WalkTreeMut<E> {
    fn walk_tree_mut(&mut self, func: &mut impl for<'a> FnMut(TreeNodeMut<'a>) -> Result<(), E>) -> Result<(), E>;
}

impl<E> WalkTreeMut<E> for Stmt {
    fn walk_tree_mut(&mut self, func: &mut impl for<'a> FnMut(TreeNodeMut<'a>) -> Result<(), E>) -> Result<(), E> {
        func(TreeNodeMut::Stmt(self))?;

        match self {
            Stmt::Declare(_, expr) => expr.walk_tree_mut(func)?,
            Stmt::Assign(_, expr) => expr.walk_tree_mut(func)?,
            Stmt::Expr(expr) => expr.walk_tree_mut(func)?,
            Stmt::Noop => {}
        };

        Ok(())
    }
}

impl<E> WalkTreeMut<E> for Expr {
    fn walk_tree_mut(&mut self, func: &mut impl for<'a> FnMut(TreeNodeMut<'a>) -> Result<(), E>) -> Result<(), E> {
        func(TreeNodeMut::Expr(self))?;

        match self {
            Expr::BinExpr(lhs, _, rhs) => {
                lhs.walk_tree_mut(func)?;
                rhs.walk_tree_mut(func)?;
            },
            Expr::UnaryExpr(_, expr) => expr.walk_tree_mut(func)?,
            Expr::Application(_, args) => {
                for arg in args.iter_mut() {
                    arg.walk_tree_mut(func)?;
                }
            },
            Expr::Dot(_, _, args) => {
                for arg in args.iter_mut() {
                    arg.walk_tree_mut(func)?;
                }
            },
            Expr::Var(_) => {},
            Expr::Lit(_) => {},
            Expr::Block(block) => block.walk_tree_mut(func)?
        }

        Ok(())
    }
}

impl<E> WalkTreeMut<E> for Block {
    fn walk_tree_mut(&mut self, func: &mut impl for<'a> FnMut(TreeNodeMut<'a>) -> Result<(), E>) -> Result<(), E> {
        func(TreeNodeMut::Block(self))?;

        for stmt in self.0.iter_mut() {
            stmt.walk_tree_mut(func)?;
        }

        if let Some(expr) = &mut self.1 {
            expr.walk_tree_mut(func)?;
        }

        Ok(())
    }
}


#[derive(Debug)]
pub enum TreeNodeMut<'a> {
    Stmt(&'a mut Stmt),
    Expr(&'a mut Expr),
    Block(&'a mut Block)
}

impl Display for TreeNodeMut<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TreeNodeMut::Stmt(stmt) => write!(f, "{}", stmt),
            TreeNodeMut::Expr(expr) => write!(f, "{}", expr),
            TreeNodeMut::Block(block) => write!(f, "{}", block)
        }
    }
}

#[test]
fn walker() {
    let doc_str = r#"
        class Sphere4D {
            radius: f32,
            Proj::proj(vector: Vec4) -> Vec4 {
                radius * normalize(vector)
            },
            Sdf::sdf(vector: Vec4) -> f32 {
                length(vector) - radius
            }
        }

        class Plane4D {
            normal: Vec4,
            Proj::proj(vector: Vec4) -> Vec4 {
                vector - dot(vector, normal) * normal
            },
            Sdf::sdf(vector: Vec4) -> f32 {
                dot(vector, normal)
            }
        }

        class Shift {
            shift: Vec4,
            shape: Class,
            Proj::proj<shape: Proj>(vector: Vec4) -> Vec4 {
                shape.proj(vector - shift) + shift
            },
            Sdf::sdf<shape: Sdf>(vector: Vec4) -> f32 {
                shape.sdf(vector - shift)
            }
        }

        class Union {
            shape1: Class,
            shape2: Class,
            Proj::proj<shape1: Proj + Sdf, shape2: Proj + Sdf>(vector: Vec4) -> Vec4 {
                abc
            }
        }
    "#.to_string();

    let mut document = parse_document(&doc_str).unwrap();

    let method = document.get_method_mut("Plane4D", &MethodKey::new(Some("Proj"), "proj")).unwrap();

    let _ = method.body.walk_tree_mut(&mut |node| {println!("{node}"); Ok::<(), ()>(())});
}