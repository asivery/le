use std::{collections::HashMap, fmt::Display};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Variable(char),
    Constant(bool),
    BinaryOp(BinaryOperation, Box<Expr>, Box<Expr>),
    UnaryOp(UnaryOperation, Box<Expr>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinaryOperation {
    And,
    Or,
    Xor,
    Implies,
    Equates,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOperation {
    Not,
}

type ExprMap = HashMap<char, Expr>;

impl Expr {
    pub fn try_match(&self, other: &Expr) -> Option<ExprMap> {
        let mut map = HashMap::new();
        match (self, other) {
            (Expr::Variable(v), other) => {
                map.insert(*v, other.clone());
            }
            (Expr::BinaryOp(self_bin, s_a, s_b), Expr::BinaryOp(other_bin, o_a, o_b)) => {
                if self_bin == other_bin {
                    map.extend(s_a.try_match(o_a)?);
                    map.extend(s_b.try_match(o_b)?);
                } else {
                    return None;
                }
            }
            (Expr::Constant(a), Expr::Constant(b)) if a != b => {
                return None;
            }
            (Expr::UnaryOp(self_un, s_u), Expr::UnaryOp(other_un, o_u)) => {
                if self_un == other_un {
                    map.extend(s_u.try_match(o_u)?);
                } else {
                    return None;
                }
            }
            (_, _) => return None,
        };
        Some(map)
    }

    pub fn substitute(&mut self, subexprs: &ExprMap) {
        match self {
            Expr::Variable(e) if subexprs.contains_key(e) => {
                *self = subexprs.get(e).unwrap().clone()
            }
            Expr::BinaryOp(_, a, b) => {
                a.substitute(subexprs);
                b.substitute(subexprs);
            }
            Expr::UnaryOp(_, e) => e.substitute(subexprs),
            _ => {}
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Variable(e) => write!(f, "{e}"),
            Self::Constant(c) => write!(f, "{}", if *c { '1' } else { '0' }),
            Self::UnaryOp(un, a) => write!(f, "{un}{a}"),
            Self::BinaryOp(bin, a, b) => write!(f, "({a} {bin} {b})"),
        }
    }
}

impl Display for UnaryOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Not => write!(f, "¬"),
        }
    }
}

impl Display for BinaryOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::And => write!(f, "∧"),
            Self::Or => write!(f, "∨"),
            Self::Xor => write!(f, "⊕"),
            Self::Implies => write!(f, "⇒"),
            Self::Equates => write!(f, "⇔"),
        }
    }
}
