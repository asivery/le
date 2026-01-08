use std::{collections::HashMap, fmt::Display, mem::discriminant};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Variable(char),
    Constant(bool),
    BinaryOp(BinaryOperation),
    UnaryOp(UnaryOperation),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinaryOperation {
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    Xor(Box<Expr>, Box<Expr>),
    Implies(Box<Expr>, Box<Expr>),
    Equates(Box<Expr>, Box<Expr>),
}

impl BinaryOperation {
    fn split(&self) -> (&Box<Expr>, &Box<Expr>) {
        match self {
            Self::And(a, b) => (a, b),
            Self::Or(a, b) => (a, b),
            Self::Xor(a, b) => (a, b),
            Self::Implies(a, b) => (a, b),
            Self::Equates(a, b) => (a, b),
        }
    }

    fn split_mut(&mut self) -> (&mut Box<Expr>, &mut Box<Expr>) {
        match self {
            Self::And(a, b) => (a, b),
            Self::Or(a, b) => (a, b),
            Self::Xor(a, b) => (a, b),
            Self::Implies(a, b) => (a, b),
            Self::Equates(a, b) => (a, b),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOperation {
    Not(Box<Expr>),
}

impl UnaryOperation {
    fn inner(&self) -> &Box<Expr> {
        match self {
            Self::Not(e) => e,
        }
    }

    fn inner_mut(&mut self) -> &mut Box<Expr> {
        match self {
            Self::Not(e) => e,
        }
    }
}

type ExprMap = HashMap<char, Expr>;

impl Expr {
    pub fn try_match(&self, other: &Expr) -> Option<ExprMap> {
        let mut map = HashMap::new();
        match (self, other) {
            (Expr::Variable(v), other) => {
                map.insert(*v, other.clone());
            }
            (Expr::BinaryOp(self_bin), Expr::BinaryOp(other_bin)) => {
                if discriminant(self_bin) == discriminant(other_bin) {
                    let (s_a, s_b) = self_bin.split();
                    let (o_a, o_b) = other_bin.split();
                    map.extend(s_a.try_match(o_a)?);
                    map.extend(s_b.try_match(o_b)?);
                } else {
                    return None;
                }
            }
            (Expr::Constant(a), Expr::Constant(b)) if a != b => {
                return None;
            }
            (Expr::UnaryOp(self_un), Expr::UnaryOp(other_un)) => {
                if discriminant(self_un) == discriminant(other_un) {
                    let s_u = self_un.inner();
                    let o_u = other_un.inner();
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
            Expr::BinaryOp(e) => {
                let sub = e.split_mut();
                sub.0.substitute(subexprs);
                sub.1.substitute(subexprs);
            }
            Expr::UnaryOp(e) => e.inner_mut().substitute(subexprs),
            _ => {}
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Variable(e) => write!(f, "{e}"),
            Self::Constant(c) => write!(f, "{}", if *c { '1' } else { '0' }),
            Self::UnaryOp(un) => write!(f, "{un}"),
            Self::BinaryOp(bin) => write!(f, "({bin})"),
        }
    }
}

impl Display for UnaryOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Not(e) => write!(f, "¬{e}"),
        }
    }
}

impl Display for BinaryOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::And(a, b) => write!(f, "{a} ∧ {b}"),
            Self::Or(a, b) => write!(f, "{a} ∨ {b}"),
            Self::Xor(a, b) => write!(f, "{a} ⊕ {b}"),
            Self::Implies(a, b) => write!(f, "{a} ⇒ {b}"),
            Self::Equates(a, b) => write!(f, "{a} ⇔ {b}"),
        }
    }
}
