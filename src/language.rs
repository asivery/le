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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOperation {
    Not(Box<Expr>),
}
