use std::{collections::HashSet, fmt::Display, mem::take};

use crate::{
    grammar::ExprParser,
    language::{BinaryOperation, Expr, UnaryOperation},
};

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum MultiOperandOperationType {
    And,
    Or,
}

#[derive(Debug, Clone)]
pub struct MultiOperandOperationValue {
    inverted: bool,
    var: char,
}

#[derive(Debug, Clone)]
pub enum MultiOperandOperation {
    Operation {
        operation: MultiOperandOperationType,
        values: Vec<MultiOperandOperation>,
    },
    Value(MultiOperandOperationValue),
}

impl Default for MultiOperandOperation {
    fn default() -> Self {
        Self::Value(MultiOperandOperationValue {
            inverted: false,
            var: '?',
        })
    }
}

struct ResolutionRuleset {
    rules: Vec<(Expr, Expr)>,
}

impl ResolutionRuleset {
    fn build(rules: &[(&str, &str)]) -> Self {
        let parser = ExprParser::new();
        let mut temp = HashSet::new();
        Self {
            rules: rules
                .iter()
                .map(|&(a, b)| {
                    (
                        *parser.parse(&mut temp, a).unwrap(),
                        *parser.parse(&mut temp, b).unwrap(),
                    )
                })
                .collect(),
        }
    }

    fn apply(&self, expr: &mut Expr) {
        // Check if expr matches. If so, translate. Then recurse.
        for (pattern, output) in &self.rules {
            if let Some(results) = pattern.try_match(expr) {
                println!("Applying rule <{pattern} ===> {output}> to {expr}");
                *expr = output.clone();
                expr.substitute(&results);
                // Recurse in case this also matches that rule.
                self.apply(expr);
            }
        }

        match expr {
            Expr::BinaryOp(_, a, b) => {
                self.apply(a);
                self.apply(b);
            }
            Expr::UnaryOp(_, a) => self.apply(a),
            _ => {}
        };
    }
}

pub fn root_translate_to_resolvable(mut expr: Expr) -> MultiOperandOperation {
    // Restrict to only AND and OR
    ResolutionRuleset::build(&[
        ("a E b", "(a & b) | (!a & !b)"),
        ("a I b", "(!a | b)"),
        ("!(a & b)", "(!a | !b)"),
        ("!(a | b)", "(!a & !b)"),
        ("!!a", "a"),
        ("a ^ b", "(a | b) & (!a | !b)"),
    ])
    .apply(&mut expr);

    let mut res = translate_to_resolvable(expr);
    res.merge_inner();
    res
}

fn translate_to_resolvable(expr: Expr) -> MultiOperandOperation {
    match expr {
        Expr::Variable(x) => MultiOperandOperation::Value(MultiOperandOperationValue {
            var: x,
            inverted: false,
        }),
        Expr::UnaryOp(UnaryOperation::Not, x) => {
            if let Expr::Variable(x) = &*x {
                MultiOperandOperation::Value(MultiOperandOperationValue {
                    var: *x,
                    inverted: true,
                })
            } else {
                unreachable!()
            }
        }
        Expr::BinaryOp(op, a, b) => {
            let translated_a = translate_to_resolvable(*a);
            let translated_b = translate_to_resolvable(*b);
            MultiOperandOperation::Operation {
                operation: match op {
                    BinaryOperation::And => MultiOperandOperationType::And,
                    BinaryOperation::Or => MultiOperandOperationType::Or,
                    _ => unreachable!(),
                },
                values: vec![translated_a, translated_b],
            }
        }
        Expr::Constant(_) => panic!("Constants not supported."),
    }
}

impl MultiOperandOperation {
    fn merge_inner(&mut self) {
        match self {
            MultiOperandOperation::Operation {
                operation: my_op,
                values,
            } => {
                if values.len() == 1 {
                    *self = values.remove(0);
                    self.merge_inner();
                    return;
                }
                let old_values = take(values);
                for mut x in old_values {
                    x.merge_inner();
                    match x {
                        MultiOperandOperation::Operation {
                            operation: inner_op,
                            values: inner_values,
                        } if inner_op == *my_op => {
                            values.extend(inner_values);
                        }
                        other => values.push(other),
                    }
                }
            }
            _ => {}
        }
    }

    pub fn depth(&self) -> usize {
        match self {
            MultiOperandOperation::Value(_) => 0,
            MultiOperandOperation::Operation {
                operation: _,
                values,
            } => 1 + values.iter().map(|x| x.depth()).max().unwrap_or(0),
        }
    }

    pub fn reduce_single_rollover(&mut self, depth: i32) {
        match self {
            Self::Operation { operation, values } => {
                let mut merged = vec![];
                values.retain_mut(|x| {
                    if matches!(x, MultiOperandOperation::Value(_)) {
                        merged.push(take(x));
                        false
                    } else {
                        true
                    }
                });
                if merged.is_empty() {
                    merged.push(values.remove(0));
                }
                values.iter_mut().for_each(|e| {
                    *e = match take(e) {
                        MultiOperandOperation::Value(v) => {
                            let mut merged = merged.clone();
                            merged.push(MultiOperandOperation::Value(v));
                            MultiOperandOperation::Operation {
                                operation: *operation,
                                values: merged,
                            }
                        },
                        MultiOperandOperation::Operation {
                            operation: sub_oper,
                            values: sub_values,
                        } => MultiOperandOperation::Operation {
                            operation: sub_oper,
                            values: sub_values
                                .into_iter()
                                .map(|e| {
                                    let mut merged = merged.clone();
                                    merged.push(e);
                                    let mut new_sub = MultiOperandOperation::Operation {
                                        operation: *operation,
                                        values: merged,
                                    };
                                    new_sub.reduce_depth(depth + 1);
                                    new_sub
                                })
                                .collect(),
                        },
                    };
                });
            }
            Self::Value(_) => {
                unreachable!();
            } // Depth = 0.
        }
        self.merge_inner();
    }

    pub fn reduce_depth(&mut self, depth: i32) {
        self.merge_inner();
        while self.depth() > 2 {
            self.reduce_single_rollover(depth);
        }
    }
}

impl Display for MultiOperandOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MultiOperandOperation::Value(x) => {
                if x.inverted {
                    write!(f, "{}\u{0304}", x.var)
                } else {
                    write!(f, "{}", x.var)
                }
            }
            MultiOperandOperation::Operation { operation, values } => match operation {
                MultiOperandOperationType::And => {
                    write!(
                        f,
                        "{{{}}}",
                        values
                            .iter()
                            .map(|e| format!("{e}"))
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                }
                MultiOperandOperationType::Or => {
                    write!(
                        f,
                        "{}",
                        values
                            .iter()
                            .map(|e| format!("{e}"))
                            .collect::<Vec<_>>()
                            .join("")
                    )
                }
            },
        }
    }
}

impl From<&MultiOperandOperation> for Expr {
    fn from(value: &MultiOperandOperation) -> Expr {
        match value {
            MultiOperandOperation::Value(v) => {
                let var = Expr::Variable(v.var);
                if v.inverted {
                    Expr::UnaryOp(UnaryOperation::Not, Box::from(var))
                } else {
                    var
                }
            }
            MultiOperandOperation::Operation { operation, values } => {
                if values.len() < 2 { panic!("Impossible type"); }
                let new_op = match operation {
                    MultiOperandOperationType::And => BinaryOperation::And,
                    MultiOperandOperationType::Or => BinaryOperation::Or,
                };
                let mut current_top = Expr::BinaryOp(new_op, Box::from(Expr::from(&values[0])), Box::from(Expr::from(&values[1])));
                for i in 2..values.len() {
                    current_top = Expr::BinaryOp(new_op, Box::from(current_top), Box::from(Expr::from(&values[i])));
                }
                current_top
            }
        }
    }
}