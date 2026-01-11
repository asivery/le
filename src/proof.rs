use std::{collections::HashSet, mem::replace};

use crate::{
    grammar::ExprParser,
    language::{Expr, UnaryOperation},
};

struct GentzenRule {
    matcher: Expr,
    output_sets: Vec<Vec<Expr>>,
}

impl GentzenRule {
    fn build(pattern: &str, out: &[&[&str]]) -> Self {
        let parser = ExprParser::new();
        Self {
            matcher: *parser.parse(&mut HashSet::new(), pattern).unwrap(),
            output_sets: out
                .iter()
                .map(|chain| {
                    chain
                        .iter()
                        .map(|e| *parser.parse(&mut HashSet::new(), e).unwrap())
                        .collect()
                })
                .collect(),
        }
    }

    fn try_match(&self, expr: &Expr) -> Option<Vec<Vec<Expr>>> {
        let extracted = self.matcher.try_match(expr)?;
        let mut output = self.output_sets.clone();
        for i in output.iter_mut() {
            for j in i.iter_mut() {
                j.substitute(&extracted);
            }
        }
        Some(output)
    }
}

fn create_gentzen_rules() -> Vec<Vec<GentzenRule>> {
    vec![
        // Type-alpha:
        vec![
            GentzenRule::build("!!a", &[&["a"]]),
            GentzenRule::build("!(a & b)", &[&["!a", "!b"]]),
            GentzenRule::build("(a | b)", &[&["a", "b"]]),
            GentzenRule::build("a I b", &[&["!a", "b"]]),
            GentzenRule::build("!(a E b)", &[&["!(a I b)", "!(b I a)"]]),
            GentzenRule::build("a ^ b", &[&["!(a I b)", "!(b I a)"]]),
        ],
        // Type-beta:
        vec![
            GentzenRule::build("a & b", &[&["a"], &["b"]]),
            GentzenRule::build("!(a | b)", &[&["!a"], &["!b"]]),
            GentzenRule::build("!(a I b)", &[&["a"], &["!b"]]),
            GentzenRule::build("a E b", &[&["a I b"], &["b I a"]]),
            GentzenRule::build("!(a ^ b)", &[&["a I b"], &["b I a"]]),
        ],
    ]
}

#[derive(Clone, Debug)]
pub struct Chain {
    pub formulas: Vec<Expr>,
}

impl Chain {
    pub fn is_terminal(&self) -> bool {
        let vars = self
            .formulas
            .iter()
            .filter_map(|e| match e {
                Expr::Variable(v) => Some(*v),
                _ => None,
            })
            .collect::<Vec<char>>();
        for form in &self.formulas {
            match form {
                Expr::UnaryOp(UnaryOperation::Not, z) => match &**z {
                    Expr::Variable(z) if vars.contains(z) => return true,
                    _ => {}
                },
                _ => {}
            }
        }
        return false;
    }
}

#[derive(Clone, Debug)]
pub struct Frame {
    pub chains: Vec<Chain>,
}

#[derive(Clone, Debug)]
pub struct Proof {
    pub frames: Vec<Frame>,
    pub complete: bool,
}

impl Proof {
    pub fn create_gentzen(expr: &Expr) -> Self {
        let rules = create_gentzen_rules();
        let mut last_frame = Frame {
            chains: vec![Chain {
                formulas: vec![expr.clone()],
            }],
        };
        let mut frames = vec![];
        'main: loop {
            // Should we terminate?
            if last_frame.chains.iter().all(|e| e.is_terminal()) {
                break;
            }

            // Check the rules
            for ruleset in &rules {
                for (c, chain) in last_frame.chains.iter().enumerate() {
                    if chain.is_terminal() {
                        continue;
                    }
                    for (i, formula) in chain.formulas.iter().enumerate() {
                        for rule in ruleset {
                            if let Some(new_set) = rule.try_match(formula) {
                                let mut other_chains = last_frame
                                    .chains
                                    .iter()
                                    .enumerate()
                                    .filter(|&(j, e)| c != j && !e.is_terminal())
                                    .map(|e| e.1)
                                    .cloned()
                                    .collect::<Vec<Chain>>();
                                let base_of_chain_without_formula = chain
                                    .formulas
                                    .iter()
                                    .enumerate()
                                    .filter_map(
                                        |(j, e)| if i != j { Some(e.clone()) } else { None },
                                    )
                                    .collect::<Vec<_>>();
                                let new_chains = new_set
                                    .into_iter()
                                    .map(|mut e| {
                                        e.extend_from_slice(&base_of_chain_without_formula);
                                        Chain { formulas: e }
                                    })
                                    .collect::<Vec<Chain>>();
                                other_chains.extend_from_slice(&new_chains);
                                frames.push(replace(
                                    &mut last_frame,
                                    Frame {
                                        chains: other_chains,
                                    },
                                ));
                                continue 'main;
                            }
                        }
                    }
                }
            }

            // No rule matched - give up.
            frames.push(last_frame);
            return Proof {
                frames,
                complete: false,
            };
        }
        frames.push(last_frame);

        Proof {
            frames,
            complete: true,
        }
    }
}
