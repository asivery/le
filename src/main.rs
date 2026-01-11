use clap::Parser;
use lalrpop_util::lalrpop_mod;
use prettytable::{Cell, Row, Table, format::Alignment};
use std::collections::{HashMap, HashSet};

use crate::{
    grammar::ExprParser,
    language::{BinaryOperation, Expr, UnaryOperation},
    proof::Proof,
    resolver::root_translate_to_resolvable,
};

mod language;
mod proof;
mod resolver;

lalrpop_mod!(grammar);
type Mappings = HashMap<char, bool>;

impl BinaryOperation {
    pub fn eval(&self, mappings: &Mappings, a: &Expr, b: &Expr) -> bool {
        match self {
            BinaryOperation::And => a.eval(mappings) && b.eval(mappings),
            BinaryOperation::Or => a.eval(mappings) || b.eval(mappings),
            BinaryOperation::Xor => a.eval(mappings) || b.eval(mappings),
            BinaryOperation::Implies => (!a.eval(mappings)) || b.eval(mappings),
            BinaryOperation::Equates => {
                (a.eval(mappings) && b.eval(mappings)) || (!a.eval(mappings) && !b.eval(mappings))
            }
        }
    }
}

impl UnaryOperation {
    pub fn eval(&self, mappings: &Mappings, a: &Expr) -> bool {
        match self {
            UnaryOperation::Not => !a.eval(mappings),
        }
    }
}

impl Expr {
    pub fn eval(&self, mappings: &Mappings) -> bool {
        match self {
            Expr::Constant(x) => *x,
            Expr::Variable(x) => *mappings.get(x).unwrap(),
            Expr::BinaryOp(bin, a, b) => bin.eval(mappings, a, b),
            Expr::UnaryOp(un, a) => un.eval(mappings, a),
        }
    }
}

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Cli {
    expression: String,
    #[arg(long, short, default_value_t = false)]
    no_table: bool,
    #[arg(long, short, default_value_t = false)]
    proof: bool,
    #[arg(long, short, default_value_t = false)]
    to_normal: bool,
}

struct UsedVars(pub Vec<char>);

impl UsedVars {
    pub fn derive_mapping(&self, mut num: u32) -> Mappings {
        let mut m = Mappings::new();
        for x in self.0.iter().rev() {
            m.insert(*x, num & 1 == 1);
            num >>= 1;
        }

        m
    }
}

fn main() {
    let args = Cli::parse();
    let expr = ExprParser::new();
    let mut used_vars = HashSet::new();
    let parsed = expr.parse(&mut used_vars, &args.expression).unwrap();
    if args.to_normal {
        let mut converted = root_translate_to_resolvable(*parsed);
        println!("Basic multi-operand form: {converted}");
        converted.reduce_depth(0);
        println!("Converted formula: {converted}");
        println!("(Alternative): {:#}", Expr::from(&converted));
        return;
    }
    if args.proof {
        let mut proof = Proof::create_gentzen(&*parsed);
        if proof.complete {
            println!("Proof successful!");
        } else {
            println!("Proof FAILED!");
        }

        proof.frames.reverse();
        let mut i = proof.frames.len();
        for frame in proof.frames {
            println!("==== Frame {i} ====");
            for chain in frame.chains {
                if chain.is_terminal() {
                    print!("(AXIOM) ");
                }
                print!("|- ");
                if chain.formulas.len() > 1 {
                    print!("{{");
                }

                print!(
                    "{}",
                    chain
                        .formulas
                        .iter()
                        .map(|e| format!("{e}"))
                        .collect::<Vec<_>>()
                        .join(", ")
                );

                if chain.formulas.len() > 1 {
                    print!("}}");
                }

                print!("      ");
            }
            i -= 1;
            println!("\n");
        }
        return;
    }
    let mut used_vars: Vec<char> = used_vars.into_iter().collect();
    used_vars.sort();
    let used_vars = UsedVars(used_vars);

    let remapped_expression = format!("{parsed}");

    let mut table = Table::new();
    let mut header_row = Row::new(
        used_vars
            .0
            .iter()
            .map(|x| Cell::new(&x.to_string()))
            .collect(),
    );
    header_row.add_cell(Cell::new(""));
    header_row.add_cell(Cell::new(&remapped_expression));
    table.add_row(header_row);
    table.add_empty_row();

    let mut t_cell = Cell::new("T").style_spec("Fg");
    let mut f_cell = Cell::new("F").style_spec("Fr");

    f_cell.align(Alignment::CENTER);
    t_cell.align(Alignment::CENTER);

    let mut is_taut = true;

    let state_count = 2u32.pow(used_vars.0.len() as u32);
    for i_num in 0..state_count {
        let mappings = used_vars.derive_mapping(i_num);
        let mut row = Row::new(
            used_vars
                .0
                .iter()
                .map(|e| {
                    if *mappings.get(e).unwrap() {
                        t_cell.clone()
                    } else {
                        f_cell.clone()
                    }
                })
                .collect(),
        );
        row.add_cell(Cell::new(""));
        row.add_cell(if parsed.eval(&mappings) {
            t_cell.clone()
        } else {
            is_taut = false;
            f_cell.clone()
        });
        table.add_row(row);
    }

    if !args.no_table {
        table.printstd();
    }

    if is_taut {
        println!("This is a tautology!");
    } else {
        println!("This is NOT a tautology!");
    }
}
