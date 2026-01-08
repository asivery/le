use clap::Parser;
use lalrpop_util::lalrpop_mod;
use prettytable::{Cell, Row, Table, format::Alignment};
use std::collections::{HashMap, HashSet};

use crate::{
    grammar::ExprParser,
    language::{BinaryOperation, Expr, UnaryOperation},
};

mod language;
mod proof;

lalrpop_mod!(grammar);
type Mappings = HashMap<char, bool>;

impl BinaryOperation {
    pub fn eval(&self, mappings: &Mappings) -> bool {
        match self {
            BinaryOperation::And(a, b) => a.eval(mappings) && b.eval(mappings),
            BinaryOperation::Or(a, b) => a.eval(mappings) || b.eval(mappings),
            BinaryOperation::Xor(a, b) => a.eval(mappings) || b.eval(mappings),
            BinaryOperation::Implies(a, b) => (!a.eval(mappings)) || b.eval(mappings),
            BinaryOperation::Equates(a, b) => {
                (a.eval(mappings) && b.eval(mappings)) || (!a.eval(mappings) && !b.eval(mappings))
            }
        }
    }
}

impl UnaryOperation {
    pub fn eval(&self, mappings: &Mappings) -> bool {
        match self {
            UnaryOperation::Not(a) => !a.eval(mappings),
        }
    }
}

impl Expr {
    pub fn eval(&self, mappings: &Mappings) -> bool {
        match self {
            Expr::Constant(x) => *x,
            Expr::Variable(x) => *mappings.get(x).unwrap(),
            Expr::BinaryOp(bin) => bin.eval(mappings),
            Expr::UnaryOp(un) => un.eval(mappings),
        }
    }
}

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Cli {
    expression: String,
    #[arg(long, short, default_value_t=false)]
    no_table: bool,
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
    let mut char_remap = HashMap::new();
    char_remap.insert('I', '⇒');
    char_remap.insert('E', '⇔');
    char_remap.insert('!', '¬');
    char_remap.insert('&', '∧');
    char_remap.insert('|', '∨');
    char_remap.insert('^', '⊕');

    let args = Cli::parse();
    let expr = ExprParser::new();
    let mut used_vars = HashSet::new();
    let parsed = expr.parse(&mut used_vars, &args.expression).unwrap();
    let mut used_vars: Vec<char> = used_vars.into_iter().collect();
    used_vars.sort();
    let used_vars = UsedVars(used_vars);

    let remapped_expression = args
        .expression
        .chars()
        .map(|e| *char_remap.get(&e).unwrap_or(&e))
        .collect::<String>();

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
