#![allow(dead_code)]

pub mod ast;
pub mod token;

use lalrpop_util::lalrpop_mod;

lalrpop_mod!(cool_grammar);
