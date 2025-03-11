#![allow(dead_code)]

pub mod symbol;
pub mod token;
pub mod ast;
pub mod class_table;
pub mod env;
pub mod semant;

use lalrpop_util::lalrpop_mod;
lalrpop_mod!(cool_grammar);
