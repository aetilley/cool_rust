#![allow(dead_code)]

pub mod ast;
pub mod ast_parse;
pub mod class_table;
pub mod env;
pub mod semant;
pub mod symbol;
pub mod token;
pub mod token_utils;

use lalrpop_util::lalrpop_mod;
lalrpop_mod!(cool_grammar);
