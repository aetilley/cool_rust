#![allow(dead_code)]

pub mod token;
pub mod ast;


use lalrpop_util::lalrpop_mod;

lalrpop_mod!(cool_grammar);
