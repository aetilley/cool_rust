use ustr::{ustr, Ustr};

// Note that Sym implements the Copy trait.
// Caution:  Calling `clone` on a &Sym returns a
// copy of a Sym, but calling `to_owned` will
// return a String.
pub type Sym = Ustr;

pub fn sym(s: &str) -> Ustr {
    ustr(s)
}

// Would be nice if we could define
// globals as follows, but sym is a non-constant
// function.  Think about this.
//  arg         : Sym = sym("arg");
//  arg2        : Sym = sym("arg2");
//  Bool        : Sym = sym("Bool");
//  concat      : Sym = sym("concat");
//  abort       : Sym = sym("abort");
//  copy        : Sym = sym("copy");
//  CInt        : Sym = sym("Int");
//  in_int      : Sym = sym("in_int");
//  in_string   : Sym = sym("in_string");
//  IO          : Sym = sym("IO");
//  length      : Sym = sym("length");
//  Main        : Sym = sym("Main");
//  main_meth   : Sym = sym("main");
//  _no_class   : Sym = sym("_no_class");
//  _no_type    : Sym = sym("_no_type");
//  Object      : Sym = sym("Object");
//  out_int     : Sym = sym("out_int");
//  out_string  : Sym = sym("out_string");
//  _prim_slot  : Sym = sym("_prim_slot");
//  self_sym    : Sym = sym("self");
//  SELF_TYPE   : Sym = sym("SELF_TYPE");
//  Str         : Sym = sym("String");
//  _str_field  : Sym = sym("_str_field");
//  substr      : Sym = sym("substr");
//  type_name   : Sym = sym("type_name");
//  _val        : Sym = sym("_val");

// In progress....

use interner::global::GlobalPool;
use interner::Pooled;
use std::hash::RandomState;

static STRINGS: GlobalPool<String> = GlobalPool::new();
static INTS: GlobalPool<String> = GlobalPool::new();
static SPECIAL: GlobalPool<String> = GlobalPool::new();

type SSym = Pooled<&'static GlobalPool<String>, RandomState>;

fn strsym(s: &str) -> SSym {
    STRINGS.get(s)
}

fn intsym(s: &str) -> SSym {
    INTS.get(s)
}

fn special(s: &str) -> SSym {
    SPECIAL.get(s)
}

// TEST this before jumping in
fn dump_strings() {
    let _all: Vec<Pooled<&'static GlobalPool<String>, RandomState>> = STRINGS.pooled();
    todo!();
}

fn dump_ints() {
    let _all: Vec<Pooled<&'static GlobalPool<String>, RandomState>> = STRINGS.pooled();
    todo!();
}
