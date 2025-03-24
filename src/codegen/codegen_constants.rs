use crate::symbol::Sym;

// TODO make these usize

// Type Names
pub const BOOL: &str = "Bool";
pub const IO: &str = "IO";
pub const OBJECT: &str = "Object";
pub const INT: &str = "Int";
pub const STRING: &str = "String";

// Method Names
pub const LENGTH: &str = "length";

pub const SELF: &str = "self";
pub const INIT: &str = "init";
pub const TRUE: &str = "true";
pub const FALSE: &str = "false";

pub const VTABLE_IND: u32 = 0;

pub const OBJECT_PREFIX_SIZE: u32 = 1;

pub const INT_VAL_IND: u32 = 1;
pub const BOOL_VAL_IND: u32 = 1;
pub const STRING_LEN_IND: u32 = 1;
pub const STRING_CONTENT_IND: u32 = 2;

// Should maybe use %c to read whitespace.
pub const STRING_FORMATTER: &str = "%s\0";
pub const INT_FORMATTER: &str = "%d\0";
pub const MAX_IN_STRING_LEN: u64 = 10000;

pub const OUT_STRING: &str = "out_string";
pub const IN_STRING: &str = "in_string";

pub fn method_ref(cls_name: &Sym, method_name: &Sym) -> String {
    format!("{}.{}", cls_name, method_name)
}

pub fn vtable_ref(cls_name: &Sym) -> String {
    format!("{}_vtable", cls_name)
}

use std::hash::{DefaultHasher, Hash, Hasher};

pub fn init_ref(cls: &Sym) -> String {
    format!("{}_init", cls)
}

pub fn global_bool_ref(b: bool) -> String {
    format!("bool_{}", b)
}

pub fn global_int_ref(i: &Sym) -> String {
    let mut hasher = DefaultHasher::new();
    i.hash(&mut hasher);
    format!("int_{}", hasher.finish())
}

pub fn global_string_ref(s: &Sym) -> String {
    let mut hasher = DefaultHasher::new();
    s.hash(&mut hasher);
    format!("string_{}", hasher.finish())
}
