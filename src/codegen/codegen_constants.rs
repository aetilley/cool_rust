use crate::symbol::Sym;
use std::hash::{DefaultHasher, Hash, Hasher};

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

pub const CLASS_ID_IND: u32 = 0;
pub const OBJECT_PREFIX_SIZE: u32 = 1;

pub const VTABLE_MASTER_VECTOR: &str = "vtable_master_vector";
pub const PARENT_VECTOR: &str = "parent_vector";
pub const TYPE_NAME_VECTOR: &str = "type_name_vector";
pub const STRUCT_SIZE_TABLE: &str = "struct_size_table";

pub const INT_VAL_IND: u32 = 1;
pub const BOOL_VAL_IND: u32 = 1;
pub const STRING_LEN_IND: u32 = 1;
pub const STRING_CONTENT_IND: u32 = 2;

// Should maybe use %c to read whitespace.
pub const STRING_FORMATTER: &str = "%s\0";
pub const INT_FORMATTER: &str = "%d\0";
pub const MAX_IN_STRING_LEN: u64 = 100;

pub const OUT_STRING: &str = "out_string";
pub const IN_STRING: &str = "in_string";
pub const OUT_INT: &str = "out_int";
pub const IN_INT: &str = "in_int";
pub const IS_SUBTYPE: &str = "is_subtype";
pub const MIN_BOUND_FINDER: &str = "min_bound_finder";
pub const TYPE_IS_MEMBER: &str = "type_is_member";
pub const CASE_SELECTOR: &str = "case_selector";
pub const CONCAT: &str = "concat";
pub const SUBSTRING: &str = "substr";
pub const ABORT: &str = "abort";
pub const COPY: &str = "copy";
pub const TYPE_NAME: &str = "type_name";

pub const ENTRY: &str = "main";

pub fn method_ref(cls_name: &Sym, method_name: &Sym) -> String {
    format!("{}.{}", cls_name, method_name)
}

pub fn vtable_ref(cls_name: &Sym) -> String {
    format!("{}_vtable", cls_name)
}

pub fn init_ref(cls: &Sym) -> String {
    format!("{}_init", cls)
}

pub fn global_bool_ref(b: bool) -> String {
    format!("bool_{}", b)
}

pub fn global_int_ref(i: &Sym) -> String {
    let i_str = &i[..];
    let mut hasher = DefaultHasher::new();
    i_str.hash(&mut hasher);
    format!("int_{}", hasher.finish())
}

pub fn global_string_ref(s: &Sym) -> String {
    let s_str = &s[..];
    let mut hasher = DefaultHasher::new();
    s_str.hash(&mut hasher);
    format!("string_{}", hasher.finish())
}
