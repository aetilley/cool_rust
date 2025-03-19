use crate::symbol::Sym;

pub const VTABLE_IND: u32 = 0;

pub const INT_VAL_IND: u32 = 1;
pub const BOOL_VAL_IND: u32 = 1;
pub const STRING_LEN_IND: u32 = 1;
pub const STRING_CONTENT_IND: u32 = 2;

pub fn method_ref(cls_name: Sym, method_name: Sym) -> String {
    format!("{}.{}", cls_name, method_name)
}

pub fn vtable_ref(cls_name: Sym) -> String {
    format!("{}_vtable", cls_name)
}
