use interner::global::GlobalPool;
use interner::Pooled;
use std::hash::RandomState;

static STRINGS: GlobalPool<String> = GlobalPool::new();
static INTS: GlobalPool<String> = GlobalPool::new();
static SPECIAL: GlobalPool<String> = GlobalPool::new();

pub type Sym = Pooled<&'static GlobalPool<String>, RandomState>;

pub fn strsym(s: &str) -> Sym {
    STRINGS.get(s)
}

pub fn intsym(s: &str) -> Sym {
    INTS.get(s)
}

pub fn sym(s: &str) -> Sym {
    SPECIAL.get(s)
}

pub fn dump_special() -> Vec<Sym> {
    SPECIAL.pooled()
}

pub fn dump_strings() -> Vec<Sym> {
    STRINGS.pooled()
}

pub fn dump_ints() -> Vec<Sym> {
    INTS.pooled()
}
