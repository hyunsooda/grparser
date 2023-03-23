use crate::metadata::{CodeLine, FreeVar, FuncSig};
use crate::value::Value;
use std::cell::RefCell;

thread_local!(static CUR_BLK_NUM: RefCell<u64> = RefCell::new(0));
thread_local!(static CUR_FUNC: RefCell<FuncInfo> =RefCell::new(FuncInfo::new( CodeLine::new(0, 0),
FuncSig::new(None, None, Value::Ident("".to_string()), vec![], vec![])
)));
thread_local!(static CUR_PACKAGE: RefCell<String> = RefCell::new("".to_string()));
thread_local!(static CUR_PARENT_FUNC: RefCell<String> = RefCell::new("".to_string()));
thread_local!(static CUR_FREE_VARS: RefCell<Vec<FreeVar>> = RefCell::new(vec![]));
thread_local!(static CUR_SYNTHETIC: RefCell<String> = RefCell::new("".to_string()));

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FuncInfo {
    pub codeline: CodeLine,
    pub func_sig: FuncSig,
}
impl FuncInfo {
    fn new(codeline: CodeLine, func_sig: FuncSig) -> Self {
        Self { codeline, func_sig }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InstrScope {
    pub func_info: FuncInfo,
    pub blk_num: u64,
}

pub fn get_cur_scope() -> InstrScope {
    InstrScope {
        func_info: get_cur_func_info(),
        blk_num: get_cur_blk_num(),
    }
}

pub fn set_cur_func_sig(func_sig: &FuncSig) {
    CUR_FUNC.with(|f| {
        *f.borrow_mut() = FuncInfo::new(CodeLine::new(0, 0), func_sig.clone());
    });
}

pub fn get_cur_func_sig() -> FuncSig {
    CUR_FUNC.with(|f| f.clone().into_inner().func_sig)
}

pub fn get_cur_func_info() -> FuncInfo {
    CUR_FUNC.with(|f| f.clone().into_inner())
}

pub fn set_cur_blk_num(num: u64) {
    CUR_BLK_NUM.with(|n| *n.borrow_mut() = num);
}

pub fn get_cur_blk_num() -> u64 {
    CUR_BLK_NUM.with(|num| num.clone().into_inner())
}

pub fn set_cur_package(package: &str) {
    CUR_PACKAGE.with(|p| *p.borrow_mut() = package.to_string())
}

pub fn get_cur_package() -> String {
    CUR_PACKAGE.with(|p| p.clone().into_inner())
}

pub fn set_cur_func_line(codeline: CodeLine) {
    CUR_FUNC.with(|f| {
        (*f.borrow_mut()).codeline = codeline;
    });
}

pub fn set_cur_func_parent(func_name: &str) {
    CUR_PARENT_FUNC.with(|f| {
        *f.borrow_mut() = func_name.to_string();
    })
}

pub fn get_cur_func_parent() -> String {
    CUR_PARENT_FUNC.with(|f| f.clone().into_inner())
}

pub fn set_cur_func_synthetic(synthetic: &str) {
    CUR_SYNTHETIC.with(|f| {
        *f.borrow_mut() = synthetic.to_string();
    })
}

pub fn get_cur_func_synthetic() -> String {
    CUR_SYNTHETIC.with(|f| f.clone().into_inner())
}

pub fn set_cur_func_free_vars(free_vars: Vec<FreeVar>) {
    CUR_FREE_VARS.with(|f| {
        *f.borrow_mut() = free_vars;
    })
}

pub fn get_cur_func_free_vars() -> Vec<FreeVar> {
    CUR_FREE_VARS.with(|f| f.clone().into_inner())
}
