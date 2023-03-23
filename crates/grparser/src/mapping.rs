use crate::error::FuncNotFound;
use crate::metadata;
use std::cell::RefCell;
use std::collections::HashMap;

// String(Full name of function including package and type) --> IR line number
thread_local!(static FUNC_TABLE: RefCell<HashMap<String, usize>> = RefCell::new(HashMap::new()));

// String(Full name of function including package and type) --> IR line number of function call
thread_local!(static JUMP_STACK: RefCell<Vec<(String, usize)>> = RefCell::new(vec![]));

pub fn get_func_entry(func_name: &str) -> Result<usize, FuncNotFound> {
    return FUNC_TABLE.with(|table| {
        for (func_name_, line_num) in table.borrow().iter() {
            if func_name == func_name_ {
                return Ok(*line_num);
            }
        }
        Err(FuncNotFound {
            func_name: func_name.to_string(),
        })
    });
}

pub fn set_func_entry(func_name: &str, line_num: usize) {
    FUNC_TABLE.with(|table| {
        table.borrow_mut().insert(func_name.to_string(), line_num);
    })
}

pub fn get_init_funcs() -> Vec<(String, usize)> {
    return FUNC_TABLE.with(|table| {
        table
            .borrow()
            .iter()
            .filter(|(func_name, _)| {
                metadata::has_func_desc(func_name, "Synthetic", "package initializer")
            })
            .map(|(func_name, line_num)| (func_name.to_string(), *line_num))
            .collect::<Vec<_>>()
    });
}

pub fn get_all_func_names() -> Vec<String> {
    return FUNC_TABLE.with(|table| {
        table
            .borrow()
            .iter()
            .map(|(func_name, _)| func_name.to_string())
            .collect::<Vec<_>>()
    });
}

pub fn push_func_callsite_pc(func_name: &str, line_num: usize) {
    JUMP_STACK.with(|stack| {
        stack.borrow_mut().push((func_name.to_string(), line_num));
    })
}

/// It's expected to be called when a `ReturnInstr` is executed.
pub fn pop_func_callsite_pc() -> Option<(String, usize)> {
    JUMP_STACK.with(|stack| stack.borrow_mut().pop())
}
