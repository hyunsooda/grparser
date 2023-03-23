use std::{error::Error, fmt};

#[derive(Debug)]
pub struct FuncNotFound {
    pub func_name: String,
}

impl Error for FuncNotFound {}

impl fmt::Display for FuncNotFound {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Not found such a function {}", self.func_name)
    }
}
