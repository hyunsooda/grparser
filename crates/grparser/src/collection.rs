// `collection.rs` contains `type` definition

use crate::parse;
use crate::scope;
use crate::typ::Typ;
use std::cell::RefCell;
use std::collections::HashMap;

// global type def map (package -> type name -> type def)
thread_local!(static TYP_DEF_MAP: RefCell<HashMap<String, HashMap<String,TypDef>>> = RefCell::new(HashMap::new()));

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct TypDef {
    pub package: String,
    pub name: String,
    pub typ: Typ,
}
impl TypDef {
    fn new(package: String, name: String, typ: Typ) -> Self {
        Self { package, name, typ }
    }
    pub fn to_string(&self) -> String {
        format!("{}.{}", self.package, self.name)
    }
}

fn insert_typ_def(package: &str, typ_name: String, typ: Typ) {
    let typ_def = TypDef::new(package.to_string(), typ_name.clone(), typ);
    TYP_DEF_MAP.with(|m| {
        m.borrow_mut()
            .entry(package.to_string())
            .or_insert(HashMap::new())
            .insert(typ_name.to_string(), typ_def);
    });
}

pub fn parse_typ_defs(typ_defs: &str) {
    let defs = typ_defs
        .split("\n")
        .map(|typ_def_str| typ_def_str.trim_start())
        .collect::<Vec<_>>();
    for def in &defs {
        if def.starts_with("package") {
            scope::set_cur_package(&def["package".len() + 1..def.len() - 1]);
        }
        if def.starts_with("type") {
            let (_, (typ_name, typ)) = parse::parse_typ_def(def).unwrap();
            let package = &scope::get_cur_package();
            insert_typ_def(package, typ_name, typ);
        }
    }
}

pub fn get_typ(typ_str: &str) -> Option<Typ> {
    let (_, (package, def_name)) = parse::parse_package_with_ident(typ_str).unwrap();
    TYP_DEF_MAP.with(|m| {
        if let Some(typ_def_map) = m.borrow().get(&package) {
            if let Some(typ_def) = typ_def_map.get(&def_name) {
                return Some(typ_def.typ.clone());
            }
        }
        None
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_typ_def() {
        let mut input = "type  Reader     interface{Read(p []byte) (n int, err error)}";
        let (_, parsed) = parse::parse_typ_def(input).unwrap();
        println!("{:?}", parsed);
        println!("{}", parsed.1.to_string());

        input = "type  State interface{Flag(c int) bool; Precision() (prec int, ok bool); Width() (wid int, ok bool); Write(b []byte) (n int, err error)}";
        let (_, parsed) = parse::parse_typ_def(input).unwrap();
        println!("{:?}", parsed);

        input = "type  stringReader string";
        let (_, parsed) = parse::parse_typ_def(input).unwrap();
        println!("{:?}", parsed);

        input = "type  Header  map[string]map[string]map[string][]string";
        let (_, parsed) = parse::parse_typ_def(input).unwrap();
        println!("{:?}", parsed);

        input = "type  Pointer    Pointer";
        let (_, parsed) = parse::parse_typ_def(input).unwrap();
        println!("{:?}", parsed);

        input = "type  rect  struct{width float64; height float64}";
        let (_, parsed) = parse::parse_struct_typ(input).unwrap();
        println!("{:?}", parsed);

        input = "type  Timer  struct{C <-chan int}";
        let (_, parsed) = parse::parse_struct_typ(input).unwrap();
        println!("{:?}", parsed);

        input = "type  runtimeTimer struct{f func(a any, b uintptr); arg any}";
        let (_, parsed) = parse::parse_struct_typ(input).unwrap();
        println!("{:?}", parsed);

        input = "Source}";
        let (_, parsed) = parse::parse_package_with_ident(input).unwrap();
        println!("{:?}", parsed);
    }
}
