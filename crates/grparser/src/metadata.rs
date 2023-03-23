use crate::instr::Instruction;
use crate::scope;
use crate::typ::Typ;
use crate::value::Value;
use std::cell::RefCell;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MetadataTyp {
    FuncDesc,
    Func,
    AstExpr,
    Var,
    Address,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MetadataAddressTyp {
    Var,
    Field,
}
impl MetadataAddressTyp {
    pub fn new(addr_typ: &str) -> Self {
        match addr_typ {
            "var" => Self::Var,
            "field" => Self::Field,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AstExprTyp {
    UnknownTyp, // TODO: `Unknown` should be removed once the entire ast tpyes are investigated
    SelectorExpr,
    BinaryExpr,
    CallExpr,
    UnaryExpr,
    StarExpr,
    BasicLit,
    IndexExpr,
    SliceExpr,
    CompositeLit,
    TypeAssertExpr,
    FuncLit,
}
impl AstExprTyp {
    pub fn from_str(expr_typ: &str) -> Self {
        match expr_typ {
            "SelectorExpr" => Self::SelectorExpr,
            "BinaryExpr" => Self::BinaryExpr,
            "CallExpr" => Self::CallExpr,
            "UnaryExpr" => Self::UnaryExpr,
            "StarExpr" => Self::UnaryExpr,
            "BasicLit" => Self::BasicLit,
            "IndexExpr" => Self::IndexExpr,
            "SliceExpr" => Self::SliceExpr,
            "CompositeLit" => Self::CompositeLit,
            "TypeAssertExpr" => Self::TypeAssertExpr,
            "FuncLit" => Self::FuncLit,
            _ => Self::UnknownTyp,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FuncSig {
    pub package: String,
    pub parent: String,
    pub free_vars: Vec<FreeVar>,
    pub is_init_func: bool,
    pub receiver_ident: Option<Value>,
    pub receiver_typ: Option<Typ>,
    pub func_ident: Value,
    pub func_param_set: Vec<(Option<Value>, Typ)>,
    pub func_ret_set: Vec<(Option<Value>, Typ)>,
}
impl FuncSig {
    pub fn new(
        receiver_ident: Option<Value>,
        receiver_typ: Option<Typ>,
        func_ident: Value,
        func_param_set: Vec<(Option<Value>, Typ)>,
        func_ret_set: Vec<(Option<Value>, Typ)>,
    ) -> Self {
        let is_init_func = if scope::get_cur_func_synthetic() == "package initializer" {
            true
        } else {
            false
        };
        Self {
            package: scope::get_cur_package(),
            parent: scope::get_cur_func_parent(),
            free_vars: scope::get_cur_func_free_vars(),
            is_init_func,
            receiver_ident,
            receiver_typ,
            func_ident,
            func_param_set,
            func_ret_set,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FuncDescMetadata {
    pub metadata_typ: MetadataTyp,
    pub prop: String,
    pub desc: String,
    pub func_sig: Option<FuncSig>,
    pub raw_ir: String,
}
impl FuncDescMetadata {
    pub fn new(prop: &str, desc: &str, func_sig: Option<FuncSig>, raw_ir: &str) -> Self {
        Self {
            metadata_typ: MetadataTyp::FuncDesc,
            prop: prop.to_string(),
            desc: desc.to_string(),
            func_sig,
            raw_ir: raw_ir.to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Metadata {
    VarMetadata(VarMetadata),
    AddressMetadataVar(AddressMetadataVar),
    AddressMetadataAst(AddressMetadataAst),
    FuncDescMetadata(FuncDescMetadata),
    FuncMetadata(FuncMetadata),
    AstExprMetadata(AstExprMetadata),
    IfLocMetadata(IfLocMetadata),
    PanicMetadata(PanicMetadata),
}
impl Metadata {
    pub fn new_func_desc(prop: &str, desc: &str, func_sig: Option<FuncSig>, raw_ir: &str) -> Self {
        Self::FuncDescMetadata(FuncDescMetadata::new(prop, desc, func_sig, raw_ir))
    }

    pub fn new_var_metadata(
        ident: Value,
        typ: Typ,
        file_loc: FileLoc,
        value: Value,
        raw_ir: &str,
    ) -> Self {
        Self::VarMetadata(VarMetadata::new(ident, typ, file_loc, value, raw_ir))
    }

    pub fn new_address_metadata_var(
        var_or_field: MetadataAddressTyp,
        ident: Value,
        typ: Typ,
        file_loc: FileLoc,
        value: Value,
        raw_ir: &str,
    ) -> Self {
        Self::AddressMetadataVar(AddressMetadataVar::new(
            var_or_field,
            ident,
            typ,
            file_loc,
            value,
            raw_ir,
        ))
    }

    pub fn new_address_metadata_ast(
        expr_typ: &str,
        file_loc: FileLoc,
        value: Value,
        raw_ir: &str,
    ) -> Self {
        Self::AddressMetadataAst(AddressMetadataAst::new(expr_typ, file_loc, value, raw_ir))
    }

    pub fn new_func_metadata(
        package: Value,
        func_ident: Value,
        func_params: Vec<Typ>,
        func_ret_typs: Vec<Typ>,
        file_loc: FileLoc,
        value: Value,
        raw_ir: &str,
    ) -> Self {
        Self::FuncMetadata(FuncMetadata::new(
            package,
            func_ident,
            func_params,
            func_ret_typs,
            file_loc,
            value,
            raw_ir,
        ))
    }

    pub fn new_ast_expr_metadata(
        expr_typ: &str,
        file_loc: FileLoc,
        value: Value,
        raw_ir: &str,
    ) -> Self {
        Self::AstExprMetadata(AstExprMetadata::new(
            AstExprTyp::from_str(expr_typ),
            file_loc,
            value,
            raw_ir,
        ))
    }

    pub fn new_ifloc_metadata(if_instr: Instruction, file_loc: FileLoc, raw_ir: &str) -> Self {
        Self::IfLocMetadata(IfLocMetadata::new(if_instr, file_loc, raw_ir))
    }

    pub fn new_panic_metadata(panic_instr: Instruction, file_loc: FileLoc, raw_ir: &str) -> Self {
        Self::PanicMetadata(PanicMetadata::new(panic_instr, file_loc, raw_ir))
    }

    pub fn get_ident(metadata: &Metadata) -> Value {
        match metadata {
            Self::FuncDescMetadata(_) => unreachable!(),
            Self::VarMetadata(metadata) => metadata.ident.clone(),
            Self::AddressMetadataVar(metadata) => metadata.ident.clone(),
            Self::AddressMetadataAst(_) => unreachable!(),
            Self::FuncMetadata(metadata) => metadata.ident.clone(),
            Self::AstExprMetadata(_) => unreachable!(),
            Self::IfLocMetadata(_) => unreachable!(),
            Self::PanicMetadata(_) => unreachable!(),
        }
    }

    pub fn get_file_loc(&self) -> FileLoc {
        match self {
            Self::FuncDescMetadata(_) => unreachable!(),
            Self::VarMetadata(metadata) => metadata.get_file_loc(),
            Self::AddressMetadataVar(metadata) => metadata.get_file_loc(),
            Self::AddressMetadataAst(metadata) => metadata.get_file_loc(),
            Self::FuncMetadata(metadata) => metadata.get_file_loc(),
            Self::AstExprMetadata(metadata) => metadata.get_file_loc(),
            Self::IfLocMetadata(metadata) => metadata.get_file_loc(),
            Self::PanicMetadata(metadata) => metadata.get_file_loc(),
        }
    }

    pub fn dump(&self) {
        match self {
            Self::FuncDescMetadata(_) => unreachable!(),
            Self::VarMetadata(metadata) => metadata.dump(),
            Self::AddressMetadataVar(metadata) => metadata.dump(),
            Self::AddressMetadataAst(metadata) => metadata.dump(),
            Self::FuncMetadata(metadata) => metadata.dump(),
            Self::AstExprMetadata(metadata) => metadata.dump(),
            Self::IfLocMetadata(metadata) => metadata.dump(),
            Self::PanicMetadata(metadata) => metadata.dump(),
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            Self::FuncDescMetadata(_) => unreachable!(),
            Self::VarMetadata(metadata) => metadata.to_string(),
            Self::AddressMetadataVar(metadata) => metadata.to_string(),
            Self::AddressMetadataAst(metadata) => metadata.to_string(),
            Self::FuncMetadata(metadata) => metadata.to_string(),
            Self::AstExprMetadata(metadata) => metadata.to_string(),
            Self::IfLocMetadata(metadata) => metadata.to_string(),
            Self::PanicMetadata(metadata) => metadata.to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VarMetadata {
    pub metadata_typ: MetadataTyp,
    pub ident: Value,
    pub typ: Typ,
    pub file_loc: FileLoc,
    pub value: Value,
    pub raw_ir: String,
}
impl VarMetadata {
    pub fn new(ident: Value, typ: Typ, file_loc: FileLoc, value: Value, raw_ir: &str) -> Self {
        Self {
            metadata_typ: MetadataTyp::Var,
            ident,
            typ,
            file_loc,
            value,
            raw_ir: raw_ir.to_string(),
        }
    }
    pub fn get_file_loc(&self) -> FileLoc {
        self.file_loc.clone()
    }
    pub fn dump(&self) {
        println!("{:?}", self);
    }
    pub fn to_string(&self) -> String {
        format!("{:?}", self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AddressMetadataVar {
    pub metadata_typ: MetadataTyp,
    pub var_or_field: MetadataAddressTyp,
    pub ident: Value,
    pub typ: Typ,
    pub file_loc: FileLoc,
    pub value: Value,
    pub raw_ir: String,
}
impl AddressMetadataVar {
    pub fn new(
        var_or_field: MetadataAddressTyp,
        ident: Value,
        typ: Typ,
        file_loc: FileLoc,
        value: Value,
        raw_ir: &str,
    ) -> Self {
        Self {
            metadata_typ: MetadataTyp::Address,
            var_or_field,
            ident,
            typ,
            file_loc,
            value,
            raw_ir: raw_ir.to_string(),
        }
    }
    pub fn get_file_loc(&self) -> FileLoc {
        self.file_loc.clone()
    }
    pub fn dump(&self) {
        println!("{:?}", self);
    }
    pub fn to_string(&self) -> String {
        format!("{:?}", self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AddressMetadataAst {
    pub metadata_typ: MetadataTyp,
    pub expr_typ: AstExprTyp,
    pub file_loc: FileLoc,
    pub value: Value,
    pub raw_ir: String,
}
impl AddressMetadataAst {
    pub fn new(expr_typ: &str, file_loc: FileLoc, value: Value, raw_ir: &str) -> Self {
        Self {
            metadata_typ: MetadataTyp::Address,
            expr_typ: AstExprTyp::from_str(expr_typ),
            file_loc,
            value,
            raw_ir: raw_ir.to_string(),
        }
    }
    pub fn get_file_loc(&self) -> FileLoc {
        self.file_loc.clone()
    }
    pub fn dump(&self) {
        println!("{:?}", self);
    }
    pub fn to_string(&self) -> String {
        format!("{:?}", self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AstExprMetadata {
    pub metadata_typ: MetadataTyp,
    pub expr_typ: AstExprTyp,
    pub file_loc: FileLoc,
    pub value: Value,
    pub raw_ir: String,
}
impl AstExprMetadata {
    pub fn new(expr_typ: AstExprTyp, file_loc: FileLoc, value: Value, raw_ir: &str) -> Self {
        Self {
            metadata_typ: MetadataTyp::AstExpr,
            expr_typ,
            file_loc,
            value,
            raw_ir: raw_ir.to_string(),
        }
    }
    pub fn get_file_loc(&self) -> FileLoc {
        self.file_loc.clone()
    }
    pub fn dump(&self) {
        println!("{:?}", self);
    }
    pub fn to_string(&self) -> String {
        format!("{:?}", self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IfLocMetadata {
    pub if_instr: Instruction,
    pub file_loc: FileLoc,
    pub raw_ir: String,
}
impl IfLocMetadata {
    pub fn new(if_instr: Instruction, file_loc: FileLoc, raw_ir: &str) -> Self {
        Self {
            if_instr,
            file_loc,
            raw_ir: raw_ir.to_string(),
        }
    }
    pub fn get_file_loc(&self) -> FileLoc {
        self.file_loc.clone()
    }
    pub fn dump(&self) {
        println!("{:?}", self);
    }
    pub fn to_string(&self) -> String {
        format!("{:?}", self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PanicMetadata {
    pub panic_instr: Instruction,
    pub file_loc: FileLoc,
    pub raw_ir: String,
}
impl PanicMetadata {
    pub fn new(panic_instr: Instruction, file_loc: FileLoc, raw_ir: &str) -> Self {
        Self {
            panic_instr,
            file_loc,
            raw_ir: raw_ir.to_string(),
        }
    }
    pub fn get_file_loc(&self) -> FileLoc {
        self.file_loc.clone()
    }
    pub fn dump(&self) {
        println!("{:?}", self);
    }
    pub fn to_string(&self) -> String {
        format!("{:?}", self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FuncMetadata {
    pub metadata_typ: MetadataTyp,
    pub package: Value,
    pub ident: Value,
    pub func_param_typs: Vec<Typ>,
    pub func_ret_typs: Vec<Typ>,
    pub file_loc: FileLoc,
    pub value: Value,
    pub raw_ir: String,
}
impl FuncMetadata {
    pub fn new(
        package: Value,
        ident: Value,
        func_param_typs: Vec<Typ>,
        func_ret_typs: Vec<Typ>,
        file_loc: FileLoc,
        value: Value,
        raw_ir: &str,
    ) -> Self {
        Self {
            metadata_typ: MetadataTyp::Func,
            package,
            ident,
            func_param_typs,
            func_ret_typs,
            file_loc,
            value,
            raw_ir: raw_ir.to_string(),
        }
    }
    pub fn get_file_loc(&self) -> FileLoc {
        self.file_loc.clone()
    }
    pub fn dump(&self) {
        println!("{:?}", self);
    }
    pub fn to_string(&self) -> String {
        format!("{:?}", self)
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum MetadataKey {
    MetadataKey(String, MetadataTyp),
}
impl MetadataKey {
    fn new(ident: &Value, metadata_typ: &MetadataTyp) -> Self {
        Self::MetadataKey(ident.to_string(), metadata_typ.clone())
    }
}

thread_local!(static METADATA_MAP: RefCell<HashMap<MetadataKey, Vec<Metadata>>> = RefCell::new(HashMap::new()));
thread_local!(static FUNC_DESC_MAP: RefCell<HashMap<String, FuncDesc>> = RefCell::new(HashMap::new()));
thread_local!(static INTERNAL_PACKAGE_MAP: RefCell<Vec<String>> = RefCell::new(vec![]));

fn get_metadata_key(metadata: &Metadata) -> MetadataKey {
    match metadata {
        Metadata::VarMetadata(ref metadata) => {
            MetadataKey::new(&metadata.value, &metadata.metadata_typ)
        }
        Metadata::AddressMetadataVar(ref metadata) => {
            MetadataKey::new(&metadata.value, &metadata.metadata_typ)
        }
        Metadata::AddressMetadataAst(ref metadata) => {
            MetadataKey::new(&metadata.value, &metadata.metadata_typ)
        }
        Metadata::FuncMetadata(ref metadata) => {
            MetadataKey::new(&metadata.ident, &metadata.metadata_typ)
        }
        Metadata::AstExprMetadata(ref metadata) => {
            MetadataKey::new(&metadata.value, &metadata.metadata_typ)
        }
        Metadata::IfLocMetadata(_) => unreachable!(),
        Metadata::PanicMetadata(_) => unreachable!(),
        Metadata::FuncDescMetadata(_) => unreachable!(),
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CodeLine {
    pub row: u64,
    pub col: u64,
}
impl CodeLine {
    pub fn new(row: u64, col: u64) -> Self {
        Self { row, col }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FreeVar {
    pub var_idx: usize,
    pub ident: Value,
    pub typ: Typ,
}
impl FreeVar {
    pub fn new(var_idx: usize, ident: Value, typ: Typ) -> Self {
        Self {
            var_idx,
            ident,
            typ,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FileLoc {
    pub filename: String,
    pub codeline: CodeLine,
}
impl FileLoc {
    pub fn new(filename: String, codeline: CodeLine) -> Self {
        Self { filename, codeline }
    }
}

pub fn store_metadata(metadata: Metadata) {
    METADATA_MAP.with(|m| {
        m.borrow_mut()
            .entry(get_metadata_key(&metadata))
            .or_insert(vec![])
            .push(metadata);
    });
}

pub fn get_all_metadata() -> Vec<Metadata> {
    let mut metas = vec![];
    METADATA_MAP.with(|m| {
        for (_, metadata) in m.borrow().iter() {
            metas.append(&mut metadata.clone());
        }
    });
    metas
}

#[derive(Debug, Clone)]
pub struct FuncDesc {
    pub desc: HashMap<String, String>,
    pub func_sig: FuncSig,
}
impl FuncDesc {
    fn new(props: Vec<String>, descs: Vec<String>, func_sig: FuncSig) -> Self {
        let mut m = HashMap::new();
        for (prop, desc) in props.into_iter().zip(descs.into_iter()) {
            m.insert(prop, desc);
        }
        Self { desc: m, func_sig }
    }
    fn dump(&self) {
        for (prop, desc) in self.desc.iter() {
            println!("{prop}: {desc}");
        }
        println!("{:?}", self.func_sig);
    }
    pub fn to_string(&self) -> String {
        let mut func_desc_str = "".to_string();
        for (prop, desc) in self.desc.iter() {
            func_desc_str = format!("{}{prop}:{desc}\n", func_desc_str);
        }
        format!("{}\n{:?}", func_desc_str, self.func_sig)
    }
}

pub fn store_func_desc(func_desc: Vec<FuncDescMetadata>) {
    let mut props = vec![];
    let mut descs = vec![];
    let mut func_sig = None;
    for metadata in func_desc {
        if metadata.prop == "Location" {
            let codeline = {
                let splitted = metadata.desc.split(":").collect::<Vec<_>>();
                CodeLine::new(
                    splitted[1].parse::<u64>().unwrap(),
                    splitted[2].parse::<u64>().unwrap(),
                )
            };
            scope::set_cur_func_line(codeline);
        }
        if metadata.prop == "Package" {
            INTERNAL_PACKAGE_MAP.with(|v| {
                v.borrow_mut().push(metadata.desc.clone());
            })
        }
        if metadata.prop != "" {
            props.push(metadata.prop);
            descs.push(metadata.desc);
        }
        if let Some(_) = metadata.func_sig {
            func_sig = metadata.func_sig;
        }
    }
    func_sig = {
        if let Some(fsig) = func_sig {
            Some(fsig)
        } else {
            unreachable!()
        }
    };
    let func_name = descs[0].clone();
    FUNC_DESC_MAP.with(|m| {
        let func_desc = FuncDesc::new(props, descs, func_sig.unwrap());
        m.borrow_mut().insert(func_name, func_desc);
    })
}

pub fn get_full_func_name(package: &str, receiver_typ: &Option<Typ>, func_ident: &str) -> String {
    let key = if let Some(receiver_typ) = receiver_typ {
        format!("({}).{}", receiver_typ.to_string(), func_ident)
    } else {
        format!("{}.{}", package, func_ident)
    };
    FUNC_DESC_MAP.with(|m| {
        if let Some(func_name) = m.borrow().get(&key) {
            func_name.desc["Name"].clone()
        } else {
            "".to_string()
        }
    })
}

pub fn get_all_func_desc() -> Vec<FuncDesc> {
    let mut func_desc_vec = vec![];
    FUNC_DESC_MAP.with(|m| {
        for (_, func_desc) in m.borrow().iter() {
            func_desc_vec.push(func_desc.clone());
        }
    });
    func_desc_vec
}

pub fn has_func_desc(func_name: &str, prop: &str, desc: &str) -> bool {
    let func_descs = get_all_func_desc();
    for func_desc in func_descs {
        if let Some(_func_name) = func_desc.desc.get("Name") {
            if let Some(_desc) = func_desc.desc.get(prop) {
                if _desc == desc && _func_name == func_name {
                    return true;
                }
            }
        }
    }
    false
}

/// `is_third_party_typ` does not look up the internal type.
/// For example, The `ExternalStruct` type is not inspected in the case of `Ptr(ExternalStruct)`.
/// Thus, the work of taking off its internal type is delegated to caller.
pub fn is_third_party_typ(typ: &Typ) -> bool {
    match typ {
        Typ::Struct(s) => is_third_party_package(&s.package),
        Typ::DefinedInterface(d) => is_third_party_package(&d.package),
        Typ::DefinedTyp(d) => is_third_party_package(&d.package),
        _ => false,
    }
}

pub fn is_third_party_package(package: &str) -> bool {
    INTERNAL_PACKAGE_MAP.with(|packages| {
        for p in packages.borrow().iter() {
            if p == package {
                return false;
            }
        }
        true
    })
}
