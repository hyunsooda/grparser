use crate::metadata::{FuncSig, Metadata};
use crate::scope::{self, InstrScope};
use crate::typ::Typ;
use crate::value::Value;
use std::fmt::Debug;

use grmacro::Instruction;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum InstrTyp {
    UnsupportedInstr,
    BlockEntryInstr,
    FuncSigInstr,
    IdentInstr,
    AssignmentInstr,
    MapAssignmentInstr,
    IfInstr,
    LiteralValueInstr,
    FuncCallInstr,
    JumpInstr,
    ExtractInstr,
    ReturnInstr,
    AllocInstr,
    IndexInstr,
    MakeAssignInstr,
    ChangeInterfaceInstr,
    MakeCallInstr,
    SliceInstr,
    DerefInstr,
    PhiInstr,
    SendInstr,
    UnaryExprInstr,
    BinaryExprInstr,
    TypeAssertInstr,
    ConvertInstr,
    InvokeInstr,
    RangeInstr,
    NextInstr,
    GetValueChanInstr,
    SendValueChanInstr,
    GoRoutineInstr,
    SelectInstr,
    FieldAccessInstr,
    ChangetypeInstr,
    MembershipTestInstr,
    ClosureInstr,
    PanicInstr,
    SliceToArrayPtrInstr,
}

#[derive(Instruction, Debug, Clone, PartialEq, Eq, Hash)]
pub enum Instruction {
    Unsupported(UnsupportedInstr),
    BlockEntry(BlockEntryInstr),
    FuncSig(FuncSigInstr),
    Ident(IdentInstr),
    Assignment(AssignmentInstr),
    MapAssignment(MapAssignmentInstr),
    If(IfInstr),
    LiteralValue(LiteralValueInstr),
    FuncCall(FuncCallInstr),
    Jump(JumpInstr),
    Extract(ExtractInstr),
    Return(ReturnInstr),
    Alloc(AllocInstr),
    Index(IndexInstr),
    AnyOrInterfaceAssign(MakeAssignInstr),
    ChangeInterfaceAssign(ChangeInterfaceInstr),
    MakeCall(MakeCallInstr),
    Slice(SliceInstr),
    Deref(DerefInstr),
    Phi(PhiInstr),
    Send(SendInstr),
    UnaryExpr(UnaryExprInstr),
    BinaryExpr(BinaryExprInstr),
    TypeAssert(TypeAssertInstr),
    Convert(ConvertInstr),
    Invoke(InvokeInstr),
    Range(RangeInstr),
    Next(NextInstr),
    GetValueChan(GetValueChanInstr),
    SendValueChan(SendValueChanInstr),
    GoRoutine(GoRoutineInstr),
    Select(SelectInstr),
    FieldAccess(FieldAccessInstr),
    Changetype(ChangetypeInstr),
    MembershipTest(MembershipTestInstr),
    Closure(ClosureInstr),
    Panic(PanicInstr),
    SliceToArrayPtr(SliceToArrayPtrInstr),
}
#[allow(non_snake_case)]
impl Instruction {
    pub fn dump(&self) {
        println!("{:?}", self);
    }
    pub fn to_string(&self) -> String {
        format!("{:?}", self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum UnaryExprSym {
    SUB,
    NOT,
    BITWISECMPL,
}
impl UnaryExprSym {
    pub fn from_str(sym: &char) -> Self {
        match sym {
            '-' => Self::SUB,
            '!' => Self::NOT,
            '^' => Self::BITWISECMPL,
            _ => unreachable!(),
        }
    }
    pub fn to_string(&self) -> String {
        match self {
            Self::SUB => "-".to_string(),
            Self::NOT => "!".to_string(),
            Self::BITWISECMPL => "^".to_string(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum BinaryExprSym {
    ADD,
    SUB,
    MUL,
    DIV,

    EQ,
    NEQ,
    GT,
    GE,
    LT,
    LE,

    AND,
    OR,

    BITAND,
    BITOR,
    BITXOR,

    SHL,
    SHR,
}
impl BinaryExprSym {
    pub fn from_str(sym: &str) -> Self {
        match sym {
            "+" => Self::ADD,
            "-" => Self::SUB,
            "*" => Self::MUL,
            "/" => Self::DIV,

            "==" => Self::EQ,
            "!=" => Self::NEQ,
            ">" => Self::GT,
            ">=" => Self::GE,
            "<" => Self::LT,
            "<=" => Self::LE,

            "&&" => Self::AND,
            "||" => Self::OR,

            "&" => Self::BITAND,
            "|" => Self::BITOR,
            "^" => Self::BITXOR,

            "<<" => Self::SHL,
            ">>" => Self::SHR,

            _ => unreachable!(),
        }
    }
    pub fn to_string(&self) -> String {
        match self {
            Self::ADD => "+".to_string(),
            Self::SUB => "-".to_string(),
            Self::MUL => "*".to_string(),
            Self::DIV => "/".to_string(),
            Self::EQ => "==".to_string(),
            Self::NEQ => "!=".to_string(),
            Self::GT => ">".to_string(),
            Self::GE => ">=".to_string(),
            Self::LT => "<".to_string(),
            Self::LE => "<=".to_string(),
            Self::AND => "&&".to_string(),
            Self::OR => "||".to_string(),
            Self::BITAND => "&".to_string(),
            Self::BITOR => "|".to_string(),
            Self::BITXOR => "^".to_string(),
            Self::SHL => "<<".to_string(),
            Self::SHR => ">>".to_string(),
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UnsupportedInstr {
    pub instr_typ: InstrTyp,
    pub instr_scope: InstrScope,
    pub metadata: Vec<Option<Metadata>>,
    pub raw_ir: String,
}
impl UnsupportedInstr {
    pub fn new(raw_ir: &str) -> Self {
        Self {
            instr_typ: InstrTyp::UnsupportedInstr,
            instr_scope: scope::get_cur_scope(),
            metadata: vec![],
            raw_ir: raw_ir.to_string(),
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FuncSigInstr {
    pub instr_typ: InstrTyp,
    pub instr_scope: InstrScope,
    pub func_sig: FuncSig,
    pub metadata: Vec<Option<Metadata>>,
    pub raw_ir: String,
}
impl FuncSigInstr {
    pub fn new(func_sig: FuncSig, raw_ir: &str) -> Self {
        Self {
            instr_typ: InstrTyp::FuncSigInstr,
            instr_scope: scope::get_cur_scope(),
            func_sig,
            metadata: vec![],
            raw_ir: raw_ir.to_string(),
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BlockEntryInstr {
    pub instr_typ: InstrTyp,
    pub instr_scope: InstrScope,
    pub blk_num: u64,
    pub entry_info: String,
    pub metadata: Vec<Option<Metadata>>,
    pub raw_ir: String,
}
impl BlockEntryInstr {
    pub fn new(blk_num: u64, entry_info: &str, raw_ir: &str) -> Self {
        Self {
            instr_typ: InstrTyp::BlockEntryInstr,
            instr_scope: scope::get_cur_scope(),
            blk_num,
            entry_info: entry_info.to_string(),
            metadata: vec![],
            raw_ir: raw_ir.to_string(),
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IdentInstr {
    pub instr_typ: InstrTyp,
    pub instr_scope: InstrScope,
    pub ident: Value,
    pub metadata: Vec<Option<Metadata>>,
    pub raw_ir: String,
}
impl IdentInstr {
    pub fn new(ident: Value, raw_ir: &str) -> Self {
        Self {
            instr_typ: InstrTyp::IdentInstr,
            instr_scope: scope::get_cur_scope(),
            ident,
            metadata: vec![],
            raw_ir: raw_ir.to_string(),
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DerefInstr {
    pub instr_typ: InstrTyp,
    pub instr_scope: InstrScope,
    pub ident: Value,
    pub n_derefs: usize, // # of dereference
    pub typ: Typ,
    pub metadata: Vec<Option<Metadata>>,
    pub raw_ir: String,
}
impl DerefInstr {
    pub fn new(ident: Value, n_derefs: usize, typ: Typ, raw_ir: &str) -> Self {
        Self {
            instr_typ: InstrTyp::DerefInstr,
            instr_scope: scope::get_cur_scope(),
            ident,
            n_derefs,
            typ,
            metadata: vec![],
            raw_ir: raw_ir.to_string(),
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AssignmentInstr {
    pub instr_typ: InstrTyp,
    pub instr_scope: InstrScope,
    pub lhs: Value,
    pub is_lhs_deref: bool,
    pub rhs_expr: Box<Instruction>,
    pub metadata: Vec<Option<Metadata>>,
    pub raw_ir: String,
}
impl AssignmentInstr {
    pub fn new(lhs: Value, is_lhs_deref: bool, rhs_expr: Box<Instruction>, raw_ir: &str) -> Self {
        Self {
            instr_typ: InstrTyp::AssignmentInstr,
            instr_scope: scope::get_cur_scope(),
            lhs,
            is_lhs_deref,
            rhs_expr,
            metadata: vec![],
            raw_ir: raw_ir.to_string(),
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MapAssignmentInstr {
    pub instr_typ: InstrTyp,
    pub instr_scope: InstrScope,
    pub lhs: Value,
    pub is_lhs_deref: bool,
    pub key_value: Value,
    pub rhs_expr: Box<Instruction>,
    pub metadata: Vec<Option<Metadata>>,
    pub raw_ir: String,
}
impl MapAssignmentInstr {
    pub fn new(
        lhs: Value,
        is_lhs_deref: bool,
        key_value: Value,
        rhs_expr: Box<Instruction>,
        raw_ir: &str,
    ) -> Self {
        Self {
            instr_typ: InstrTyp::MapAssignmentInstr,
            instr_scope: scope::get_cur_scope(),
            lhs,
            is_lhs_deref,
            key_value,
            rhs_expr,
            metadata: vec![],
            raw_ir: raw_ir.to_string(),
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IfInstr {
    pub instr_typ: InstrTyp,
    pub instr_scope: InstrScope,
    pub cond_var: Value,
    pub tb_num: u64,
    pub fb_num: u64,
    pub metadata: Vec<Option<Metadata>>,
    pub raw_ir: String,
}
impl IfInstr {
    pub fn new(cond_var: Value, tb_num: u64, fb_num: u64, raw_ir: &str) -> Self {
        Self {
            instr_typ: InstrTyp::IfInstr,
            instr_scope: scope::get_cur_scope(),
            cond_var,
            tb_num,
            fb_num,
            metadata: vec![],
            raw_ir: raw_ir.to_string(),
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LiteralValueInstr {
    pub instr_typ: InstrTyp,
    pub instr_scope: InstrScope,
    pub typ: Typ,
    pub literal_value: Value,
    pub metadata: Vec<Option<Metadata>>,
    pub raw_ir: String,
}
impl LiteralValueInstr {
    pub fn new(typ: Typ, literal_value: Value, raw_ir: &str) -> Self {
        Self {
            instr_typ: InstrTyp::LiteralValueInstr,
            instr_scope: scope::get_cur_scope(),
            typ,
            literal_value,
            metadata: vec![],
            raw_ir: raw_ir.to_string(),
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FuncCallInstr {
    pub instr_typ: InstrTyp,
    pub instr_scope: InstrScope,
    pub package: String,
    pub receiver_typ: Option<Typ>,
    pub func_ident: Value,
    pub func_params: Vec<Value>,
    pub func_ret_typs: Vec<Typ>,
    pub is_defer: bool,
    pub metadata: Vec<Option<Metadata>>,
    pub raw_ir: String,
}
impl FuncCallInstr {
    pub fn new(
        package: String,
        receiver_typ: Option<Typ>,
        func_ident: Value,
        func_params: Vec<Value>,
        func_ret_typs: Vec<Typ>,
        is_defer: bool,
        raw_ir: &str,
    ) -> Self {
        Self {
            instr_typ: InstrTyp::FuncCallInstr,
            instr_scope: scope::get_cur_scope(),
            package,
            receiver_typ,
            func_ident,
            func_params,
            func_ret_typs,
            is_defer,
            metadata: vec![],
            raw_ir: raw_ir.to_string(),
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct JumpInstr {
    pub instr_typ: InstrTyp,
    pub instr_scope: InstrScope,
    pub blk_num: u64,
    pub metadata: Vec<Option<Metadata>>,
    pub raw_ir: String,
}
impl JumpInstr {
    pub fn new(blk_num: u64, raw_ir: &str) -> Self {
        Self {
            instr_typ: InstrTyp::JumpInstr,
            instr_scope: scope::get_cur_scope(),
            blk_num,
            metadata: vec![],
            raw_ir: raw_ir.to_string(),
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExtractInstr {
    pub instr_typ: InstrTyp,
    pub instr_scope: InstrScope,
    pub ident: Value,
    pub ret_typ: Typ,
    pub ret_pos: usize,
    pub metadata: Vec<Option<Metadata>>,
    pub raw_ir: String,
}
impl ExtractInstr {
    pub fn new(ident: Value, ret_typ: Typ, ret_pos: usize, raw_ir: &str) -> Self {
        Self {
            instr_typ: InstrTyp::ExtractInstr,
            instr_scope: scope::get_cur_scope(),
            ident,
            ret_typ,
            ret_pos,
            metadata: vec![],
            raw_ir: raw_ir.to_string(),
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ReturnInstr {
    pub instr_typ: InstrTyp,
    pub instr_scope: InstrScope,
    pub ret_values: Vec<Value>,
    pub metadata: Vec<Option<Metadata>>,
    pub raw_ir: String,
}
impl ReturnInstr {
    pub fn new(ret_values: Vec<Value>, raw_ir: &str) -> Self {
        Self {
            instr_typ: InstrTyp::ReturnInstr,
            instr_scope: scope::get_cur_scope(),
            ret_values,
            metadata: vec![],
            raw_ir: raw_ir.to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AllocSite {
    Local,
    Heap,
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AllocInstr {
    pub instr_typ: InstrTyp,
    pub instr_scope: InstrScope,
    pub typ: Typ,
    pub alloc_site: AllocSite,
    pub metadata: Vec<Option<Metadata>>,
    pub raw_ir: String,
}
impl AllocInstr {
    pub fn new(typ: Typ, alloc_site: AllocSite, raw_ir: &str) -> Self {
        Self {
            instr_typ: InstrTyp::AllocInstr,
            instr_scope: scope::get_cur_scope(),
            typ,
            alloc_site,
            metadata: vec![],
            raw_ir: raw_ir.to_string(),
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IndexInstr {
    pub instr_typ: InstrTyp,
    pub instr_scope: InstrScope,
    pub ident: Value,
    pub is_ref: bool,
    pub is_deref: bool,
    pub idx: Value,
    pub typ: Typ,
    pub metadata: Vec<Option<Metadata>>,
    pub raw_ir: String,
}
impl IndexInstr {
    pub fn new(
        ident: Value,
        is_ref: bool,
        is_deref: bool,
        idx: Value,
        typ: Typ,
        raw_ir: &str,
    ) -> Self {
        Self {
            instr_typ: InstrTyp::IndexInstr,
            instr_scope: scope::get_cur_scope(),
            ident,
            is_ref,
            is_deref,
            idx,
            typ,
            metadata: vec![],
            raw_ir: raw_ir.to_string(),
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MakeAssignInstr {
    pub instr_typ: InstrTyp,
    pub instr_scope: InstrScope,
    pub lhs_typ: Typ, // `any` or `interface`
    pub rhs_typ: Typ,
    pub rhs_value: Value,
    pub metadata: Vec<Option<Metadata>>,
    pub raw_ir: String,
}
impl MakeAssignInstr {
    pub fn new(lhs_typ: Typ, rhs_typ: Typ, rhs_value: Value, raw_ir: &str) -> Self {
        Self {
            instr_typ: InstrTyp::MakeAssignInstr,
            instr_scope: scope::get_cur_scope(),
            lhs_typ,
            rhs_typ,
            rhs_value,
            metadata: vec![],
            raw_ir: raw_ir.to_string(),
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MakeCallInstr {
    pub instr_typ: InstrTyp,
    pub instr_scope: InstrScope,
    pub typ: Typ,
    pub value: Value,
    pub metadata: Vec<Option<Metadata>>,
    pub raw_ir: String,
}
impl MakeCallInstr {
    pub fn new(typ: Typ, value: Value, raw_ir: &str) -> Self {
        Self {
            instr_typ: InstrTyp::MakeCallInstr,
            instr_scope: scope::get_cur_scope(),
            typ,
            value,
            metadata: vec![],
            raw_ir: raw_ir.to_string(),
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ChangeInterfaceInstr {
    pub instr_typ: InstrTyp,
    pub instr_scope: InstrScope,
    pub lhs_typ: Typ,
    pub rhs_typ: Typ,
    pub rhs_value: Value,
    pub metadata: Vec<Option<Metadata>>,
    pub raw_ir: String,
}
impl ChangeInterfaceInstr {
    pub fn new(lhs_typ: Typ, rhs_typ: Typ, rhs_value: Value, raw_ir: &str) -> Self {
        Self {
            instr_typ: InstrTyp::ChangeInterfaceInstr,
            instr_scope: scope::get_cur_scope(),
            lhs_typ,
            rhs_typ,
            rhs_value,
            metadata: vec![],
            raw_ir: raw_ir.to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Range {
    First,
    Idx(Value),
    End,
}
#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RangeValue {
    pub first: Range,
    pub end: Range,
}
impl RangeValue {
    pub fn new(first: Range, end: Range) -> Self {
        Self { first, end }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SliceInstr {
    pub instr_typ: InstrTyp,
    pub instr_scope: InstrScope,
    pub ident: Value,
    pub typ: Typ,
    pub range: RangeValue,
    pub metadata: Vec<Option<Metadata>>,
    pub raw_ir: String,
}
impl SliceInstr {
    pub fn new(ident: Value, typ: Typ, range: RangeValue, raw_ir: &str) -> Self {
        Self {
            instr_typ: InstrTyp::SliceInstr,
            instr_scope: scope::get_cur_scope(),
            ident,
            typ,
            range,
            metadata: vec![],
            raw_ir: raw_ir.to_string(),
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UnaryExprInstr {
    pub instr_typ: InstrTyp,
    pub instr_scope: InstrScope,
    pub unary_expr_sym: UnaryExprSym,
    pub ident: Value,
    pub result_typ: Typ,
    pub metadata: Vec<Option<Metadata>>,
    pub raw_ir: String,
}
impl UnaryExprInstr {
    pub fn new(unary_expr_sym: UnaryExprSym, ident: Value, result_typ: Typ, raw_ir: &str) -> Self {
        Self {
            instr_typ: InstrTyp::UnaryExprInstr,
            instr_scope: scope::get_cur_scope(),
            unary_expr_sym,
            ident,
            result_typ,
            metadata: vec![],
            raw_ir: raw_ir.to_string(),
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BinaryExprInstr {
    pub instr_typ: InstrTyp,
    pub instr_scope: InstrScope,
    pub binary_expr_sym: BinaryExprSym,
    pub lhs: Value,
    pub rhs: Value,
    pub result_typ: Typ,
    pub metadata: Vec<Option<Metadata>>,
    pub raw_ir: String,
}
impl BinaryExprInstr {
    pub fn new(
        binary_expr_sym: BinaryExprSym,
        lhs: Value,
        rhs: Value,
        result_typ: Typ,
        raw_ir: &str,
    ) -> Self {
        Self {
            instr_typ: InstrTyp::BinaryExprInstr,
            instr_scope: scope::get_cur_scope(),
            binary_expr_sym,
            lhs,
            rhs,
            result_typ,
            metadata: vec![],
            raw_ir: raw_ir.to_string(),
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PhiInstr {
    pub instr_typ: InstrTyp,
    pub instr_scope: InstrScope,
    pub ident_hint: Value,
    pub typ: Typ,
    pub blk1: u64,
    pub value1: Value,
    pub blk2: u64,
    pub value2: Value,
    pub metadata: Vec<Option<Metadata>>,
    pub raw_ir: String,
}
impl PhiInstr {
    pub fn new(
        ident_hint: Value,
        typ: Typ,
        blk1: u64,
        value1: Value,
        blk2: u64,
        value2: Value,
        raw_ir: &str,
    ) -> Self {
        Self {
            instr_typ: InstrTyp::PhiInstr,
            instr_scope: scope::get_cur_scope(),
            ident_hint,
            typ,
            blk1,
            value1,
            blk2,
            value2,
            metadata: vec![],
            raw_ir: raw_ir.to_string(),
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SendInstr {
    pub instr_typ: InstrTyp,
    pub instr_scope: InstrScope,
    pub ident: Value,
    pub value: Value,
    pub metadata: Vec<Option<Metadata>>,
    pub raw_ir: String,
}
impl SendInstr {
    pub fn new(ident: Value, value: Value, raw_ir: &str) -> Self {
        Self {
            instr_typ: InstrTyp::SendInstr,
            instr_scope: scope::get_cur_scope(),
            ident,
            value,
            metadata: vec![],
            raw_ir: raw_ir.to_string(),
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeAssertInstr {
    pub instr_typ: InstrTyp,
    pub instr_scope: InstrScope,
    pub ident: Value,
    pub assert_typ: Typ,
    pub ret_typs: Vec<Typ>,
    pub metadata: Vec<Option<Metadata>>,
    pub raw_ir: String,
}
impl TypeAssertInstr {
    pub fn new(ident: Value, assert_typ: Typ, ret_typs: Vec<Typ>, raw_ir: &str) -> Self {
        Self {
            instr_typ: InstrTyp::TypeAssertInstr,
            instr_scope: scope::get_cur_scope(),
            ident,
            assert_typ,
            ret_typs,
            metadata: vec![],
            raw_ir: raw_ir.to_string(),
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConvertInstr {
    pub instr_typ: InstrTyp,
    pub instr_scope: InstrScope,
    pub from_typ: Typ,
    pub to_typ: Typ,
    pub value: Value,
    pub ret_typ: Typ,
    pub metadata: Vec<Option<Metadata>>,
    pub raw_ir: String,
}
impl ConvertInstr {
    pub fn new(from_typ: Typ, to_typ: Typ, value: Value, ret_typ: Typ, raw_ir: &str) -> Self {
        Self {
            instr_typ: InstrTyp::ConvertInstr,
            instr_scope: scope::get_cur_scope(),
            from_typ,
            to_typ,
            value,
            ret_typ,
            metadata: vec![],
            raw_ir: raw_ir.to_string(),
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InvokeInstr {
    pub instr_typ: InstrTyp,
    pub instr_scope: InstrScope,
    pub interface_ident: Value,
    pub func_ident: Value,
    pub func_params: Vec<Value>,
    pub ret_typs: Vec<Typ>,
    pub metadata: Vec<Option<Metadata>>,
    pub raw_ir: String,
}
impl InvokeInstr {
    pub fn new(
        interface_ident: Value,
        func_ident: Value,
        func_params: Vec<Value>,
        ret_typs: Vec<Typ>,
        raw_ir: &str,
    ) -> Self {
        Self {
            instr_typ: InstrTyp::InvokeInstr,
            instr_scope: scope::get_cur_scope(),
            interface_ident,
            func_ident,
            func_params,
            ret_typs,
            metadata: vec![],
            raw_ir: raw_ir.to_string(),
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RangeInstr {
    pub instr_typ: InstrTyp,
    pub instr_scope: InstrScope,
    pub ident: Value,
    pub metadata: Vec<Option<Metadata>>,
    pub raw_ir: String,
}
impl RangeInstr {
    pub fn new(ident: Value, raw_ir: &str) -> Self {
        Self {
            instr_typ: InstrTyp::RangeInstr,
            instr_scope: scope::get_cur_scope(),
            ident,
            metadata: vec![],
            raw_ir: raw_ir.to_string(),
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NextInstr {
    pub instr_typ: InstrTyp,
    pub instr_scope: InstrScope,
    pub ident: Value,
    pub ret_typs: Vec<Typ>,
    pub metadata: Vec<Option<Metadata>>,
    pub raw_ir: String,
}
impl NextInstr {
    pub fn new(ident: Value, ret_typs: Vec<Typ>, raw_ir: &str) -> Self {
        Self {
            instr_typ: InstrTyp::NextInstr,
            instr_scope: scope::get_cur_scope(),
            ident,
            ret_typs,
            metadata: vec![],
            raw_ir: raw_ir.to_string(),
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GetValueChanInstr {
    pub instr_typ: InstrTyp,
    pub instr_scope: InstrScope,
    pub ident: Value,
    pub ret_typs: Vec<Typ>,
    pub metadata: Vec<Option<Metadata>>,
    pub raw_ir: String,
}
impl GetValueChanInstr {
    pub fn new(ident: Value, ret_typs: Vec<Typ>, raw_ir: &str) -> Self {
        Self {
            instr_typ: InstrTyp::GetValueChanInstr,
            instr_scope: scope::get_cur_scope(),
            ident,
            ret_typs,
            metadata: vec![],
            raw_ir: raw_ir.to_string(),
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GoRoutineInstr {
    pub instr_typ: InstrTyp,
    pub instr_scope: InstrScope,
    pub func_ident: Value,
    pub func_params: Vec<Value>,
    pub metadata: Vec<Option<Metadata>>,
    pub raw_ir: String,
}
impl GoRoutineInstr {
    pub fn new(func_ident: Value, func_params: Vec<Value>, raw_ir: &str) -> Self {
        Self {
            instr_typ: InstrTyp::GoRoutineInstr,
            instr_scope: scope::get_cur_scope(),
            func_ident,
            func_params,
            metadata: vec![],
            raw_ir: raw_ir.to_string(),
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SelectInstr {
    pub instr_typ: InstrTyp,
    pub instr_scope: InstrScope,
    pub chan_exprs: Vec<Box<Instruction>>,
    pub ret_typs: Vec<Typ>,
    pub is_blocking: bool,
    pub metadata: Vec<Option<Metadata>>,
    pub raw_ir: String,
}
impl SelectInstr {
    pub fn new(
        chan_exprs: Vec<Box<Instruction>>,
        ret_typs: Vec<Typ>,
        is_blocking: bool,
        raw_ir: &str,
    ) -> Self {
        Self {
            instr_typ: InstrTyp::SelectInstr,
            instr_scope: scope::get_cur_scope(),
            chan_exprs,
            ret_typs,
            is_blocking,
            metadata: vec![],
            raw_ir: raw_ir.to_string(),
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SendValueChanInstr {
    pub instr_typ: InstrTyp,
    pub instr_scope: InstrScope,
    pub ident: Value,
    pub value: Value,
    pub metadata: Vec<Option<Metadata>>,
    pub raw_ir: String,
}
impl SendValueChanInstr {
    pub fn new(ident: Value, value: Value, raw_ir: &str) -> Self {
        Self {
            instr_typ: InstrTyp::SendValueChanInstr,
            instr_scope: scope::get_cur_scope(),
            ident,
            value,
            metadata: vec![],
            raw_ir: raw_ir.to_string(),
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FieldAccessInstr {
    pub instr_typ: InstrTyp,
    pub instr_scope: InstrScope,
    pub ident: Value,
    pub typ: Typ,
    pub is_ref: bool,
    pub field_ident: Value,
    pub field_idx: usize,
    pub metadata: Vec<Option<Metadata>>,
    pub raw_ir: String,
}
impl FieldAccessInstr {
    pub fn new(
        ident: Value,
        typ: Typ,
        is_ref: bool,
        field_ident: Value,
        field_idx: usize,
        raw_ir: &str,
    ) -> Self {
        Self {
            instr_typ: InstrTyp::FieldAccessInstr,
            instr_scope: scope::get_cur_scope(),
            ident,
            typ,
            is_ref,
            field_ident,
            field_idx,
            metadata: vec![],
            raw_ir: raw_ir.to_string(),
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ChangetypeInstr {
    pub instr_typ: InstrTyp,
    pub instr_scope: InstrScope,
    pub from_typ: Typ,
    pub to_typ: Typ,
    pub value: Value,
    pub metadata: Vec<Option<Metadata>>,
    pub raw_ir: String,
}
impl ChangetypeInstr {
    pub fn new(from_typ: Typ, to_typ: Typ, value: Value, raw_ir: &str) -> Self {
        Self {
            instr_typ: InstrTyp::ChangetypeInstr,
            instr_scope: scope::get_cur_scope(),
            from_typ,
            to_typ,
            value,
            metadata: vec![],
            raw_ir: raw_ir.to_string(),
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MembershipTestInstr {
    pub instr_typ: InstrTyp,
    pub instr_scope: InstrScope,
    pub ident: Value,
    pub key_value: Value,
    pub ret_typs: Vec<Typ>,
    pub metadata: Vec<Option<Metadata>>,
    pub raw_ir: String,
}
impl MembershipTestInstr {
    pub fn new(ident: Value, key_value: Value, ret_typs: Vec<Typ>, raw_ir: &str) -> Self {
        Self {
            instr_typ: InstrTyp::MembershipTestInstr,
            instr_scope: scope::get_cur_scope(),
            ident,
            key_value,
            ret_typs,
            metadata: vec![],
            raw_ir: raw_ir.to_string(),
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ClosureInstr {
    pub instr_typ: InstrTyp,
    pub instr_scope: InstrScope,
    pub func_ident: Value,
    pub referrers: Vec<Value>,
    pub closure_sig: Typ,
    pub metadata: Vec<Option<Metadata>>,
    pub raw_ir: String,
}
impl ClosureInstr {
    pub fn new(func_ident: Value, referrers: Vec<Value>, closure_sig: Typ, raw_ir: &str) -> Self {
        Self {
            instr_typ: InstrTyp::ClosureInstr,
            instr_scope: scope::get_cur_scope(),
            func_ident,
            referrers,
            closure_sig,
            metadata: vec![],
            raw_ir: raw_ir.to_string(),
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PanicInstr {
    pub instr_typ: InstrTyp,
    pub instr_scope: InstrScope,
    pub value: Value,
    pub metadata: Vec<Option<Metadata>>,
    pub raw_ir: String,
}
impl PanicInstr {
    pub fn new(value: Value, raw_ir: &str) -> Self {
        Self {
            instr_typ: InstrTyp::PanicInstr,
            instr_scope: scope::get_cur_scope(),
            value,
            metadata: vec![],
            raw_ir: raw_ir.to_string(),
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SliceToArrayPtrInstr {
    pub instr_typ: InstrTyp,
    pub instr_scope: InstrScope,
    pub from_typ: Typ,
    pub to_typ: Typ,
    pub value: Value,
    pub metadata: Vec<Option<Metadata>>,
    pub raw_ir: String,
}
impl SliceToArrayPtrInstr {
    pub fn new(from_typ: Typ, to_typ: Typ, value: Value, raw_ir: &str) -> Self {
        Self {
            instr_typ: InstrTyp::SliceToArrayPtrInstr,
            instr_scope: scope::get_cur_scope(),
            from_typ,
            to_typ,
            value,
            metadata: vec![],
            raw_ir: raw_ir.to_string(),
        }
    }
}
