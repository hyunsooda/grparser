use crate::instr::BinaryExprSym;
use crate::parse;
use crate::typ::{self, Typ};
use ordered_float::OrderedFloat;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Value {
    Ident(String),
    VaridicIdent(String),
    LiteralValue(LiteralValue),
}

impl Value {
    pub fn new_literal_value(typ: &Typ, value_str: &str) -> Self {
        Value::LiteralValue(LiteralValue::new(typ, value_str))
    }
    pub fn new_ident(ident: &str) -> Self {
        Value::Ident(ident.to_string())
    }
    pub fn new_variadic_ident(ident: &str) -> Self {
        Value::VaridicIdent(ident.to_string())
    }
    pub fn get_ident_str(&self) -> Option<&str> {
        if let Self::Ident(ident) = self {
            Some(ident)
        } else {
            None
        }
    }
    pub fn get_literal_value(&self) -> Option<&LiteralValue> {
        if let Self::LiteralValue(value) = self {
            Some(value)
        } else {
            None
        }
    }
    pub fn get_variadic_ident_str(&self) -> Option<&str> {
        if let Self::VaridicIdent(ident) = self {
            Some(ident)
        } else {
            None
        }
    }
    pub fn to_string(&self) -> String {
        match self {
            Self::Ident(v) => v.clone(),
            Self::VaridicIdent(v) => v.clone(),
            Self::LiteralValue(v) => v.to_string(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum LiteralValue {
    Int(i64),
    Int8(i8),
    Int16(i16),
    Int32(i32),
    Int64(i64),
    Uint(u64),
    Uint8(u8),
    Uint16(u16),
    Uint32(u32),
    Uint64(u64),
    Uintptr(u64),
    Byte(u8),
    Rune(i32),
    Complex64(BinaryExprSym, OrderedFloat<f32>, OrderedFloat<f32>),
    Complex128(BinaryExprSym, OrderedFloat<f64>, OrderedFloat<f64>),
    Float32(OrderedFloat<f32>),
    Float64(OrderedFloat<f64>),
    Bool(bool),
    Error(Error),
    String(String),
    AnyNil(NilTyp),
    UnsafePointerNil(NilTyp),
    InterfaceNil(NilTyp),
    StructNil(NilTyp),
    ChanNil(NilTyp),
    ArrayNil(typ::Array),
    SliceNil(typ::Slice),
    DefinedTypLiteral(String),
}

impl LiteralValue {
    pub fn new(typ: &Typ, value_str: &str) -> Self {
        match typ {
            Typ::Int => Self::Int(value_str.parse::<i64>().unwrap()),
            Typ::Int8 => Self::Int8(value_str.parse::<i8>().unwrap()),
            Typ::Int16 => Self::Int16(value_str.parse::<i16>().unwrap()),
            Typ::Int32 => Self::Int32(value_str.parse::<i32>().unwrap()),
            Typ::Int64 => Self::Int64(value_str.parse::<i64>().unwrap()),
            Typ::Uint => Self::Uint(value_str.parse::<u64>().unwrap()),
            Typ::Uint8 => Self::Uint8(value_str.parse::<u8>().unwrap()),
            Typ::Uint16 => Self::Uint16(value_str.parse::<u16>().unwrap()),
            Typ::Uint32 => Self::Uint32(value_str.parse::<u32>().unwrap()),
            Typ::Uint64 => Self::Uint64(value_str.parse::<u64>().unwrap()),
            Typ::Uintptr => Self::Uintptr(value_str.parse::<u64>().unwrap()),
            Typ::Byte => Self::Byte(value_str.parse::<u8>().unwrap()),
            Typ::Rune => Self::Rune(value_str.parse::<i32>().unwrap()),
            Typ::Complex64 => {
                let (sym, real, img) = parse::decomposite_complex64(value_str);
                assert_eq!(sym, BinaryExprSym::ADD);
                Self::Complex64(sym, OrderedFloat(real), OrderedFloat(img))
            }
            Typ::Complex128 => {
                let (sym, real, img) = parse::decomposite_complex128(value_str);
                assert_eq!(sym, BinaryExprSym::ADD);
                Self::Complex128(sym, OrderedFloat(real), OrderedFloat(img))
            }
            Typ::Float32 => Self::Float32(OrderedFloat(value_str.parse::<f32>().unwrap())),
            Typ::Float64 => Self::Float64(OrderedFloat(value_str.parse::<f64>().unwrap())),
            Typ::Bool => Self::Bool(value_str.parse::<bool>().unwrap()),
            Typ::Error => Self::Error(Error::new(value_str)),
            Typ::String => Self::String(value_str.to_string()),
            Typ::Any => Self::AnyNil(NilTyp::AnyNil),
            Typ::UnsafePointer => Self::UnsafePointerNil(NilTyp::UnsafePointerNil),
            Typ::Interface => Self::InterfaceNil(NilTyp::InterfaceNil),
            Typ::Struct(_) => Self::StructNil(NilTyp::StructNil),
            Typ::Chan(_) => Self::ChanNil(NilTyp::ChanNil),
            Typ::Array(arr) => Self::ArrayNil(arr.clone()),
            Typ::Slice(slice) => Self::SliceNil(slice.clone()),
            _ => Self::DefinedTypLiteral(value_str.to_string()),
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            Self::DefinedTypLiteral(s) => s.to_string(),
            Self::Int(v) => v.to_string(),
            Self::Int8(v) => v.to_string(),
            Self::Int16(v) => v.to_string(),
            Self::Int32(v) => v.to_string(),
            Self::Int64(v) => v.to_string(),
            Self::Uint(v) => v.to_string(),
            Self::Uint8(v) => v.to_string(),
            Self::Uint16(v) => v.to_string(),
            Self::Uint32(v) => v.to_string(),
            Self::Uint64(v) => v.to_string(),
            Self::Uintptr(v) => v.to_string(),
            Self::Byte(v) => v.to_string(),
            Self::Rune(v) => v.to_string(),
            Self::Complex64(sym, real, img) => format!(
                "({} {} {}i)",
                real.to_string(),
                sym.to_string(),
                img.to_string()
            ),
            Self::Complex128(sym, real, img) => format!(
                "({} {} {}i)",
                real.to_string(),
                sym.to_string(),
                img.to_string()
            ),
            Self::Float32(v) => v.to_string(),
            Self::Float64(v) => v.to_string(),
            Self::Bool(v) => v.to_string(),
            Self::Error(v) => match v {
                Error::Error(v) => v.clone(),
                Error::Nil => "Nil".to_string(),
            },
            Self::String(v) => v.clone(),
            Self::AnyNil(_)
            | Self::UnsafePointerNil(_)
            | Self::ArrayNil(_)
            | Self::SliceNil(_)
            | Self::InterfaceNil(_)
            | Self::StructNil(_)
            | Self::ChanNil(_) => "nil".to_string(),
        }
    }

    pub fn get_int(&self) -> i64 {
        match self {
            Self::Int(v) => *v,
            _ => unreachable!("Expect i64 type"),
        }
    }
    pub fn get_int8(&self) -> i8 {
        match self {
            Self::Int8(v) => v.clone(),
            _ => unreachable!("Expect i8 type"),
        }
    }
    pub fn get_int16(&self) -> i16 {
        match self {
            Self::Int16(v) => *v,
            _ => unreachable!("Expect i16 type"),
        }
    }
    pub fn get_int32(&self) -> i32 {
        match self {
            Self::Int32(v) => *v,
            _ => unreachable!("Expect i32 type"),
        }
    }
    pub fn get_int64(&self) -> i64 {
        match self {
            Self::Int64(v) => *v,
            _ => unreachable!("Expect i64 type"),
        }
    }

    pub fn get_uint(&self) -> u64 {
        match self {
            Self::Uint(v) => *v,
            _ => unreachable!("Expect u64 type"),
        }
    }
    pub fn get_uint8(&self) -> u8 {
        match self {
            Self::Uint8(v) => *v,
            _ => unreachable!("Expect u8 type"),
        }
    }
    pub fn get_uint16(&self) -> u16 {
        match self {
            Self::Uint16(v) => *v,
            _ => unreachable!("Expect u16 type"),
        }
    }
    pub fn get_uint32(&self) -> u32 {
        match self {
            Self::Uint32(v) => *v,
            _ => unreachable!("Expect u32 type"),
        }
    }
    pub fn get_uint64(&self) -> u64 {
        match self {
            Self::Uint64(v) => *v,
            _ => unreachable!("Expect u64 type"),
        }
    }
    pub fn get_uintptr(&self) -> u64 {
        match self {
            Self::Uintptr(v) => *v,
            _ => unreachable!("Expect u64 type"),
        }
    }
    pub fn get_byte(&self) -> u8 {
        match self {
            Self::Byte(v) => *v,
            _ => unreachable!("Expect u8 type"),
        }
    }
    pub fn get_rune(&self) -> i32 {
        match self {
            Self::Rune(v) => *v,
            _ => unreachable!("Expect i32 type"),
        }
    }
    pub fn get_complex64(&self) -> (BinaryExprSym, f32, f32) {
        match self {
            Self::Complex64(sym, real, img) => (sym.clone(), real.into_inner(), img.into_inner()),
            _ => unreachable!("Expect complex64 type"),
        }
    }
    pub fn get_complex128(&self) -> (BinaryExprSym, f64, f64) {
        match self {
            Self::Complex128(sym, real, img) => (sym.clone(), real.into_inner(), img.into_inner()),
            _ => unreachable!("Expect complex128 type"),
        }
    }
    pub fn get_float32(&self) -> f32 {
        match self {
            Self::Float32(v) => v.into_inner(),
            _ => unreachable!("Expect f32 type"),
        }
    }
    pub fn get_float64(&self) -> f64 {
        match self {
            Self::Float64(v) => v.into_inner(),
            _ => unreachable!("Expect f64 type"),
        }
    }
    pub fn get_bool(&self) -> bool {
        match self {
            Self::Bool(v) => *v,
            _ => unreachable!("Expect bool type"),
        }
    }
    pub fn get_error(&self) -> Error {
        match self {
            Self::Error(v) => v.clone(),
            _ => unreachable!("Expect Error type"),
        }
    }
    pub fn get_string(&self) -> String {
        match self {
            Self::String(v) => v.clone(),
            _ => unreachable!("Expect String type"),
        }
    }
    pub fn get_any_nil(&self) -> NilTyp {
        match self {
            Self::AnyNil(v) => v.clone(),
            _ => unreachable!("Expect AnyNil type"),
        }
    }
    pub fn get_unsafe_ptr_nil(&self) -> NilTyp {
        match self {
            Self::UnsafePointerNil(v) => v.clone(),
            _ => unreachable!("Expect UnsafePointerNil type"),
        }
    }
    pub fn get_interface_nil(&self) -> NilTyp {
        match self {
            Self::InterfaceNil(v) => v.clone(),
            _ => unreachable!("Expect InterfaceNil type"),
        }
    }
    pub fn get_struct_nil(&self) -> NilTyp {
        match self {
            Self::StructNil(v) => v.clone(),
            _ => unreachable!("Expect StructNil type"),
        }
    }
    pub fn get_chan_nil(&self) -> NilTyp {
        match self {
            Self::ChanNil(v) => v.clone(),
            _ => unreachable!("Expect ChanNil type"),
        }
    }
    pub fn get_array_nil(&self) -> typ::Array {
        match self {
            Self::ArrayNil(v) => v.clone(),
            _ => unreachable!("Expect ArrayNil type"),
        }
    }
    pub fn get_slice_nil(&self) -> typ::Slice {
        match self {
            Self::SliceNil(v) => v.clone(),
            _ => unreachable!("Expect SliceNil type"),
        }
    }
    pub fn get_defined_typ_literal(&self) -> String {
        match self {
            Self::DefinedTypLiteral(v) => v.clone(),
            _ => unreachable!("Expect String type"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NilTyp {
    AnyNil,
    UnsafePointerNil,
    InterfaceNil,
    StructNil,
    ChanNil,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Error {
    Error(String),
    Nil,
}
impl Error {
    fn new(value_str: &str) -> Self {
        if value_str == "nil" {
            Self::Nil
        } else {
            Self::Error(value_str.to_string())
        }
    }
}
