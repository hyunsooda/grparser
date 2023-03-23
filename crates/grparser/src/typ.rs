use crate::collection;
use crate::parse;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Typ {
    Empty,
    Int,
    Int8,
    Int16,
    Int32,
    Int64,
    Uint,
    Uint8,
    Uint16,
    Uint32,
    Uint64,
    Uintptr,
    Byte,
    Rune,
    Complex64,
    Complex128,
    Float32,
    Float64,
    Bool,
    Error,
    Any,
    UnsafePointer,
    String,
    Ptr(Box<Typ>),
    Array(Array),
    Slice(Slice),
    Variadic(Box<Typ>),
    Interface,
    Chan(Chan),
    Map((Box<Typ>, Box<Typ>)),               // (key type, value type)
    FuncPtr((Box<Vec<Typ>>, Box<Vec<Typ>>)), // (func param types, func ret types)

    EmptyStruct,
    Struct(Struct),
    DefinedInterface(DefinedInterface),

    DefinedTyp(DefinedTyp), // Type alias, struct, defined interface
}

impl Typ {
    pub fn from_str(input: &str) -> Self {
        match input {
            "int" => Self::Int,
            "int8" => Self::Int8,
            "int16" => Self::Int16,
            "int32" => Self::Int32,
            "int64" => Self::Int64,
            "uint" => Self::Uint,
            "uint8" => Self::Uint8,
            "uint16" => Self::Uint16,
            "uint32" => Self::Uint32,
            "uint64" => Self::Uint64,
            "uintptr" => Self::Uintptr,
            "byte" => Self::Byte,
            "rune" => Self::Rune,
            "complex64" => Self::Complex64,
            "complex128" => Self::Complex128,
            "float32" => Self::Float32,
            "float64" => Self::Float64,
            "bool" => Self::Bool,
            "error" => Self::Error,
            "any" => Self::Any,
            "unsafe.Pointer" => Self::UnsafePointer,
            "string" => Self::String,
            "interface{}" => Self::Interface,
            "struct{}" => Self::EmptyStruct,
            "()" => Self::Empty,
            _ => {
                if input.len() > 0 && input.chars().nth(0).unwrap() == '*' {
                    // Ptr
                    let typ = Self::from_str(&input[1..]);
                    Self::Ptr(Box::new(typ))
                } else if input.len() > 0 && input.chars().nth(0).unwrap() == '[' {
                    let (typ, len) = parse::decomposite_slice_and_array_typ(input);
                    if len == Length::Unknown {
                        // Slice
                        Self::Slice(Slice::new(&typ, len))
                    } else {
                        // Array (fixed size)
                        Self::Array(Array::new(&typ, len))
                    }
                } else if input.contains("...") {
                    // Variadic
                    let typ = Self::from_str(&input[..input.len() - 3]);
                    Self::Variadic(Box::new(typ))
                } else if input.starts_with("chan ")
                    || input.starts_with("<-")
                    || input.starts_with("chan<- ")
                {
                    // Channel
                    let (chan_typ, typ) = parse::decomposite_chan(input);
                    Self::Chan(Chan::new(chan_typ, typ))
                } else if input.starts_with("map[") {
                    // Map
                    let (n_ptr, key_typ, value_typ) = parse::decomposite_map_typ(input);
                    let mut m = Self::Map((Box::new(key_typ), Box::new(value_typ)));
                    for _ in n_ptr {
                        m = Self::Ptr(Box::new(m));
                    }
                    m
                } else if input.starts_with("func") {
                    // function pointer
                    let (n_ptr, param_typs, ret_typs) = parse::decomposite_func_ptr(input);
                    let mut func_ptr = Self::FuncPtr((Box::new(param_typs), Box::new(ret_typs)));
                    for _ in n_ptr {
                        func_ptr = Self::Ptr(Box::new(func_ptr));
                    }
                    func_ptr
                } else if input.starts_with("struct{") {
                    // literal struct type
                    let (_, s) = parse::parse_struct_literal(input).unwrap();
                    s
                } else {
                    Self::DefinedTyp(DefinedTyp::new(input))
                }
            }
        }
    }

    pub fn get_inner_typ_of_ptr(&self) -> &Self {
        match self {
            Self::Empty
            | Self::Int
            | Self::Int8
            | Self::Int16
            | Self::Int32
            | Self::Int64
            | Self::Uint
            | Self::Uint8
            | Self::Uint16
            | Self::Uint32
            | Self::Uint64
            | Self::Uintptr
            | Self::Byte
            | Self::Rune
            | Self::Complex64
            | Self::Complex128
            | Self::Float32
            | Self::Float64
            | Self::Bool
            | Self::Error
            | Self::Any
            | Self::UnsafePointer
            | Self::String
            | Self::Interface
            | Self::Variadic(_)
            | Self::Array(_)
            | Self::Slice(_)
            | Self::Chan(_)
            | Self::Map(_)
            | Self::FuncPtr(_)
            | Self::DefinedInterface(_)
            | Self::DefinedTyp(_)
            | Self::EmptyStruct
            | Self::Struct(_) => self,
            Self::Ptr(ptr) => match **ptr {
                Self::Ptr(ref ptr_) => &*ptr_.get_inner_typ_of_ptr(),
                _ => ptr,
            },
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            Self::Empty => "".to_string(),
            Self::Int => "int".to_string(),
            Self::Int8 => "int8".to_string(),
            Self::Int16 => "int16".to_string(),
            Self::Int32 => "int32".to_string(),
            Self::Int64 => "int64".to_string(),
            Self::Uint => "uint".to_string(),
            Self::Uint8 => "uint8".to_string(),
            Self::Uint16 => "uint16".to_string(),
            Self::Uint32 => "uint32".to_string(),
            Self::Uint64 => "uint64".to_string(),
            Self::Uintptr => "uintptr".to_string(),
            Self::Byte => "byte".to_string(),
            Self::Rune => "rune".to_string(),
            Self::Complex64 => "complex64".to_string(),
            Self::Complex128 => "complex128".to_string(),
            Self::Float32 => "float32".to_string(),
            Self::Float64 => "float64".to_string(),
            Self::Bool => "bool".to_string(),
            Self::Error => "error".to_string(),
            Self::Any => "any".to_string(),
            Self::UnsafePointer => "unsafe.Pointer".to_string(),
            Self::String => "string".to_string(),
            Self::Interface => "interface{}".to_string(),
            Self::EmptyStruct => "struct{}".to_string(),
            Self::DefinedInterface(i) => i.to_string(),
            Self::Map((key_typ, value_typ)) => {
                format!("map[{}]{}", key_typ.to_string(), value_typ.to_string())
            }
            Self::Ptr(ptr) => {
                let mut ptr_str = "".to_string();
                let mut ptr = ptr.clone();
                loop {
                    match *ptr.clone() {
                        Self::Ptr(ptr_) => {
                            ptr_str = format!("{}*", ptr_str);
                            ptr = ptr_;
                        }
                        typ => {
                            ptr_str = format!("{}*{}", ptr_str, typ.to_string());
                            break;
                        }
                    }
                }
                ptr_str
            }
            Self::FuncPtr((param_typs, ret_typs)) => format!(
                "func({}) ({})",
                parse::typs_to_string(param_typs),
                parse::typs_to_string(ret_typs)
            ),
            Self::Slice(slice) => slice.to_string(),
            Self::Array(array) => array.to_string(),
            Self::Variadic(t) => t.to_string(),
            Self::Chan(chan) => chan.to_string(),
            Self::Struct(s) => s.to_string(),
            Self::DefinedTyp(d) => d.to_string(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Length {
    Unknown,
    Len(usize),
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Slice {
    pub typ: Box<Typ>,
    pub length: Length,
}
impl Slice {
    pub fn new(typ: &Typ, length: Length) -> Self {
        Self {
            typ: Box::new(typ.clone()),
            length,
        }
    }
    fn to_string(&self) -> String {
        if let Length::Len(l) = self.length {
            format!("[{}]{}", self.typ.to_string(), l)
        } else {
            format!("[]{}", self.typ.to_string())
        }
    }
}

// `Array` and `Slice` are the same implementation, only names are different.
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Array {
    pub typ: Box<Typ>,
    pub length: Length,
}
impl Array {
    pub fn new(typ: &Typ, length: Length) -> Self {
        Self {
            typ: Box::new(typ.clone()),
            length,
        }
    }
    fn to_string(&self) -> String {
        if let Length::Len(l) = self.length {
            format!("[{}]{}", self.typ.to_string(), l)
        } else {
            format!("[]{}", self.typ.to_string())
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct FuncSet {
    pub func_ident: String,
    pub func_param_typs: Vec<Typ>,
    pub func_ret_typs: Vec<Typ>,
    pub embedded: bool,
    pub embedded_typ: Option<Typ>,
}
impl FuncSet {
    pub fn new(
        func_ident: String,
        func_param_typs: Vec<Typ>,
        func_ret_typs: Vec<Typ>,
        embedded: bool,
        embedded_typ: Option<Typ>,
    ) -> Self {
        Self {
            func_ident,
            func_param_typs,
            func_ret_typs,
            embedded,
            embedded_typ,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct DefinedInterface {
    pub package: String,
    pub interface_ident: String,
    pub func_set: Vec<FuncSet>,
    pub raw_ir: String,
}
impl DefinedInterface {
    pub fn new(
        package: String,
        interface_ident: String,
        func_set: Vec<FuncSet>,
        raw_ir: String,
    ) -> Self {
        Self {
            package,
            interface_ident,
            func_set,
            raw_ir: remove_double_space(&raw_ir),
        }
    }
    fn to_string(&self) -> String {
        self.raw_ir.clone()
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Tag {
    pub name: String,
    pub value: String,
}
impl Tag {
    pub fn new(name: String, value: String) -> Self {
        Self { name, value }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Field {
    pub package: String,
    pub ident: String,
    pub typ: Typ,
    pub tag: Option<Tag>,
    pub embedded: bool,
}
impl Field {
    pub fn new(package: String, ident: String, typ: Typ, tag: Option<Tag>, embedded: bool) -> Self {
        let ident = if embedded {
            format!("[Embedding field] {}", typ.to_string())
        } else {
            ident
        };
        Self {
            package,
            ident,
            typ,
            tag,
            embedded,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Struct {
    pub package: String,
    pub struct_ident: String,
    pub fields: Vec<Field>,
    pub raw_ir: String,
}
impl Struct {
    pub fn new(package: String, struct_ident: String, fields: Vec<Field>, raw_ir: String) -> Self {
        Self {
            package,
            struct_ident,
            fields,
            raw_ir: remove_double_space(&raw_ir),
        }
    }
    fn to_string(&self) -> String {
        self.raw_ir.clone()
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum ChanTyp {
    Send,
    Receive,
    Both,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Chan {
    pub chan_typ: ChanTyp,
    pub typ: Box<Typ>,
}
impl Chan {
    pub fn new(chan_typ: ChanTyp, typ: Typ) -> Self {
        Self {
            chan_typ: chan_typ,
            typ: Box::new(typ),
        }
    }
    fn to_string(&self) -> String {
        let chan_typ_str = match self.chan_typ {
            ChanTyp::Send => "chan<-",
            ChanTyp::Receive => "<-chan",
            ChanTyp::Both => "chan",
        };
        format!("{} {}", chan_typ_str, self.typ.to_string())
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct DefinedTyp {
    pub package: String,
    pub name: String,
}
impl DefinedTyp {
    pub fn new(typ_str: &str) -> Self {
        let (_, (package, name)) = parse::parse_package_with_ident(typ_str).unwrap();
        Self { package, name }
    }
    pub fn to_string(&self) -> String {
        format!("{}.{}", self.package, self.name)
    }
    pub fn get_typ(&self) -> Option<Typ> {
        collection::get_typ(&self.to_string())
    }
}

fn remove_double_space(raw_ir: &str) -> String {
    raw_ir
        .split(" ")
        .into_iter()
        .filter(|s| *s != "")
        .collect::<Vec<_>>()
        .join(" ")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ptr_typ() {
        let input = "***int";
        let typ = Typ::from_str(input);
        assert_eq!(
            Typ::Ptr(Box::new(Typ::Ptr(Box::new(Typ::Ptr(Box::new(Typ::Int)))))),
            typ
        );
    }

    #[test]
    fn test_ptr_to_string() {
        let ptr = Typ::Ptr(Box::new(Typ::Ptr(Box::new(Typ::Ptr(Box::new(Typ::Int))))));
        assert_eq!(ptr.to_string(), "***int");
    }

    #[test]
    fn test_array_typ() {
        let input = "**[5]int";
        let parsed = Typ::from_str(input);
        println!("{:?}", parsed);
        let expected = Typ::Ptr(Box::new(Typ::Ptr(Box::new(Typ::Array(Array {
            typ: Box::new(Typ::Int),
            length: Length::Len(5),
        })))));
        assert_eq!(parsed, expected);
    }

    #[test]
    fn test_slice_typ() {
        let input = "**[]int";
        let parsed = Typ::from_str(input);
        println!("{:?}", parsed);
        let expected = Typ::Ptr(Box::new(Typ::Ptr(Box::new(Typ::Slice(Slice {
            typ: Box::new(Typ::Int),
            length: Length::Unknown,
        })))));
        println!("{}", expected.to_string());
        assert_eq!(parsed, expected);
    }

    #[test]
    fn test_array_to_string() {
        let array = Typ::Ptr(Box::new(Typ::Ptr(Box::new(Typ::Array(Array {
            typ: Box::new(Typ::Int),
            length: Length::Len(5),
        })))));
        println!("{:?}", array.to_string());
    }

    #[test]
    fn test_slice_to_string() {
        let slice = Typ::Ptr(Box::new(Typ::Ptr(Box::new(Typ::Slice(Slice {
            typ: Box::new(Typ::Int),
            length: Length::Unknown,
        })))));
        println!("{:?}", slice.to_string());
    }

    #[test]
    fn test_get_inner_typ_of_ptr() {
        let slice = Typ::Ptr(Box::new(Typ::Ptr(Box::new(Typ::Slice(Slice {
            typ: Box::new(Typ::Int),
            length: Length::Unknown,
        })))));
        println!("{:?}", slice.get_inner_typ_of_ptr());
    }

    #[test]
    fn test_map() {
        let mut input = "[5][2][1]int";
        let mut parsed = Typ::from_str(input);
        println!("{:?}", parsed);

        input = "map[string]string";
        parsed = Typ::from_str(input);
        println!("{:?}", parsed);

        input = "*map[string]string";
        parsed = Typ::from_str(input);
        println!("{:?}", parsed);

        input = "*map[string]map[string]string";
        parsed = Typ::from_str(input);
        println!("{:?}", parsed);

        input = "map[string]map[string]map[string]string";
        parsed = Typ::from_str(input);
        println!("{:?}", parsed);

        input = "map[string][]string";
        parsed = Typ::from_str(input);
        println!("{:?}", parsed);
    }
}
