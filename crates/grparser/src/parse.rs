extern crate nom;
use crate::instr::{
    AllocInstr, AllocSite, AssignmentInstr, BinaryExprInstr, BinaryExprSym, BlockEntryInstr,
    ChangeInterfaceInstr, ChangetypeInstr, ClosureInstr, ConvertInstr, DerefInstr, ExtractInstr,
    FieldAccessInstr, FuncCallInstr, FuncSigInstr, GetValueChanInstr, GoRoutineInstr, IdentInstr,
    IfInstr, IndexInstr, Instruction, InvokeInstr, JumpInstr, LiteralValueInstr, MakeAssignInstr,
    MakeCallInstr, MapAssignmentInstr, MembershipTestInstr, NextInstr, PanicInstr, PhiInstr, Range,
    RangeInstr, RangeValue, ReturnInstr, SelectInstr, SendInstr, SendValueChanInstr, SliceInstr,
    SliceToArrayPtrInstr, TypeAssertInstr, UnaryExprInstr, UnaryExprSym, UnsupportedInstr,
};
use crate::mapping;
use crate::metadata::{self, CodeLine, FileLoc, FreeVar, FuncSig, Metadata, MetadataAddressTyp};
use crate::scope;
use crate::typ::{ChanTyp, DefinedInterface, Field, FuncSet, Length, Struct, Tag, Typ};
use crate::value::{LiteralValue, Value};
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while},
    character::complete::{alpha1, alphanumeric1, char, digit1},
    character::{is_alphabetic, is_alphanumeric},
    combinator::map,
    multi::{many0, many1},
    number::complete::{double, float},
    sequence::{delimited, preceded, tuple},
    IResult,
};

fn is_func_entry(input: &str) -> bool {
    if input.starts_with("func") && input.ends_with(":") {
        true
    } else {
        false
    }
}

fn is_func_desc(input: &str) -> bool {
    if is_func_entry(input) || input.chars().nth(0) == Some('#') {
        true
    } else {
        false
    }
}

pub fn typs_to_string(typs: &Vec<Typ>) -> String {
    let mut typ_str = "".to_string();
    if typs.len() > 0 {
        typ_str = typs[0].to_string();
        for typ in typs.iter().skip(1) {
            typ_str = format!("{}, {}", typ_str, typ.to_string());
        }
    }
    typ_str
}

fn skip_spaces(input: &str) -> IResult<&str, &str> {
    let chars = " \t\r\n";
    take_while(move |ch| chars.contains(ch))(input)
}

fn parse_codeline(input: &str) -> IResult<&str, CodeLine> {
    let (input, (row, _, col)) = tuple((digit1, char(':'), digit1))(input)?;
    Ok((
        input,
        CodeLine::new(row.parse::<u64>().unwrap(), col.parse::<u64>().unwrap()),
    ))
}

fn parse_file_loc(input: &str) -> IResult<&str, FileLoc> {
    if input.len() == 1 && input.chars().next().unwrap() == '-' {
        return Ok(("", FileLoc::new("".to_string(), CodeLine::new(0, 0))));
    }
    let err = nom::error::Error::new(input, nom::error::ErrorKind::Fail);
    let parsed_go_filename: IResult<&str, &str> = take_while(move |ch| ch != ':')(input);
    if let Ok((residual, go_filename)) = parsed_go_filename {
        if let Ok((residual, codeline)) = parse_codeline(&residual[1..]) {
            return Ok((residual, FileLoc::new(go_filename.to_string(), codeline)));
        } else {
            return Err(nom::Err::Error(err));
        }
    }
    Err(nom::Err::Error(err))
}

fn parse_unsupported_instr(instr: &str) -> IResult<&str, Instruction> {
    Ok(("", Instruction::Unsupported(UnsupportedInstr::new(instr))))
}

fn parse_func_sig(instr: &str) -> IResult<&str, FuncSig> {
    let (instr_, func_sig) = alt((
        parse_func_no_return,
        parse_func_sig_without_receiver,
        parse_func_sig_with_receiver,
    ))(instr)?;
    scope::set_cur_func_sig(&func_sig);
    Ok((instr_, func_sig))
}

fn parse_func_no_return(instr: &str) -> IResult<&str, FuncSig> {
    let (instr_, (func_ident, param_set, _)) = tuple((
        preceded(tag("func"), preceded(skip_spaces, parse_ident)),
        parse_func_param_idents_typs,
        char(':'),
    ))(instr)?;
    scope::set_cur_blk_num(0);

    Ok((
        instr_,
        FuncSig::new(None, None, func_ident, param_set, vec![]),
    ))
}

fn parse_func_sig_without_receiver(instr: &str) -> IResult<&str, FuncSig> {
    let (instr_, (func_ident, param_set, ret_set, _)) = tuple((
        preceded(tag("func"), preceded(skip_spaces, parse_ident)),
        parse_func_param_idents_typs,
        alt((
            preceded(skip_spaces, parse_func_param_idents_typs),
            map(tag(""), |_| vec![]), // no return
        )),
        char(':'),
    ))(instr)?;
    scope::set_cur_blk_num(0);

    Ok((
        instr_,
        FuncSig::new(None, None, func_ident, param_set, ret_set),
    ))
}

fn parse_func_sig_with_receiver(instr: &str) -> IResult<&str, FuncSig> {
    let (instr_, (_, _, (receiver_ident, receiver_typ), func_ident, param_set, ret_set, _)) =
        tuple((
            tag("func"),
            skip_spaces,
            delimited(
                char('('),
                tuple((parse_ident, preceded(skip_spaces, parse_typ))),
                char(')'),
            ),
            preceded(skip_spaces, parse_ident),
            parse_func_param_idents_typs,
            alt((
                preceded(skip_spaces, parse_func_param_idents_typs),
                map(tag(""), |_| vec![]), // no return
            )),
            char(':'),
        ))(instr)?;
    scope::set_cur_blk_num(0);

    Ok((
        instr_,
        FuncSig::new(
            Some(receiver_ident),
            Some(receiver_typ),
            func_ident,
            param_set,
            ret_set,
        ),
    ))
}

fn parse_func_desc(instr: &str) -> IResult<&str, Metadata> {
    if is_func_entry(instr) {
        // if function signature
        let (instr_, func_sig) = parse_func_sig(instr)?;
        Ok((
            instr_,
            Metadata::new_func_desc("", "", Some(func_sig), instr),
        ))
    } else {
        let (desc, (_, _, prop, _, _)) = tuple((
            char('#'),
            skip_spaces,
            alt((tag("Free variables"), alpha1)),
            char(':'),
            skip_spaces,
        ))(instr)?;
        Ok((desc, Metadata::new_func_desc(prop, desc, None, instr)))
    }
}

pub fn parse_free_vars(instr: &str) -> IResult<&str, Vec<(&str, Value, Typ)>> {
    let (residual, free_vars) = delimited(
        char('['),
        many0(map(
            tuple((
                digit1,
                preceded(char(':'), parse_ident),
                preceded(skip_spaces, parse_typ),
                many0(tag(", ")),
            )),
            |(var_idx, ident, typ, _)| (var_idx, ident, typ),
        )),
        char(']'),
    )(instr)?;
    Ok((residual, free_vars))
}

fn parse_func_entry(instr: &str) -> IResult<&str, Instruction> {
    let (instr_, func_sig) = parse_func_sig(instr)?;
    Ok((
        instr_,
        Instruction::FuncSig(FuncSigInstr::new(func_sig, instr)),
    ))
}

fn parse_block_entry(instr: &str) -> IResult<&str, Instruction> {
    let (metadata, (block_num, _, _)) = tuple((digit1, char(':'), skip_spaces))(instr)?;
    let block_num = block_num.parse::<u64>().unwrap();
    scope::set_cur_blk_num(block_num);
    Ok((
        metadata,
        Instruction::BlockEntry(BlockEntryInstr::new(block_num, metadata, instr)),
    ))
}

pub fn parse_typ_def_ident(input: &str) -> IResult<&str, String> {
    map(preceded(tag("type  "), parse_ident), |ident| {
        ident.to_string()
    })(input)
}

pub fn parse_unsafe_ptr_def(typ_stmt: &str) -> IResult<&str, (String, Typ)> {
    let (typ_stmt_, (ident, _)) = tuple((
        preceded(tag("type  "), parse_ident),
        preceded(skip_spaces, tag("Pointer")),
    ))(typ_stmt)?;
    Ok((typ_stmt_, (ident.to_string(), Typ::UnsafePointer)))
}

pub fn parse_primitive_typ_def(typ_stmt: &str) -> IResult<&str, (String, Typ)> {
    let (typ_stmt_, (ident, typ)) = tuple((
        preceded(tag("type  "), parse_ident),
        preceded(skip_spaces, parse_typ),
    ))(typ_stmt)?;
    Ok((typ_stmt_, (ident.to_string(), typ)))
}

pub fn parse_struct_field_loop(typ_stmt: &str) -> IResult<&str, Vec<Field>> {
    delimited(
        tag("struct{"),
        many0(map(
            tuple((
                alt((
                    parse_struct_field_with_tag,
                    parse_struct_field,
                    parse_struct_embidding_field,
                )),
                many0(tag("; ")),
            )),
            |((ident, typ, tag, embedded), _)| {
                let (_, (package, _)) = parse_package_with_ident(&typ.to_string()).unwrap();
                let package = if package == "" {
                    scope::get_cur_package()
                } else {
                    package
                };
                Field::new(package, ident, typ, tag, embedded)
            },
        )),
        char('}'),
    )(typ_stmt)
}

pub fn parse_struct_typ(typ_stmt: &str) -> IResult<&str, (String, Typ)> {
    let (typ_stmt_, (struct_ident, fields)) = tuple((
        preceded(tag("type  "), parse_ident),
        preceded(skip_spaces, parse_struct_field_loop),
    ))(typ_stmt)?;
    Ok((
        typ_stmt_,
        (
            struct_ident.to_string(),
            Typ::Struct(Struct::new(
                scope::get_cur_package(),
                struct_ident.to_string(),
                fields,
                typ_stmt.to_string(),
            )),
        ),
    ))
}

fn parse_struct_field(field_str: &str) -> IResult<&str, (String, Typ, Option<Tag>, bool)> {
    let (field_str_, (field_ident, typ)) =
        tuple((parse_ident, preceded(skip_spaces, parse_typ)))(field_str)?;
    Ok((field_str_, (field_ident.to_string(), typ, None, false)))
}

fn parse_struct_field_with_tag(field_str: &str) -> IResult<&str, (String, Typ, Option<Tag>, bool)> {
    let (field_str_, (field_ident, typ, (tag_name, tag_value), _)) = tuple((
        parse_ident,
        preceded(skip_spaces, parse_typ),
        preceded(
            skip_spaces,
            preceded(
                char('"'),
                tuple((
                    alphanumeric1,
                    delimited(
                        tag(":\\\""),
                        map(take_while(move |ch| ch != '\\'), |s: &str| s.to_string()),
                        tag("\\\""),
                    ),
                )),
            ),
        ),
        char('"'),
    ))(field_str)?;
    Ok((
        field_str_,
        (
            field_ident.to_string(),
            typ,
            Some(Tag::new(tag_name.to_string(), tag_value)),
            false,
        ),
    ))
}

fn parse_struct_embidding_field(
    field_str: &str,
) -> IResult<&str, (String, Typ, Option<Tag>, bool)> {
    let (field_str_, typ) = parse_typ(field_str)?;
    Ok((field_str_, ("".to_string(), typ, None, true)))
}

pub fn parse_struct_literal(typ_stmt: &str) -> IResult<&str, Typ> {
    let (typ_stmt_, fields) = parse_struct_field_loop(typ_stmt)?;
    Ok((
        typ_stmt_,
        Typ::Struct(Struct::new(
            "".to_string(),
            "[Literal struct]".to_string(),
            fields,
            typ_stmt.to_string(),
        )),
    ))
}

pub fn parse_struct_literal_str(typ_stmt: &str) -> IResult<&str, String> {
    if typ_stmt.starts_with("struct{") && (&typ_stmt["struct{".len()..]).contains("}") {
        if let Some(end_bracket_idx) = typ_stmt.chars().position(|c| c == '}') {
            let struct_str = &typ_stmt[..end_bracket_idx + 1];
            let residual = &typ_stmt[end_bracket_idx + 1..];
            return Ok((residual, struct_str.to_string()));
        }
    }
    let err = nom::error::Error::new(typ_stmt, nom::error::ErrorKind::NonEmpty);
    Err(nom::Err::Error(err))
}

pub fn parse_interface_field(
    typ_stmt: &str,
) -> IResult<&str, (Value, Vec<Typ>, Vec<Vec<Typ>>, bool)> {
    let (typ_stmt_, (ident, param_typs, ret_typs, _)) = tuple((
        parse_ident,
        parse_func_param_typs,
        many0(preceded(skip_spaces, parse_func_param_typs)),
        many0(tag("; ")),
    ))(typ_stmt)?;
    Ok((typ_stmt_, (ident, param_typs, ret_typs, false)))
}

fn parse_interface_embedding_field(
    typ_stmt: &str,
) -> IResult<&str, (Value, Vec<Typ>, Vec<Vec<Typ>>, bool)> {
    let (typ_stmt_, (package, ident)) = parse_package_with_ident(typ_stmt)?;
    let package_with_ident = if package == "" {
        format!("{}.{}", scope::get_cur_package(), ident)
    } else {
        format!("{}.{}", package, ident)
    };
    match typ_stmt_.chars().nth(0).unwrap() {
        ';' | '}' => {
            let typ = Typ::from_str(&package_with_ident);
            Ok((
                typ_stmt_,
                (
                    Value::new_ident(&format!("[Embedding field] {}", typ.to_string())),
                    vec![typ],
                    vec![],
                    true,
                ),
            ))
        }
        _ => {
            let err = nom::error::Error::new(typ_stmt_, nom::error::ErrorKind::NonEmpty);
            Err(nom::Err::Error(err))
        }
    }
}

pub fn parse_interface_typ(typ_stmt: &str) -> IResult<&str, (String, Typ)> {
    let (typ_stmt_, (interface_ident, func_set)) = tuple((
        preceded(tag("type  "), parse_ident),
        delimited(
            preceded(skip_spaces, tag("interface{")),
            many0(map(
                alt((parse_interface_embedding_field, parse_interface_field)),
                |(func_ident, mut func_param_typs, mut func_ret_typs, embedded)| {
                    if !embedded {
                        let ret_typs = if let Some(ret_typs) = func_ret_typs.pop() {
                            ret_typs
                        } else {
                            vec![]
                        };
                        FuncSet::new(
                            func_ident.to_string(),
                            func_param_typs,
                            ret_typs,
                            embedded,
                            None,
                        )
                    } else {
                        let embedded_field_typ = func_param_typs.pop().unwrap();
                        FuncSet::new(
                            func_ident.to_string(),
                            vec![],
                            vec![],
                            embedded,
                            Some(embedded_field_typ),
                        )
                    }
                },
            )),
            char('}'),
        ),
    ))(typ_stmt)?;
    Ok((
        typ_stmt_,
        (
            interface_ident.to_string(),
            Typ::DefinedInterface(DefinedInterface::new(
                scope::get_cur_package(),
                interface_ident.to_string(),
                func_set,
                typ_stmt.to_string(),
            )),
        ),
    ))
}

// Called from `collection.rs`
pub fn parse_typ_def(const_stmt: &str) -> IResult<&str, (String, Typ)> {
    let (residual, (ident, typ)) = alt((
        parse_unsafe_ptr_def,
        parse_interface_typ,
        parse_struct_typ,
        parse_primitive_typ_def,
    ))(const_stmt)?;
    Ok((residual, (ident, typ)))
}

// Used only for post process of cross defined type
pub fn parse_ptr_idents(typ_stmt: &str) -> IResult<&str, Vec<(String, String, usize)>> {
    let (typ_stmt_, (_, ident_vec)) = tuple((
        preceded(tag("type "), parse_ident),
        preceded(
            skip_spaces,
            delimited(
                tag("struct{"),
                many1(map(
                    // non-function field
                    tuple((
                        parse_ident,
                        preceded(skip_spaces, tuple((many0(char('*')), parse_ident))),
                        many0(tag("; ")),
                    )),
                    |(ident, (n_ptr, typ_ident), _)| {
                        (ident.to_string(), typ_ident.to_string(), n_ptr.len())
                    },
                )),
                char('}'),
            ),
        ),
    ))(typ_stmt)?;
    Ok((typ_stmt_, ident_vec))
}

pub fn parse_const_var(const_stmt: &str) -> IResult<&str, (String, LiteralValue, Typ)> {
    let (residual, (ident, _, literal_value, typ)) = tuple((
        preceded(tag("const "), parse_ident),
        preceded(skip_spaces, parse_ident),
        preceded(tag(" = "), parse_literal_value),
        preceded(skip_spaces, parse_typ),
    ))(const_stmt)?;
    Ok((
        residual,
        (
            ident.to_string(),
            literal_value.get_literal_value().unwrap().clone(),
            typ,
        ),
    ))
}

fn consume_non_alpha(input: &str) -> &str {
    for (idx, c) in input.chars().enumerate() {
        if is_alphabetic(c as u8) {
            return &input[idx..];
        }
    }
    input
}

pub fn parse_package_with_ident_wihtout_package(input: &str) -> IResult<&str, (String, String)> {
    let (residual, ident) = parse_ident(input)?;
    Ok((residual, ("".to_string(), ident.to_string())))
}

pub fn parse_package_with_ident_with_package(input: &str) -> IResult<&str, (String, String)> {
    let (residual, package) = parse_package(input)?;
    let (residual, ident) = preceded(many0(char('.')), parse_ident)(residual)?;
    Ok((residual, (package.to_string(), ident.to_string())))
}

pub fn parse_package_with_ident(input: &str) -> IResult<&str, (String, String)> {
    let input = consume_non_alpha(input);
    alt((
        parse_package_with_ident_with_package,
        parse_package_with_ident_wihtout_package,
    ))(input)
}

pub fn consume_package_with_ident(input: &str) -> IResult<&str, String> {
    let input = consume_non_alpha(input);
    let (input_, (package, ident)) = alt((
        parse_package_with_ident_with_package,
        parse_package_with_ident_wihtout_package,
    ))(input)?;
    Ok((input_, format!("{}.{}", package, ident)))
}

pub fn parse_package(instr: &str) -> IResult<&str, Value> {
    let (instr_, package) = many1(alt((tag("gopkg."), alphanumeric1, tag("-"), tag("/"))))(instr)?;
    let package = package.join("");
    let check_version: IResult<_, _> = tuple((tag(".v"), digit1))(instr_);
    if let Ok((instr_, (_, version))) = check_version {
        Ok((
            instr_,
            Value::new_ident(&format!("{}.v{}", package, version)),
        ))
    } else {
        Ok((instr_, Value::new_ident(&package)))
    }
}

fn parse_ident_str(input: &str) -> IResult<&str, String> {
    let (instr, ident) = many1(alt((alphanumeric1, tag("_"), tag("$"))))(input)?;
    Ok((instr, ident.join("")))
}

fn parse_ident(input: &str) -> IResult<&str, Value> {
    let (instr, ident) = parse_ident_str(input)?;
    Ok((instr, Value::new_ident(&ident)))
}

fn parse_variadic(instr: &str) -> IResult<&str, Value> {
    let (instr, (ident, _)) = tuple((parse_ident, tag("...")))(instr)?;
    Ok((
        instr,
        Value::new_variadic_ident(ident.get_ident_str().unwrap()),
    ))
}

fn parse_value(instr: &str) -> IResult<&str, Value> {
    alt((parse_literal_value, parse_variadic, parse_ident))(instr)
}

fn parse_array_with_single_idx_str(instr: &str) -> IResult<&str, String> {
    let (instr, (n_ptr, left_bracket, mut idx, right_bracket, typ)) = tuple((
        many0(char('*')),
        char('['),
        many0(digit1),
        char(']'),
        parse_typ_str,
    ))(instr)?;
    let idx = if let Some(idx) = idx.pop() { idx } else { "" };
    Ok((
        instr,
        format!(
            "{}{}{}{}{}",
            n_ptr.iter().collect::<String>(),
            left_bracket,
            idx,
            right_bracket,
            typ
        ),
    ))
}

fn parse_array_with_multidim_idx_str(instr: &str) -> IResult<&str, String> {
    let (instr, (n_ptr, indicies, typ)) = tuple((
        many0(char('*')),
        many0(tuple((char('['), many0(digit1), char(']')))),
        parse_typ_str,
    ))(instr)?;

    let mut idx_str = "".to_string();
    for (left_bracket, mut idx, right_bracket) in indicies {
        let idx = if let Some(idx) = idx.pop() { idx } else { "" };
        idx_str = format!("{}{}{}{}", idx_str, left_bracket, idx, right_bracket);
    }
    idx_str = format!("{}{}{}", n_ptr.iter().collect::<String>(), idx_str, typ);
    Ok((instr, idx_str))
}

fn parse_array_with_range(instr: &str) -> IResult<&str, RangeValue> {
    let (instr, (mut first, mut end, _)) = tuple((
        preceded(char('['), many0(parse_value)),
        preceded(char(':'), many0(parse_value)),
        char(']'),
    ))(instr)?;
    let range_value = match (first.pop(), end.pop()) {
        (Some(first), Some(end)) => (Range::Idx(first), Range::Idx(end)),
        (Some(first), _) => (Range::Idx(first), Range::End),
        (_, Some(end)) => (Range::First, Range::Idx(end)),
        _ => (Range::First, Range::End),
    };
    Ok((instr, RangeValue::new(range_value.0, range_value.1)))
}

fn parse_binary_expr_sym(instr: &str) -> IResult<&str, &str> {
    alt((
        tag("+"),
        tag("-"),
        tag("/"),
        tag("*"),
        tag("=="),
        tag("!="),
        tag(">="),
        tag("<="),
        tag("&&"),
        tag("||"),
        tag("&"),
        tag("|"),
        tag("^"),
        tag("<<"),
        tag(">>"),
        tag("<"),
        tag(">"),
    ))(instr)
}

fn alt_int_typ(instr: &str) -> IResult<&str, &str> {
    alt((
        tag("int8"),
        tag("int16"),
        tag("int32"),
        tag("int64"),
        tag("int"),
        tag("uint8"),
        tag("uint16"),
        tag("uint32"),
        tag("uint64"),
        tag("uintptr"),
        tag("uint"),
    ))(instr)
}

fn alt_primitive_typs(instr: &str) -> IResult<&str, &str> {
    alt((
        tag("byte"),
        tag("rune"),
        tag("complex64"),
        tag("complex128"),
        tag("float32"),
        tag("float64"),
        tag("string"),
        tag("bool"),
        tag("error"),
        tag("any"),
        tag("unsafe.Pointer"),
        tag("struct{}"),
        tag("()"),
    ))(instr)
}

fn alt_untyped_int_typ(instr: &str) -> IResult<&str, &str> {
    preceded(tag("untyped "), alt_int_typ)(instr)
}

fn alt_untyped_primitive_typs(instr: &str) -> IResult<&str, &str> {
    preceded(tag("untyped "), alt_primitive_typs)(instr)
}

fn parse_typ_primitive(instr: &str) -> IResult<&str, String> {
    let (instr, (n_ptr, typ_str)) = tuple((
        many0(char('*')),
        map(
            alt((
                tag("interface{}"),
                alt_int_typ,
                alt_primitive_typs,
                alt_untyped_int_typ,
                alt_untyped_primitive_typs,
            )),
            |s: &str| s.to_string(),
        ),
    ))(instr)?;
    match instr.chars().nth(0) {
        Some(c) if is_alphanumeric(c as u8) => {
            let err = nom::error::Error::new(instr, nom::error::ErrorKind::NonEmpty);
            Err(nom::Err::Error(err))
        }
        _ => Ok((
            instr,
            format!("{}{}", n_ptr.iter().collect::<String>(), typ_str),
        )),
    }
}

fn parse_typ_func_ptr(instr: &str) -> IResult<&str, (Vec<char>, Vec<Typ>, Vec<Typ>)> {
    let (instr_, (n_ptr, param_typs, ret_typs)) = tuple((
        many0(char('*')),
        preceded(tag("func"), parse_func_param_typs),
        alt((
            preceded(skip_spaces, parse_func_param_typs),
            map(tag(""), |_| vec![]), // no return
        )),
    ))(instr)?;
    Ok((instr_, (n_ptr, param_typs, ret_typs)))
}

fn parse_typ_func_ptr_str(instr: &str) -> IResult<&str, String> {
    let (instr_, (n_ptr, param_typs, ret_typs)) = parse_typ_func_ptr(instr)?;
    Ok((
        instr_,
        format!(
            "{}func({}) ({})",
            n_ptr.iter().collect::<String>(),
            typs_to_string(&param_typs),
            typs_to_string(&ret_typs)
        ),
    ))
}

fn parse_typ_map(instr: &str) -> IResult<&str, (Vec<char>, Typ, Typ)> {
    // (Pointer, key type, value type)
    let (instr_, (n_ptr, key_typ, value_typ)) = tuple((
        many0(char('*')),
        delimited(tag("map["), parse_typ, tag("]")),
        parse_typ,
    ))(instr)?;
    Ok((instr_, (n_ptr, key_typ, value_typ)))
}

fn parse_typ_map_str(instr: &str) -> IResult<&str, String> {
    let (instr, (n_ptr, key_typ, value_typ)) = parse_typ_map(instr)?;
    Ok((
        instr,
        format!(
            "{}map[{}]{}",
            n_ptr.iter().collect::<String>(),
            key_typ.to_string(),
            value_typ.to_string()
        ),
    ))
}

fn parse_typ_str_typ(instr: &str) -> IResult<&str, String> {
    alt((
        parse_typ_map_str,
        parse_typ_func_ptr_str,
        parse_typ_primitive,
        map(parse_ident, |s| s.to_string()), // User-defined struct type
    ))(instr)
}

fn parse_typ_str_with_package(instr: &str) -> IResult<&str, String> {
    let (instr_, (n_ptr, package_str, typ_str)) = tuple((
        alt((many0(char('*')), many0(char('&')))),
        parse_package,
        preceded(char('.'), parse_typ_str_typ),
    ))(instr)?;
    Ok((
        instr_,
        format!(
            "{}{}.{}",
            n_ptr.iter().collect::<String>(),
            package_str.get_ident_str().unwrap(),
            typ_str.to_string()
        ),
    ))
}

fn parse_typ_str_without_package(instr: &str) -> IResult<&str, String> {
    let (instr, (n_ptr, typ_str)) =
        tuple((alt((many0(char('*')), many0(char('&')))), parse_typ_str_typ))(instr)?;
    Ok((
        instr,
        format!(
            "{}{}.{}",
            n_ptr.iter().collect::<String>(),
            scope::get_cur_package(),
            typ_str.to_string()
        ),
    ))
}

fn parse_typ_str_without_chan_variadic(instr: &str) -> IResult<&str, String> {
    alt((
        parse_typ_map_str,
        parse_typ_func_ptr_str,
        parse_typ_primitive,
        parse_typ_str_with_package,
        parse_typ_str_without_package,
    ))(instr)
}

fn parse_typ_str_with_chan(instr: &str) -> IResult<&str, String> {
    let (instr, (n_ptr, chan_typ_str, typ)) = tuple((
        many0(char('*')),
        alt((tag("chan "), tag("<-chan "), tag("chan<- "))),
        parse_typ_str_without_chan_variadic,
    ))(instr)?;
    Ok((
        instr,
        format!(
            "{}{} {}",
            n_ptr.iter().collect::<String>(),
            chan_typ_str,
            typ
        ),
    ))
}

fn parse_typ_str_with_variadic(instr: &str) -> IResult<&str, String> {
    let (instr, (n_ptr, typ, _)) = tuple((
        many0(char('*')),
        parse_typ_str_without_chan_variadic,
        tag("..."),
    ))(instr)?;
    Ok((
        instr,
        format!("{}{}...", n_ptr.iter().collect::<String>(), typ),
    ))
}

fn parse_typ_str(instr: &str) -> IResult<&str, String> {
    alt((
        parse_typ_str_with_chan,
        parse_typ_str_with_variadic,
        parse_typ_str_without_chan_variadic,
    ))(instr)
}

pub fn parse_typ(instr: &str) -> IResult<&str, Typ> {
    let (instr, typ) = alt((
        parse_struct_literal_str,
        parse_typ_str,
        parse_array_with_single_idx_str,
        parse_array_with_multidim_idx_str,
    ))(instr)?;
    Ok((instr, Typ::from_str(&typ)))
}

pub fn decomposite_slice_and_array_typ(input: &str) -> (Typ, Length) {
    // (e.g.,) [5]int (fixed array), []int (slice)
    let (_, (mut length, typ)) =
        tuple((delimited(char('['), many0(digit1), char(']')), parse_typ))(input).unwrap();
    let length = if let Some(length) = length.pop() {
        Length::Len(length.parse::<usize>().unwrap())
    } else {
        Length::Unknown
    };
    (typ, length)
}

pub fn decomposite_map_typ(input: &str) -> (Vec<char>, Typ, Typ) {
    let (_, (n_ptr, key_typ, value_typ)) = parse_typ_map(input).unwrap();
    (n_ptr, key_typ, value_typ)
}

pub fn decomposite_complex64(input: &str) -> (BinaryExprSym, f32, f32) {
    let (_, (sym, real, img)) = parse_complex64_primitive(input).unwrap();
    (sym, real, img)
}

pub fn decomposite_complex128(input: &str) -> (BinaryExprSym, f64, f64) {
    let (_, (sym, real, img)) = parse_complex128_primitive(input).unwrap();
    (sym, real, img)
}

pub fn decomposite_func_ptr(input: &str) -> (Vec<char>, Vec<Typ>, Vec<Typ>) {
    let (_, (n_ptr, param_typs, ret_typs)) = parse_typ_func_ptr(input).unwrap();
    (n_ptr, param_typs, ret_typs)
}

pub fn decomposite_chan(input: &str) -> (ChanTyp, Typ) {
    let (_, (chan_typ_str, typ)) = tuple((
        alt((tag("chan "), tag("<-chan "), tag("chan<- "))),
        preceded(skip_spaces, parse_typ),
    ))(input)
    .unwrap();
    let chan_typ = match chan_typ_str {
        "chan " => ChanTyp::Both,
        "<-chan " => ChanTyp::Receive,
        "chan<- " => ChanTyp::Send,
        _ => unreachable!("{}", chan_typ_str),
    };
    (chan_typ, typ)
}

fn parse_only_typ_of_param(instr: &str) -> IResult<&str, Typ> {
    let (instr_, typ) =
        preceded(consume_package_with_ident, preceded(skip_spaces, parse_typ))(instr)?;
    Ok((instr_, typ))
}

fn empty(instr: &str) -> IResult<&str, Vec<Typ>> {
    Ok((instr, vec![]))
}

fn parse_only_typ_of_param_no_ident(instr: &str) -> IResult<&str, Typ> {
    let (instr_, typ) = parse_typ(instr)?;
    match instr_.chars().nth(0).unwrap() {
        ',' | ')' => Ok((instr_, typ)),
        _ => {
            let err = nom::error::Error::new(instr_, nom::error::ErrorKind::NonEmpty);
            Err(nom::Err::Error(err))
        }
    }
}

fn parse_param_set(instr: &str) -> IResult<&str, (Option<Value>, Typ)> {
    alt((
        map(
            tuple((parse_ident, preceded(skip_spaces, parse_typ))),
            |(ident, typ)| (Some(ident), typ),
        ),
        map(parse_typ, |typ| (None, typ)),
    ))(instr)
}

fn parse_func_param_idents_typs(instr: &str) -> IResult<&str, Vec<(Option<Value>, Typ)>> {
    let (instr_, (first_param_set, mut param_set)) = alt((
        map(
            tuple((map(tag("()"), |_| Typ::Empty), empty)),
            |(typ, _)| ((None, typ), vec![]),
        ),
        delimited(
            char('('),
            tuple((parse_param_set, many0(preceded(tag(", "), parse_param_set)))),
            char(')'),
        ),
        map(tuple((parse_typ, many0(parse_typ))), |(typ, _)| {
            ((None, typ), vec![])
        }), // single typ
    ))(instr)?;
    if param_set.len() == 0 {
        param_set = if first_param_set.1 == Typ::Empty {
            vec![]
        } else {
            vec![first_param_set.clone()]
        }
    } else {
        param_set.insert(0, first_param_set.clone());
    }
    Ok((instr_, param_set))
}

fn parse_func_param_typs(instr: &str) -> IResult<&str, Vec<Typ>> {
    let (instr_, (first_param_typ, mut param_typs)) = alt((
        tuple((map(tag("()"), |_| Typ::Empty), empty)),
        delimited(
            char('('),
            tuple((
                alt((parse_only_typ_of_param_no_ident, parse_only_typ_of_param)),
                many0(preceded(
                    tag(", "),
                    alt((parse_only_typ_of_param_no_ident, parse_only_typ_of_param)),
                )),
            )),
            char(')'),
        ),
        tuple((parse_typ, many0(parse_typ))), // single typ
    ))(instr)?;
    if param_typs.len() == 0 {
        param_typs = if first_param_typ == Typ::Empty {
            vec![]
        } else {
            vec![first_param_typ.clone()]
        }
    } else {
        param_typs.insert(0, first_param_typ.clone());
    }
    Ok((instr_, param_typs))
}

fn parse_func_params(instr: &str) -> IResult<&str, Vec<Value>> {
    let (instr_, (mut first_param, mut params)) = delimited(
        char('('),
        tuple((
            many0(parse_value),
            many0(preceded(char(','), preceded(skip_spaces, parse_value))),
        )),
        char(')'),
    )(instr)?;
    if let Some(param) = first_param.pop() {
        if params.len() == 0 {
            params = vec![param];
        } else {
            params.insert(0, param);
        }
    } else {
        if params.len() == 0 {
            params = vec![];
        }
    }
    Ok((instr_, params))
}

fn parse_complex64_primitive(instr: &str) -> IResult<&str, (BinaryExprSym, f32, f32)> {
    let (instr_, (_, real, sym, img, _, _)) = tuple((
        char('('),
        float,
        preceded(skip_spaces, parse_binary_expr_sym),
        preceded(skip_spaces, float),
        tag("i"),
        char(')'),
    ))(instr)?;
    Ok((instr_, (BinaryExprSym::from_str(sym), real, img)))
}

fn parse_complex128_primitive(instr: &str) -> IResult<&str, (BinaryExprSym, f64, f64)> {
    let (instr_, (_, real, sym, img, _, _)) = tuple((
        char('('),
        double,
        preceded(skip_spaces, parse_binary_expr_sym),
        preceded(skip_spaces, double),
        tag("i"),
        char(')'),
    ))(instr)?;
    Ok((instr_, (BinaryExprSym::from_str(sym), real, img)))
}

fn parse_complex64_primitive_str(instr: &str) -> IResult<&str, String> {
    let (instr_, (sym, real, img)) = parse_complex64_primitive(instr)?;
    Ok((instr_, format!("({} {} {}i)", real, sym.to_string(), img)))
}

fn parse_complex128_primitive_str(instr: &str) -> IResult<&str, String> {
    let (instr_, (sym, real, img)) = parse_complex128_primitive(instr)?;
    Ok((instr_, format!("({} {} {}i)", real, sym.to_string(), img)))
}

fn parse_literal_value_primitive(instr: &str) -> IResult<&str, String> {
    alt((
        map(double, |v| v.to_string()),
        map(digit1, |v: &str| v.to_string()),
        map(tag("true"), |_| "true".to_string()),
        map(tag("false"), |_| "false".to_string()),
        map(tag("nil"), |_| "nil".to_string()),
        parse_complex64_primitive_str,
        parse_complex128_primitive_str,
        delimited(
            char('"'),
            map(take_while(move |ch| ch != '"'), |s: &str| s.to_string()), // string typ
            char('"'),
        ), // string typ
    ))(instr)
}

fn parse_literal_value(instr: &str) -> IResult<&str, Value> {
    let (instr_, (literal_value, mut typ)) = tuple((
        parse_literal_value_primitive,
        many0(preceded(char(':'), parse_typ)),
    ))(instr)?;
    let typ = if let Some(typ) = typ.pop() {
        typ
    } else {
        unreachable!()
    };
    Ok((instr_, Value::new_literal_value(&typ, &literal_value)))
}

fn parse_literal_instr(instr: &str) -> IResult<&str, Instruction> {
    let (instr_, (literal_value, _, typ)) =
        tuple((parse_literal_value_primitive, char(':'), parse_typ))(instr)?;
    let value = Value::new_literal_value(&typ, &literal_value);
    Ok((
        instr_,
        Instruction::LiteralValue(LiteralValueInstr::new(typ, value, instr)),
    ))
}

fn parse_unary_expr(instr: &str) -> IResult<&str, Instruction> {
    let (instr_, (unary_expr_sym, ident, result_typ)) = tuple((
        alt((char('-'), char('!'), char('^'))),
        parse_ident,
        preceded(skip_spaces, parse_typ),
    ))(instr)?;
    let unary_expr_sym = UnaryExprSym::from_str(&unary_expr_sym);
    Ok((
        instr_,
        Instruction::UnaryExpr(UnaryExprInstr::new(
            unary_expr_sym,
            ident,
            result_typ,
            instr,
        )),
    ))
}

fn parse_binary_expr(instr: &str) -> IResult<&str, Instruction> {
    let (instr_, (value1, binary_expr_sym, value2, result_typ)) = tuple((
        parse_value,
        preceded(skip_spaces, parse_binary_expr_sym),
        preceded(skip_spaces, parse_value),
        preceded(skip_spaces, parse_typ),
    ))(instr)?;
    let binary_expr_sym = BinaryExprSym::from_str(binary_expr_sym);
    Ok((
        instr_,
        Instruction::BinaryExpr(BinaryExprInstr::new(
            binary_expr_sym,
            value1,
            value2,
            result_typ,
            instr,
        )),
    ))
}

fn parse_ident_instr(instr: &str) -> IResult<&str, Instruction> {
    let (instr_, ident) = parse_ident(instr)?;
    if instr_.len() > 0 {
        let err = nom::error::Error::new(instr_, nom::error::ErrorKind::NonEmpty);
        Err(nom::Err::Error(err))
    } else {
        Ok((instr_, Instruction::Ident(IdentInstr::new(ident, instr))))
    }
}

fn parse_deref_instr(instr: &str) -> IResult<&str, Instruction> {
    let (instr_, (derefs, ident, typ)) = tuple((
        many0(char('*')),
        parse_ident,
        preceded(skip_spaces, parse_typ),
    ))(instr)?;
    if instr_.len() > 0 {
        let err = nom::error::Error::new(instr_, nom::error::ErrorKind::NonEmpty);
        Err(nom::Err::Error(err))
    } else {
        Ok((
            instr_,
            Instruction::Deref(DerefInstr::new(ident, derefs.len(), typ, instr)),
        ))
    }
}

fn parse_jump(instr: &str) -> IResult<&str, Instruction> {
    let (instr_, blk_num) = preceded(tag("jump"), preceded(skip_spaces, digit1))(instr)?;
    let blk_num = blk_num.parse::<u64>().unwrap();
    Ok((instr_, Instruction::Jump(JumpInstr::new(blk_num, instr))))
}

fn parse_none_return(instr: &str) -> IResult<&str, Instruction> {
    let (instr_, _) = tag("return")(instr)?;
    Ok((instr_, Instruction::Return(ReturnInstr::new(vec![], instr))))
}

fn parse_single_return(instr: &str) -> IResult<&str, Instruction> {
    let (instr_, (_, value)) = tuple((tag("return"), preceded(skip_spaces, parse_value)))(instr)?;
    Ok((
        instr_,
        Instruction::Return(ReturnInstr::new(vec![value], instr)),
    ))
}

fn parse_multiple_return(instr: &str) -> IResult<&str, Instruction> {
    let (instr_, (_, _, first_value, mut values)) = tuple((
        tag("return"),
        skip_spaces,
        parse_value,
        many0(preceded(char(','), preceded(skip_spaces, parse_value))),
    ))(instr)?;
    values.insert(0, first_value);
    Ok((instr_, Instruction::Return(ReturnInstr::new(values, instr))))
}

fn parse_return(instr: &str) -> IResult<&str, Instruction> {
    alt((
        parse_multiple_return,
        parse_single_return,
        parse_none_return,
    ))(instr)
}

fn is_defer(instr: &str) -> (&str, bool) {
    let defer: IResult<_, _> = tag("defer ")(instr);
    if let Ok((instr_, _)) = defer {
        (instr_, true)
    } else {
        (instr, false)
    }
}

fn unwrap_ret_typs(mut typs: Vec<Vec<Typ>>) -> Vec<Typ> {
    if typs.len() > 0 {
        typs.pop().unwrap()
    } else {
        vec![]
    }
}

fn parse_call(instr: &str) -> IResult<&str, Instruction> {
    alt((
        parse_call_with_receiver,
        parse_call_without_receiver,
        parse_call_local_func,
    ))(instr)
}

fn parse_call_with_receiver(instr: &str) -> IResult<&str, Instruction> {
    let (instr_, is_defer) = is_defer(instr);
    let (_, (_, package)) = preceded(char('('), tuple((many0(char('*')), parse_package)))(instr)?;
    let (instr_, (receiver_typ, func_ident, params, ret_typs)) = tuple((
        delimited(char('('), parse_typ, char(')')),
        preceded(char('.'), parse_ident),
        parse_func_params,
        many0(preceded(skip_spaces, parse_func_param_typs)),
    ))(instr_)?;
    let ret_typs = unwrap_ret_typs(ret_typs);
    let package = if !package.to_string().contains("/") {
        scope::get_cur_package()
    } else {
        package.to_string()
    };
    Ok((
        instr_,
        Instruction::FuncCall(FuncCallInstr::new(
            package,
            Some(receiver_typ),
            func_ident,
            params,
            ret_typs,
            is_defer,
            instr,
        )),
    ))
}

fn parse_call_without_receiver(instr: &str) -> IResult<&str, Instruction> {
    let (instr_, is_defer) = is_defer(instr);
    let (instr_, (package, func_ident, params, ret_typs)) = tuple((
        parse_package,
        preceded(char('.'), parse_ident),
        parse_func_params,
        many0(preceded(skip_spaces, parse_func_param_typs)),
    ))(instr_)?;
    let ret_typs = unwrap_ret_typs(ret_typs);
    Ok((
        instr_,
        Instruction::FuncCall(FuncCallInstr::new(
            package.to_string(),
            None,
            func_ident,
            params,
            ret_typs,
            is_defer,
            instr,
        )),
    ))
}

fn parse_call_local_func(instr: &str) -> IResult<&str, Instruction> {
    let (instr_, is_defer) = is_defer(instr);
    let (instr_, (func_ident, params, ret_typs)) = tuple((
        parse_ident,
        parse_func_params,
        many0(preceded(skip_spaces, parse_func_param_typs)),
    ))(instr_)?;
    let ret_typs = unwrap_ret_typs(ret_typs);
    Ok((
        instr_,
        Instruction::FuncCall(FuncCallInstr::new(
            scope::get_cur_package(),
            None,
            func_ident,
            params,
            ret_typs,
            is_defer,
            instr,
        )),
    ))
}

/// parse_info is actually used, but the value of return is not used
fn parse_info(instr: &str) -> IResult<&str, String> {
    let (instr, info) = delimited(char('('), parse_ident_str, char(')'))(instr)?;
    Ok((instr, info))
}

fn parse_alloc(instr: &str) -> IResult<&str, Instruction> {
    let (instr_, (alloc_site, typ1, /*alloc_info*/ _, typ2)) = tuple((
        alt((tag("new"), tag("local"))),
        preceded(skip_spaces, parse_typ),
        preceded(skip_spaces, parse_info),
        preceded(skip_spaces, parse_typ),
    ))(instr)?;
    let alloc_site = if alloc_site == "new" {
        AllocSite::Heap
    } else {
        AllocSite::Local
    };
    let typ = match typ1.get_inner_typ_of_ptr() {
        Typ::Array(_) => typ1,
        _ => typ2,
    };
    Ok((
        instr_,
        Instruction::Alloc(AllocInstr::new(typ, alloc_site, instr)),
    ))
}

fn parse_index_access(instr: &str) -> IResult<&str, Instruction> {
    let (instr_, (ref_or_deref, ident, idx, typ)) = tuple((
        alt((many1(char('*')), many1(char('&')), many0(char('0')))),
        parse_ident,
        delimited(char('['), parse_value, char(']')),
        preceded(skip_spaces, parse_typ),
    ))(instr)?;
    assert!(ref_or_deref.len() <= 1);
    let (is_ref, is_deref) = if ref_or_deref.len() > 0 {
        if ref_or_deref[0] == '&' {
            (true, false)
        } else {
            (false, true)
        }
    } else {
        (false, false)
    };
    Ok((
        instr_,
        Instruction::Index(IndexInstr::new(ident, is_ref, is_deref, idx, typ, instr)),
    ))
}

fn parse_make_assign(instr: &str) -> IResult<&str, Instruction> {
    let (instr_, (any_or_interface_typ, rhs_typ, rhs_value, _)) = tuple((
        preceded(tag("make"), preceded(skip_spaces, parse_typ)),
        preceded(
            skip_spaces,
            preceded(tag("<-"), preceded(skip_spaces, parse_typ)),
        ),
        preceded(skip_spaces, delimited(char('('), parse_value, char(')'))),
        preceded(skip_spaces, parse_typ),
    ))(instr)?;
    Ok((
        instr_,
        Instruction::AnyOrInterfaceAssign(MakeAssignInstr::new(
            any_or_interface_typ,
            rhs_typ,
            rhs_value,
            instr,
        )),
    ))
}

fn parse_change_interface(instr: &str) -> IResult<&str, Instruction> {
    let (instr_, (lhs_typ, rhs_typ, rhs_value)) = tuple((
        preceded(tag("change interface "), parse_typ),
        preceded(tag(" <- "), parse_typ),
        preceded(skip_spaces, delimited(char('('), parse_ident, char(')'))),
    ))(instr)?;
    Ok((
        instr_,
        Instruction::ChangeInterfaceAssign(ChangeInterfaceInstr::new(
            lhs_typ, rhs_typ, rhs_value, instr,
        )),
    ))
}

fn parse_make_call(instr: &str) -> IResult<&str, Instruction> {
    let (instr_, (typ, value)) = tuple((
        preceded(tag("make "), parse_typ),
        preceded(skip_spaces, parse_value),
    ))(instr)?;
    Ok((
        instr_,
        Instruction::MakeCall(MakeCallInstr::new(typ, value, instr)),
    ))
}

fn parse_extract(instr: &str) -> IResult<&str, Instruction> {
    let (instr_, (ident, ret_pos, typ)) = tuple((
        preceded(tag("extract "), parse_ident),
        preceded(tag(" #"), digit1),
        preceded(skip_spaces, parse_typ),
    ))(instr)?;
    Ok((
        instr_,
        Instruction::Extract(ExtractInstr::new(
            ident,
            typ,
            ret_pos.parse::<usize>().unwrap(),
            instr,
        )),
    ))
}

fn parse_slice(instr: &str) -> IResult<&str, Instruction> {
    let (instr_, (arr_val, range, typ)) = tuple((
        preceded(tag("slice"), preceded(skip_spaces, parse_value)),
        parse_array_with_range,
        preceded(skip_spaces, parse_typ),
    ))(instr)?;
    Ok((
        instr_,
        Instruction::Slice(SliceInstr::new(arr_val, typ, range, instr)),
    ))
}

fn parse_send(instr: &str) -> IResult<&str, Instruction> {
    let (instr_, (ident, value)) = tuple((
        preceded(tag("send "), parse_ident),
        preceded(tag(" <- "), parse_value),
    ))(instr)?;
    Ok((
        instr_,
        Instruction::Send(SendInstr::new(ident, value, instr)),
    ))
}

fn parse_chan_assign_helper(instr: &str) -> IResult<&str, Instruction> {
    let (instr_, ident) = preceded(tag("<-"), parse_ident)(instr)?;
    Ok((
        instr_,
        Instruction::GetValueChan(GetValueChanInstr::new(ident, vec![], instr)),
    ))
}

fn parse_get_value_from_chan(instr: &str) -> IResult<&str, Instruction> {
    let (instr_, (ident, ret_typs)) = alt((
        parse_get_value_from_chan_without_membership,
        parse_get_value_from_chan_with_membership,
    ))(instr)?;
    Ok((
        instr_,
        Instruction::GetValueChan(GetValueChanInstr::new(ident, ret_typs, instr)),
    ))
}

fn parse_get_value_from_chan_without_membership(instr: &str) -> IResult<&str, (Value, Vec<Typ>)> {
    tuple((
        preceded(tag("<-"), parse_ident),
        preceded(skip_spaces, parse_func_param_typs),
    ))(instr)
}

fn parse_get_value_from_chan_with_membership(instr: &str) -> IResult<&str, (Value, Vec<Typ>)> {
    let (instr_, (ident, _, ret_typs)) = tuple((
        preceded(tag("<-"), parse_ident),
        tag(",ok"),
        preceded(skip_spaces, parse_func_param_typs),
    ))(instr)?;
    Ok((instr_, (ident, ret_typs)))
}

fn parse_phi(instr: &str) -> IResult<&str, Instruction> {
    let (instr_, (_, _, (blk1, value1, blk2, value2), ident, typ)) = tuple((
        tag("phi"),
        skip_spaces,
        delimited(
            char('['),
            tuple((
                digit1,                                                  // block1
                preceded(char(':'), preceded(skip_spaces, parse_value)), // value1
                preceded(char(','), preceded(skip_spaces, digit1)),      // block2
                preceded(char(':'), preceded(skip_spaces, parse_value)), // value2
            )),
            char(']'),
        ),
        preceded(skip_spaces, preceded(char('#'), parse_value)),
        preceded(skip_spaces, parse_typ),
    ))(instr)?;

    let blk1 = blk1.parse::<u64>().unwrap();
    let blk2 = blk2.parse::<u64>().unwrap();
    Ok((
        instr_,
        Instruction::Phi(PhiInstr::new(ident, typ, blk1, value1, blk2, value2, instr)),
    ))
}

fn parse_typ_assert_without_ok(instr: &str) -> IResult<&str, Instruction> {
    let (instr_, (ident, assert_typ, ret_typ)) = tuple((
        preceded(tag("typeassert "), parse_ident),
        preceded(char('.'), delimited(char('('), parse_typ, char(')'))),
        preceded(skip_spaces, parse_typ),
    ))(instr)?;
    Ok((
        instr_,
        Instruction::TypeAssert(TypeAssertInstr::new(
            ident,
            assert_typ,
            vec![ret_typ],
            instr,
        )),
    ))
}

fn parse_typ_assert_with_ok(instr: &str) -> IResult<&str, Instruction> {
    let (instr_, (ident, assert_typ, ret_typs)) = tuple((
        preceded(tag("typeassert,ok "), parse_ident),
        preceded(char('.'), delimited(char('('), parse_typ, char(')'))),
        preceded(skip_spaces, parse_func_param_typs),
    ))(instr)?;
    Ok((
        instr_,
        Instruction::TypeAssert(TypeAssertInstr::new(ident, assert_typ, ret_typs, instr)),
    ))
}

fn parse_typ_assert(instr: &str) -> IResult<&str, Instruction> {
    alt((parse_typ_assert_with_ok, parse_typ_assert_without_ok))(instr)
}

fn parse_convert(instr: &str) -> IResult<&str, Instruction> {
    let (instr_, (to_typ, from_typ, value, ret_typ)) = tuple((
        preceded(tag("convert "), parse_typ),
        preceded(tag(" <- "), parse_typ),
        delimited(tag(" ("), parse_value, char(')')),
        preceded(skip_spaces, parse_typ),
    ))(instr)?;
    Ok((
        instr_,
        Instruction::Convert(ConvertInstr::new(from_typ, to_typ, value, ret_typ, instr)),
    ))
}

fn parse_invoke(instr: &str) -> IResult<&str, Instruction> {
    let (instr_, (interface_ident, func_ident, params, ret_typs)) = tuple((
        preceded(tag("invoke "), parse_ident),
        preceded(char('.'), parse_ident),
        parse_func_params,
        preceded(skip_spaces, parse_func_param_typs),
    ))(instr)?;
    Ok((
        instr_,
        Instruction::Invoke(InvokeInstr::new(
            interface_ident,
            func_ident,
            params,
            ret_typs,
            instr,
        )),
    ))
}

fn parse_range(instr: &str) -> IResult<&str, Instruction> {
    let (instr_, ident) = preceded(tag("range "), parse_ident)(instr)?;
    Ok((instr_, Instruction::Range(RangeInstr::new(ident, instr))))
}

fn parse_next(instr: &str) -> IResult<&str, Instruction> {
    let (instr_, (ident, ret_typs)) = tuple((
        preceded(tag("next "), parse_ident),
        preceded(skip_spaces, parse_func_param_typs),
    ))(instr)?;
    Ok((
        instr_,
        Instruction::Next(NextInstr::new(ident, ret_typs, instr)),
    ))
}

fn parse_go(instr: &str) -> IResult<&str, Instruction> {
    let (instr_, (func_ident, params)) = tuple((
        preceded(tag("go "), parse_ident),
        preceded(skip_spaces, parse_func_params),
    ))(instr)?;
    Ok((
        instr_,
        Instruction::GoRoutine(GoRoutineInstr::new(func_ident, params, instr)),
    ))
}

fn parse_send_value_to_chan(instr: &str) -> IResult<&str, Instruction> {
    let (instr_, (ident, value)) = tuple((parse_ident, preceded(tag("<-"), parse_value)))(instr)?;
    Ok((
        instr_,
        Instruction::SendValueChan(SendValueChanInstr::new(ident, value, instr)),
    ))
}

fn parse_chan_assign_exprs(instr: &str) -> IResult<&str, Vec<Box<Instruction>>> {
    let (instr_, (first_chan_expr, mut chan_exprs)) = delimited(
        char('['),
        tuple((
            alt((
                map(parse_chan_assign_helper, |expr| Box::new(expr)),
                map(parse_send_value_to_chan, |expr| Box::new(expr)),
            )),
            many0(preceded(
                tag(", "),
                alt((
                    map(parse_chan_assign_helper, |expr| Box::new(expr)),
                    map(parse_send_value_to_chan, |expr| Box::new(expr)),
                )),
            )),
        )),
        char(']'),
    )(instr)?;
    if chan_exprs.len() > 0 {
        chan_exprs.insert(0, first_chan_expr);
    }
    Ok((instr_, chan_exprs))
}

fn parse_select(instr: &str) -> IResult<&str, Instruction> {
    let (instr_, (is_blocking, chan_assign_exprs, ret_typs)) = tuple((
        alt((tag("select blocking "), tag("select nonblocking "))),
        parse_chan_assign_exprs,
        preceded(skip_spaces, parse_func_param_typs),
    ))(instr)?;
    let is_blocking = if is_blocking == "select blocking " {
        true
    } else {
        false
    };
    Ok((
        instr_,
        Instruction::Select(SelectInstr::new(
            chan_assign_exprs,
            ret_typs,
            is_blocking,
            instr,
        )),
    ))
}

fn parse_field(instr: &str) -> IResult<&str, Instruction> {
    let (instr_, (is_ref, ident, field_ident, field_idx, typ)) = tuple((
        many0(char('&')),
        parse_ident,
        preceded(char('.'), parse_ident),
        preceded(
            skip_spaces,
            delimited(char('['), preceded(char('#'), digit1), char(']')),
        ),
        preceded(skip_spaces, parse_typ),
    ))(instr)?;
    let is_ref = if is_ref.len() > 0 { true } else { false };
    Ok((
        instr_,
        Instruction::FieldAccess(FieldAccessInstr::new(
            ident,
            typ,
            is_ref,
            field_ident,
            field_idx.parse::<usize>().unwrap(),
            instr,
        )),
    ))
}

fn parse_changetype(instr: &str) -> IResult<&str, Instruction> {
    let (instr_, (to_typ, from_typ, value, _)) = tuple((
        preceded(tag("changetype "), parse_typ),
        preceded(tag(" <- "), parse_typ),
        preceded(skip_spaces, delimited(char('('), parse_value, char(')'))),
        preceded(skip_spaces, parse_typ),
    ))(instr)?;
    Ok((
        instr_,
        Instruction::Changetype(ChangetypeInstr::new(from_typ, to_typ, value, instr)),
    ))
}

fn parse_membership_test(instr: &str) -> IResult<&str, Instruction> {
    let (instr_, (ident, key_value, ret_typs)) = tuple((
        parse_ident,
        delimited(char('['), parse_value, char(']')),
        preceded(preceded(tag(",ok"), skip_spaces), parse_func_param_typs),
    ))(instr)?;
    Ok((
        instr_,
        Instruction::MembershipTest(MembershipTestInstr::new(ident, key_value, ret_typs, instr)),
    ))
}

fn parse_panic(instr: &str) -> IResult<&str, Instruction> {
    let (instr_, value) = preceded(tag("panic "), parse_value)(instr)?;
    Ok((instr_, Instruction::Panic(PanicInstr::new(value, instr))))
}

fn parse_slice_to_array_ptr(instr: &str) -> IResult<&str, Instruction> {
    let (instr_, (to_typ, from_typ, value)) = tuple((
        preceded(tag("slice to array pointer "), parse_typ),
        preceded(tag(" <- "), parse_typ),
        preceded(skip_spaces, delimited(char('('), parse_value, char(')'))),
    ))(instr)?;
    Ok((
        instr_,
        Instruction::SliceToArrayPtr(SliceToArrayPtrInstr::new(from_typ, to_typ, value, instr)),
    ))
}

fn parse_closure(instr: &str) -> IResult<&str, Instruction> {
    let (instr_, (func_ident, (first_ref, mut refs), closure_sig)) = tuple((
        preceded(tag("make closure "), parse_ident),
        preceded(
            skip_spaces,
            delimited(
                char('['),
                tuple((
                    parse_value,
                    many0(preceded(char(','), preceded(skip_spaces, parse_value))),
                )),
                char(']'),
            ),
        ),
        preceded(skip_spaces, parse_typ),
    ))(instr)?;
    let refs = if refs.len() == 0 {
        vec![first_ref]
    } else {
        refs.insert(0, first_ref);
        refs
    };
    Ok((
        instr_,
        Instruction::Closure(ClosureInstr::new(func_ident, refs, closure_sig, instr)),
    ))
}

fn parse_group1(instr: &str) -> IResult<&str, Instruction> {
    alt((
        parse_call,
        parse_alloc,
        parse_index_access,
        parse_closure,
        parse_make_assign,
        parse_slice,
        parse_unary_expr,
        parse_binary_expr,
        parse_literal_instr,
        parse_phi,
        parse_ident_instr,
        parse_extract,
        parse_change_interface,
        parse_make_call,
        parse_deref_instr,
        parse_typ_assert,
        parse_convert,
        parse_invoke,
        parse_range,
        parse_next,
        parse_get_value_from_chan,
    ))(instr)
}

fn parse_group2(instr: &str) -> IResult<&str, Instruction> {
    alt((
        parse_select,
        parse_field,
        parse_changetype,
        parse_membership_test,
        parse_slice_to_array_ptr,
        parse_unsupported_instr,
    ))(instr)
}

fn parse_expr(instr: &str) -> IResult<&str, Instruction> {
    alt((parse_group1, parse_group2))(instr)
}

fn parse_assignment(instr: &str) -> IResult<&str, Instruction> {
    alt((parse_assignment_with_map, parse_assignment_without_map))(instr)
}

fn parse_assignment_without_map(instr: &str) -> IResult<&str, Instruction> {
    let (instr_, (lhs_deref, ident, expr_instr)) = tuple((
        many0(char('*')),
        parse_ident,
        preceded(tag(" = "), parse_expr),
    ))(instr)?;
    let is_lhs_deref = if lhs_deref.len() > 0 { true } else { false };
    Ok((
        instr_,
        Instruction::Assignment(AssignmentInstr::new(
            ident,
            is_lhs_deref,
            Box::new(expr_instr),
            instr,
        )),
    ))
}

fn parse_assignment_with_map(instr: &str) -> IResult<&str, Instruction> {
    let (instr_, (lhs_deref, ident, key_value, expr_instr)) = tuple((
        many0(char('*')),
        parse_ident,
        delimited(char('['), parse_value, char(']')),
        preceded(tag(" = "), parse_expr),
    ))(instr)?;
    let is_lhs_deref = if lhs_deref.len() > 0 { true } else { false };
    Ok((
        instr_,
        Instruction::MapAssignment(MapAssignmentInstr::new(
            ident,
            is_lhs_deref,
            key_value,
            Box::new(expr_instr),
            instr,
        )),
    ))
}

fn parse_if(instr: &str) -> IResult<&str, Instruction> {
    let (instr_, (_, _, ident, _, _, _, tb, _, _, _, fb)) = tuple((
        tag("if"),
        skip_spaces,
        alt((
            // 3
            parse_literal_value,
            parse_ident,
        )),
        skip_spaces,
        tag("goto"),
        skip_spaces,
        digit1, // 7,
        skip_spaces,
        tag("else"),
        skip_spaces,
        digit1, // 11
    ))(instr)?;

    Ok((
        instr_,
        Instruction::If(IfInstr::new(
            ident,
            tb.parse::<u64>().unwrap(),
            fb.parse::<u64>().unwrap(),
            instr,
        )),
    ))
}

fn get_instr(instr: &str) -> IResult<&str, Instruction> {
    alt((
        parse_func_entry,
        parse_block_entry,
        parse_call,
        parse_assignment,
        parse_if,
        parse_jump,
        parse_return,
        parse_send,
        parse_go,
        parse_panic,
        parse_unsupported_instr,
    ))(instr)
}

pub fn parse_metadata_var(instr: &str) -> IResult<&str, Metadata> {
    let (instr_, (ident, typ, _, value, file_loc)) = tuple((
        preceded(
            char(';'),
            preceded(
                skip_spaces,
                preceded(tag("var"), preceded(skip_spaces, parse_ident)),
            ),
        ),
        preceded(skip_spaces, parse_typ),
        preceded(
            skip_spaces,
            preceded(char('@'), preceded(skip_spaces, parse_codeline)),
        ),
        preceded(
            skip_spaces,
            preceded(tag("is"), preceded(skip_spaces, parse_value)),
        ),
        preceded(tag(" --> "), parse_file_loc),
    ))(instr)?;
    Ok((
        instr_,
        Metadata::new_var_metadata(ident, typ, file_loc, value, instr),
    ))
}

pub fn parse_metadata_func(instr: &str) -> IResult<&str, Metadata> {
    let (instr_, ((package, func_ident), func_param_typs, func_ret_typs, _, value, file_loc)) =
        tuple((
            preceded(
                char(';'),
                preceded(
                    skip_spaces,
                    preceded(
                        tag("func"),
                        preceded(
                            skip_spaces,
                            tuple((parse_package, preceded(char('.'), parse_ident))),
                        ),
                    ),
                ),
            ),
            parse_func_param_typs,
            preceded(skip_spaces, parse_func_param_typs),
            preceded(
                skip_spaces,
                preceded(char('@'), preceded(skip_spaces, parse_codeline)),
            ),
            preceded(
                skip_spaces,
                preceded(tag("is"), preceded(skip_spaces, parse_value)),
            ),
            preceded(tag(" --> "), parse_file_loc),
        ))(instr)?;
    Ok((
        instr_,
        Metadata::new_func_metadata(
            package,
            func_ident,
            func_param_typs,
            func_ret_typs,
            file_loc,
            value,
            instr,
        ),
    ))
}

pub fn parse_ifloc_metadata(instr: &str) -> IResult<&str, Metadata> {
    let (instr_, (if_instr, file_loc)) = tuple((
        preceded(tag("; [if loc]: "), parse_if),
        preceded(tag(" --> "), parse_file_loc),
    ))(instr)?;
    Ok((
        instr_,
        Metadata::new_ifloc_metadata(if_instr, file_loc, instr),
    ))
}

pub fn parse_panic_metadata(instr: &str) -> IResult<&str, Metadata> {
    let (instr_, (panic_instr, file_loc)) = tuple((
        preceded(tag("; "), parse_panic),
        preceded(tag(" --> "), parse_file_loc),
    ))(instr)?;
    Ok((
        instr_,
        Metadata::new_panic_metadata(panic_instr, file_loc, instr),
    ))
}

pub fn parse_metadata_address(instr: &str) -> IResult<&str, Metadata> {
    alt((parse_metadata_address_var, parse_metadata_address_ast))(instr)
}

pub fn parse_metadata_address_ast(instr: &str) -> IResult<&str, Metadata> {
    let (instr_, (expr_typ, _, value, file_loc)) = tuple((
        preceded(tag("; address of *ast."), alpha1),
        preceded(tag(" @ "), parse_codeline),
        preceded(tag(" is "), parse_value),
        preceded(tag(" --> "), parse_file_loc),
    ))(instr)?;

    Ok((
        instr_,
        Metadata::new_address_metadata_ast(expr_typ, file_loc, value, instr),
    ))
}

pub fn parse_metadata_address_var(instr: &str) -> IResult<&str, Metadata> {
    let (instr_, (var_or_field, ident, typ, _, value, file_loc)) = tuple((
        preceded(tag("; address of "), alt((tag("var"), tag("field")))),
        preceded(skip_spaces, parse_value),
        preceded(skip_spaces, parse_typ),
        preceded(tag(" @ "), parse_codeline),
        preceded(tag(" is "), parse_value),
        preceded(tag(" --> "), parse_file_loc),
    ))(instr)?;
    Ok((
        instr_,
        Metadata::new_address_metadata_var(
            MetadataAddressTyp::new(var_or_field),
            ident,
            typ,
            file_loc,
            value,
            instr,
        ),
    ))
}

pub fn parse_metadata_ast(instr: &str) -> IResult<&str, Metadata> {
    let (instr_, (expr_typ, _, value, file_loc)) = tuple((
        preceded(
            char(';'),
            preceded(skip_spaces, preceded(tag("*ast."), alpha1)),
        ),
        preceded(
            skip_spaces,
            preceded(char('@'), preceded(skip_spaces, parse_codeline)),
        ),
        preceded(
            skip_spaces,
            preceded(tag("is"), preceded(skip_spaces, parse_value)),
        ),
        preceded(tag(" --> "), parse_file_loc),
    ))(instr)?;
    Ok((
        instr_,
        Metadata::new_ast_expr_metadata(expr_typ, file_loc, value, instr),
    ))
}

pub fn collect_metadata(metadata: Vec<&str>, line_num: usize) -> Option<Metadata> {
    let mut func_desc_metadata = vec![];
    let input_metadata_len = metadata.len();
    let mut parsed_metadata = None;
    for (idx, ir) in metadata.iter().enumerate() {
        assert!(ir.contains(";") == true || is_func_desc(ir));
        let metadata = alt((
            parse_metadata_var,
            parse_func_desc,
            parse_metadata_func,
            parse_metadata_ast,
            parse_metadata_address,
            parse_ifloc_metadata,
            parse_panic_metadata,
        ))(ir);
        if let Ok((_, metadata)) = metadata {
            match metadata {
                Metadata::VarMetadata(_)
                | Metadata::AddressMetadataVar(_)
                | Metadata::AddressMetadataAst(_)
                | Metadata::FuncMetadata(_)
                | Metadata::AstExprMetadata(_) => {
                    metadata::store_metadata(metadata.clone());
                    assert_eq!(input_metadata_len - 1, idx);
                    parsed_metadata = Some(metadata);
                    break;
                }
                Metadata::IfLocMetadata(_) | Metadata::PanicMetadata(_) => {
                    parsed_metadata = Some(metadata);
                    break;
                }

                Metadata::FuncDescMetadata(metadata) => {
                    func_desc_metadata.push(metadata.clone());
                    if metadata.prop == "Package" {
                        scope::set_cur_package(&metadata.desc);
                    }
                    if metadata.prop == "Parent" {
                        scope::set_cur_func_parent(&metadata.desc);
                    }
                    if metadata.prop == "Name" {
                        mapping::set_func_entry(&metadata.desc, line_num);
                        scope::set_cur_func_parent("");
                        scope::set_cur_func_free_vars(vec![]);
                        scope::set_cur_func_synthetic("");
                    }
                    if metadata.prop == "Synthetic" {
                        scope::set_cur_func_synthetic(&metadata.desc);
                    }
                    if metadata.prop == "Free variables" {
                        let (_, free_vars) = parse_free_vars(&metadata.desc).unwrap();
                        let free_vars = free_vars
                            .into_iter()
                            .map(|(var_idx, ident, typ)| {
                                FreeVar::new(var_idx.parse::<usize>().unwrap(), ident, typ)
                            })
                            .collect::<Vec<_>>();
                        scope::set_cur_func_free_vars(free_vars);
                    }
                    if is_func_entry(ir) {
                        metadata::store_func_desc(func_desc_metadata.clone());
                        assert_eq!(input_metadata_len - 1, idx);
                        parsed_metadata = None;
                        break;
                    }
                }
            }
        }
    }
    parsed_metadata
}

pub fn get_instrs(ir: &str) -> Vec<Instruction> {
    let mut instrs: Vec<Instruction> = vec![];
    let mut metadata = vec![];
    let mut metadata_started = false;
    let irs = ir.split("\n").collect::<Vec<_>>();
    for (idx, line) in irs.iter().enumerate() {
        if line.contains(";") {
            if let Some(md) = collect_metadata(vec![line.trim_start()], idx) {
                if let Some(mut instr) = instrs.pop() {
                    instr.update_metadata(Some(md));
                    instrs.push(instr);
                }
            }
        }
        if is_func_desc(line) {
            metadata.push(line.trim_start());
            metadata_started = true;
        }
        if !is_func_desc(line) && !line.contains(";") && line.len() != 0 {
            if metadata_started {
                collect_metadata(metadata, idx);
                metadata = vec![];
                metadata_started = false;
            }
            let (_, instr) = get_instr(line.trim_start()).unwrap();
            instrs.push(instr);
        }
    }
    instrs
}

pub fn dump_to_file(file_path: &str, instrs: &Vec<Instruction>) -> std::io::Result<()> {
    let mut instrs_str = "".to_string();
    for instr in instrs {
        instrs_str = format!("{}\n{}", instrs_str, instr.to_string());
    }
    let mut metadata_str = "".to_string();
    for metadata in metadata::get_all_metadata() {
        metadata_str = format!("{}\n{}", metadata_str, metadata.to_string());
    }
    let mut func_desc_str = "".to_string();
    for func_desc in metadata::get_all_func_desc() {
        func_desc_str = format!("{}\n{}", func_desc_str, func_desc.to_string());
    }
    let collected_str = format!(
        "[Func description]------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------\n{func_desc_str}\n[Metadata]------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------\n{metadata_str}\n\n[IR]------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------\n{instrs_str}"
    );
    std::fs::write(file_path, collected_str)?;
    println!("written to {}", file_path);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_ident() {
        let mut input = "init$guard                                                   bool";
        let (_, parsed) = parse_ident(input).unwrap();
        println!("{:?}", parsed);
        assert_eq!(Value::Ident("init$guard".to_string()), parsed);

        input = "t0 = 123";
        let (_, parsed) = parse_ident(input).unwrap();
        println!("{:?}", parsed);
        assert_eq!(Value::Ident("t0".to_string()), parsed);
    }

    #[test]
    fn test_parse_package() {
        let mut instr = "command-line-argument.A";
        let (_, parsed) = parse_package(instr).unwrap();
        assert_eq!(parsed, Value::Ident("command-line-argument".to_string()));

        instr = "gopkg.in/urfave/cli.Args";
        let (_, parsed) = parse_package(instr).unwrap();
        assert_eq!(parsed, Value::Ident("gopkg.in/urfave/cli".to_string()));

        instr = "gopkg.in/urfave/cli.v1.Args";
        let (_, parsed) = parse_package(instr).unwrap();
        assert_eq!(parsed, Value::Ident("gopkg.in/urfave/cli.v1".to_string()));

        instr = "example/mypack.ABC";
        let (_, parsed) = parse_package(instr).unwrap();
        assert_eq!(parsed, Value::Ident("example/mypack".to_string()));
    }

    #[test]
    fn test_parse_assignment() {
        let instr = "t0 = *init$guard                                                   bool";
        let (residual, parsed) = parse_assignment(instr).unwrap();
        assert_eq!(residual.len(), 0);
        parsed.dump();
    }

    #[test]
    fn test_parse_variadic() {
        let ident = "t3...";
        let (_, parsed) = parse_variadic(ident).unwrap();
        println!("{:?}", parsed);
    }

    #[test]
    fn test_parse_call() {
        let mut input = "(A).bar(t18, 1:int, 2:int)                      (int, int, error)";
        let (_, parsed) = parse_call(input).unwrap();
        println!("{:?}", parsed);

        input = "(A).bar(t18, 1:int, 2:int)                                    int";
        let (_, parsed) = parse_call(input).unwrap();
        println!("{:?}", parsed);

        input = "fmt.init()     ()";
        let (_, parsed) = parse_call(input).unwrap();
        println!("{:?}", parsed);

        input = "fmt.Println(t3...)                              (n int, err error)";
        let (_, parsed) = parse_call(input).unwrap();
        println!("{:?}", parsed);

        input = "(example/anotherpackage.ABC).Hi2(t5)                       string";
        let (_, parsed) = parse_call(input).unwrap();
        println!("{:?}", parsed);

        input = "(*example/anotherpackage.ABC).Hi2(t5)                       string";
        let (_, parsed) = parse_call(input).unwrap();
        println!("{:?}", parsed);

        input = "foo()                  (myerr1 error, myerr2 error, myerr3 error)";
        let (_, parsed) = parse_call(input).unwrap();
        println!("{:?}", parsed);
    }

    #[test]
    fn test_parse_alloc() {
        let mut input = "new [1]any (varargs)      *[1]any";
        let (_, parsed) = parse_alloc(input).unwrap();
        println!("{:?}", parsed);

        input = "local [5][2]int (nArr)         *[5][2]int";
        let (_, parsed) = parse_alloc(input).unwrap();
        println!("{:?}", parsed);

        input = "local [10][5][2]int (nArr)         *[10][5][2]int";
        let (_, parsed) = parse_alloc(input).unwrap();
        println!("{:?}", parsed);

        input = "new ***A (ptr2)                                             ****A";
        let (_, parsed) = parse_alloc(input).unwrap();
        println!("{:?}", parsed);
    }

    #[test]
    fn test_parse_index_access() {
        let input = "&t59[3:int]    *int";
        let (_, parsed) = parse_index_access(input).unwrap();
        println!("{:?}", parsed);
    }

    #[test]
    fn test_parse_make_assign_instr() {
        let input = r#"make any <- string ("HELLO":string)   any"#;
        let (_, parsed) = parse_make_assign(input).unwrap();
        println!("{:?}", parsed);
    }

    #[test]
    fn test_parse_slice() {
        let input = "slice t0[:]    []any";
        let (_, parsed) = parse_slice(input).unwrap();
        println!("{:?}", parsed);
    }

    // function metadata is ignored by grbuilder
    /*
    #[test]
    fn test_parse_func_metadata() {
        let input = "; func fmt.Println(a ...any) (n int, err error) @ 7:7 is Println";
        let parsed = parse_metadata_func(input).unwrap();
        println!("{:?}", parsed);
    }
    */

    #[test]
    fn test_parse_ast_metadata() {
        let input = "; *ast.CallExpr @ 46:2 is t41 --> /home/user/goanalyzer/example/project-example/main.go:53:2";
        let parsed = parse_metadata_ast(input).unwrap();
        println!("{:?}", parsed);
    }

    #[test]
    fn test_parse_binary_expr() {
        let mut input = "t6 == 2:int                             bool";
        let mut parsed = parse_binary_expr(input).unwrap();
        println!("{:?}", parsed);

        input = "t8 & t7                                                        int";
        parsed = parse_binary_expr(input).unwrap();
        println!("{:?}", parsed);

        input = "t8 | t7                                                        int";
        parsed = parse_binary_expr(input).unwrap();
        println!("{:?}", parsed);

        input = "t8 ^ t7                                                        int";
        parsed = parse_binary_expr(input).unwrap();
        println!("{:?}", parsed);

        input = "t8 << t7                                                        int";
        parsed = parse_binary_expr(input).unwrap();
        println!("{:?}", parsed);

        input = "t8 >> t7                                                        int";
        parsed = parse_binary_expr(input).unwrap();
        println!("{:?}", parsed);

        input = "t22 > 100:int                                                bool";
        parsed = parse_binary_expr(input).unwrap();
        println!("{:?}", parsed);
    }

    #[test]
    fn test_parse_unary_expr() {
        let mut input = "!t11                                                         bool";
        let mut parsed = parse_unary_expr(input).unwrap();
        println!("{:?}", parsed);

        input = "^t3                                                            int";
        parsed = parse_unary_expr(input).unwrap();
        println!("{:?}", parsed);

        input = "-t5                                                            int";
        parsed = parse_unary_expr(input).unwrap();
        println!("{:?}", parsed);
    }

    #[test]
    fn test_parse_phi() {
        let input = "phi [0: 0:int, 5: t13] #myidx       int";
        let parsed = parse_phi(input).unwrap();
        println!("{:?}", parsed);
    }

    #[test]
    fn test_parse_typ() {
        let input = "*example/mypack.ABC";
        let parsed = parse_typ(input).unwrap();
        println!("{:?}", parsed);
    }

    #[test]
    fn test_parse_func_entry() {
        let mut input = "func (a *example.A) bar(v1 int, v2 int) int:";
        let mut parsed = parse_func_entry(input).unwrap();
        println!("{:?}", parsed);

        input = "func init():";
        parsed = parse_func_entry(input).unwrap();
        println!("{:?}", parsed);

        input = "func init() (int, float):";
        parsed = parse_func_entry(input).unwrap();
        println!("{:?}", parsed);
        // input = "func foo() (myerr1 error, myerr2 error, myerr3 error):";
    }

    #[test]
    fn test_parse_func_desc() {
        let input = "# Name: example/mypack.init";
        let parsed = parse_func_desc(input).unwrap();
        println!("{:?}", parsed);
    }

    #[test]
    fn test_parse_func_sig() {
        let mut input = "func (abc *ABC) Hi2() string:";
        let mut parsed = parse_func_sig(input).unwrap();
        println!("{:?}", parsed);

        input = "func (a A) baz(v1 int, v2 int) (int, int):";
        parsed = parse_func_sig(input).unwrap();
        println!("{:?}", parsed);

        input = "func (a A) baz(v1 int, v2 int):";
        parsed = parse_func_sig(input).unwrap();
        println!("{:?}", parsed);
    }

    #[test]
    fn test_is_func_entry() {
        let input = "func (a A) baz(v1 int, v2 int):";
        println!("{}", is_func_entry(input));
    }

    #[test]
    fn test_parse_metadata_address() {
        let input = "; address of var abc example/anotherpackage.ABC @ 34:2 is t0 --> /home/user/goanalyzer/example/project-example/main.go:53:2";
        let parsed = parse_metadata_address(input).unwrap();
        println!("{:?}", parsed);
    }

    #[test]
    fn test_parse_metadata_array_var() {
        let input = "; var arr2 []int @ 65:6 is nil:[]int --> /home/user/goanalyzer/example/project-example/main.go:53:2";
        let parsed = parse_metadata_var(input).unwrap();
        println!("{:?}", parsed);
    }

    #[test]
    fn test_parse_return() {
        let input = "return t0";
        let parsed = parse_return(input).unwrap();
        println!("{:?}", parsed);
    }

    #[test]
    fn test_parse_extract() {
        let input = "extract t28 #2                                              error";
        let (_, parsed) = parse_extract(input).unwrap();
        println!("{:?}", parsed);
    }

    #[test]
    fn test_parse_change_interface_assign() {
        let input = "change interface any <- error (t41)                           any";
        let (_, parsed) = parse_change_interface(input).unwrap();
        println!("{:?}", parsed);
    }

    #[test]
    fn test_parse_slice_and_array() {
        let mut input = "local []int (litarr)                                       *[]int";
        let (_, parsed) = parse_alloc(input).unwrap();
        println!("{:?}", parsed);

        input = "local [5][2]int (nArr)                                *[5][2]int";
        let (_, parsed) = parse_alloc(input).unwrap();
        println!("{:?}", parsed);
    }

    #[test]
    fn test_parse_string() {
        let input = "\"qwe == \":string";
        let (_, parsed) = parse_value(input).unwrap();
        println!("{:?}", parsed);
    }

    #[test]
    fn test_parse_chan_typ() {
        let mut input = "chan bool";
        let (_, parsed) = parse_typ_str(input).unwrap();
        println!("{:?}", parsed);

        input = "local chan bool (ch)        *chan bool";
        let (_, parsed) = parse_alloc(input).unwrap();
        println!("{:?}", parsed);

        input = "make chan bool t139           chan bool";
        let (_, parsed) = parse_make_call(input).unwrap();
        println!("{:?}", parsed);
    }

    #[test]
    fn test_parse_send() {
        let input = "send t141 <- true:bool";
        let (_, parsed) = parse_send(input).unwrap();
        println!("{:?}", parsed);
    }

    #[test]
    fn test_parse_complex() {
        let mut input = "local complex128 (ca)               *complex128";
        let (_, parsed) = parse_alloc(input).unwrap();
        println!("{:?}", parsed);

        input = "(-1 + 2i):complex128";
        let (_, parsed) = parse_expr(input).unwrap();
        println!("{:?}", parsed);
        println!("{}", parsed.to_string());
    }

    #[test]
    fn test_parse_select() {
        let mut input = "select blocking [t126<-true:bool, <-t127] (index int, ok bool, bool)";
        let (_, parsed) = parse_select(input).unwrap();
        println!("{:?}", parsed);

        input = "select blocking [t126<-true:bool, t127<-true:bool] (index int, ok bool)";
        let (_, parsed) = parse_select(input).unwrap();
        println!("{:?}", parsed);

        input = "select blocking [<-t126, <-t127] (index int, ok bool, bool, bool)";
        let (_, parsed) = parse_select(input).unwrap();
        println!("{:?}", parsed);
    }

    #[test]
    fn test_parse_package_with_ident() {
        let mut input = "[]*net/url.URL";
        let (_, parsed) = parse_package_with_ident(input).unwrap();
        println!("{:?}", parsed);
        assert_eq!(parsed.1, "URL");

        input = "Status";
        let (_, parsed) = parse_package_with_ident(input).unwrap();
        println!("{:?}", parsed);
        assert_eq!(parsed.1, "Status");
    }

    #[test]
    fn test_parse_untype() {
        let input = "if true:untyped bool goto 4 else 2";
        let (_, parsed) = parse_if(input).unwrap();
        println!("{:?}", parsed);
    }

    #[test]
    fn test_metadata_file_loc() {
        let input = "; var abcabc2 *example/mypack.ABC @ 53:2 is t9 --> /home/user/goanalyzer/example/project-example/main.go:53:2";
        let metadata = alt((
            parse_metadata_var,
            parse_func_desc,
            parse_metadata_func,
            parse_metadata_ast,
            parse_metadata_address,
            parse_ifloc_metadata,
        ))(input);
        println!("{:?}", metadata);

        let input = "; [if loc]: if t10 goto 1 else 3 --> /home/user/goanalyzer/example/panic-reachable-example/main.go:11:5";
        let metadata = alt((
            parse_metadata_var,
            parse_func_desc,
            parse_metadata_func,
            parse_metadata_ast,
            parse_metadata_address,
            parse_ifloc_metadata,
        ))(input);
        println!("{:?}", metadata);

        let input = "; [if loc]: if t0 goto 2 else 1 --> -";
        let metadata = alt((
            parse_metadata_var,
            parse_func_desc,
            parse_metadata_func,
            parse_metadata_ast,
            parse_metadata_address,
            parse_ifloc_metadata,
        ))(input);
        println!("{:?}", metadata);
    }
}
