### grparser

grparser is a Golang parser for Intermediate Representation (IR), named after the combination of "Go," "IR," and "parser".

### TODO


### Implementation
grparser can recognize all types in Golang. It parses Golang's IR line by line and converts them into an `Instruction` type of enum.
All IRs are associated with a function and package, and every Instruction type contains the corresponding package and function signature.
Additionally, if there is debug information available for the IR, the metadata is appended into the `Instruction` enum as the metadata field.


Consider given IR below.
```
# Name: (*example.A).foo
# Package: example
# Location: /home/user/goanalyzer/example/main.go:12:13
# Locals:
#   0:  t0 *A
#   1:  t1 int
#   2:  t2 int
#   3:  t3 int
#   4:  t4 string
func (a *A) foo(v1 int, v2 int, v3 int, s string) int:
0:                                                                entry P:0 S:0
    t0 = local *A (a)                                                   **A
    *t0 = a
```

The converted Instruction of `*t0 = a` is
```
Assignment(AssignmentInstr { instr_scope: InstrScope { func_info: FuncInfo { codeline: CodeLine { row: 12, col: 13 }, func_sig: FuncSig { package: "example", receiver_ident: Some(Ident("a")), receiver_typ: Some(Ptr(Struct("example.A"))), func_ident: Ident("foo"), func_param_typs: [Int, Int, Int, String], func_ret_typs:
 [Int] } }, blk_num: 0 }, lhs: Ident("t0"), is_lhs_deref: true, rhs_expr: Ident(IdentInstr { instr_scope: InstrScope { func_info: FuncInfo { codeline: CodeLine { row: 12, col: 13 }, func_sig: FuncSig { package: "example", receiver_ident: Some(Ident("a")), receiver_typ: Some(Ptr(Struct("example.A"))), func_ident: Ident(
"foo"), func_param_typs: [Int, Int, Int, String], func_ret_typs: [Int] } }, blk_num: 0 }, ident: Ident("a"), metadata: [], raw_ir: "a" }), metadata: [], raw_ir: "*t0 = a" })
```

### Instruction Note
- [Semantic of instruction]
    - change interface <lhs_typ> <- <rhs_typ>`
        - semantic: typ1 = typ2
        - ex) `change interface any <- error (t198)`

- [Type to string]
	- Function pointer type discards identifiers and only retains types

- [Function finder]
    - A first instruction of function can be retrieved through `get_func_entry(<package/type/function_name>)` in the `mapping.rs`. If the error `FuncNotFound` is returned, it means no function definition was found in the given IRs.
    Thus, that function must be builtin(https://pkg.go.dev/builtin)) function or external library function.

