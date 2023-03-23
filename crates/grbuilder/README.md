### grbuilder

grbuilder builds the project and converts it into the SSA form (IR), and debloats unnecessary IRs.

### Prerequisite

- The Go project must be buildable. Otherwise, the generated SSA file will lack some of the IRs
- The `gotools/ssadump`` binary is a necessary tool for building grbuilder. The following describes the underlying behavior of gotools/ssadump.
    - The current `gotools/ssadump` tool automatically cut off the length of literal stirng to 20 chars.
        - The original literal string should be preserved as it is.
    - The full path of the file location in the debug metadata is not displayed, which makes it difficult to accurately report bugs for localization purposes
        - File location should be provided together.
    - Some instructions do not have metadata to be mapped to source code location, such as If and Panic instruction.
        - These instructions should be modified to have metadata for accurate reporting.

I have modified the `gotools/ssadump` tool to address the three underlying behaviors mentioned above, in order to make it more widely usable. The modified code can be found at https://github.com/hyunsooda/tools.

### How to run
cargo run <go proejct directory path> (ex. cargo run ../../example)

### Debloated
1. Wrapper functions removed
2. Function debug info removed
3. `rundefers` removed
4. Removed package and function signatures
5. Removed `Locals` information of function description

### Reproduction
1. `FreeVar` property of the function description is reorderd. Given,
```
# Free variables:
#   0:    idx1 *int
#   1:    idx2 *int
#   2:    idx3 *int
```
is converted as:
```
# Free variables: [0:idx1 *int, 1:idx2 *int, 2:idx3 *int]
```

### Splitted IRs
`main.goir`: Set of instructions
`types.goir`: Set of type definitions
`const.goir`: Set of const variables

## Note
- The `types.goir` file may contain duplicate types due to packages appearing multiple times. However, `grparser` only parses unique types and guarantees that there is no duplication in type parsing.
