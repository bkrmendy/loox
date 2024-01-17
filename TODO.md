## Goals and Ambitions

### Correctness
- [ ] expression evaluation shouldn't return a new env
- [ ] statement evaluation shouldn't return an expression (+ solve the testing/repl printing problem)

### Infrastructure
- [ ] implement synchronization and error signaling for all statements and expressions
- [ ] add line + column information to tokens and expressions

### Language features
- [ ] parse/eval while loops

- [ ] parse/eval objects
- [ ] make sure only objects have reference semantics
- [ ] make sure numbers/strings etc have value semantics

- [ ] parse/eval scopes
- [ ] parse/eval chained if/else if/else

- [ ] [BUG] recursive functions don't work
- [ ] add support for function literals
- [ ] add support for return statements and clarify trailing statement

### VM
- [ ] implement the compiler
- [ ] implement the interpreter

### Standard library
- [ ] print, read from terminal, file
- [ ] built-in arrays + functions
- [ ] built-in hashmap + functions

### Type system
- [ ] add a type system
- [ ] add type checking
- [ ] add support for type annotations

### Ecosystem
- [ ] Language server that implements LSP
- [ ] VSCode extension

### Done
- [x] propagate errors from parsing
- [x] parse/eval ifs
