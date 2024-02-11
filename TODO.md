## Goals and Ambitions

### Correctness
- [ ] statement evaluation (maybe) shouldn't return an expression (+ solve the testing/repl printing problem)
- [ ] add support for return statements and clarify the trailing statement question
- [ ] return from if/else 

### Infrastructure
- [ ] implement synchronization and error signaling for all statements and expressions
- [ ] create a sep_by function for parameter lists and object literals

### Language features
- [ ] parse/eval scopes
- [ ] parse/eval chained if/else if/else
- [ ] add support for function literals

### VM
Support for compiling and running the following
- [ ] compile unary expressions
- [ ] expressions, except function calls, function literals and prop accesses
- [ ] variable declarations and accesses
- [ ] add support for variable decl
- [ ] implement the compiler
- [ ] implement the bytecode interpreter

### Standard library
- [ ] read from terminal, file
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
- [x] add source location information to expressions and statements
- [x] print to terminal
- [x] add source location information to tokens
- [x] expression evaluation shouldn't return a new env
- [x] make sure only objects have reference semantics
- [x] make sure numbers/strings etc have value semantics
- [x] parse/eval objects
- [x] parse/eval property accesses
- [x] parse/eval while loops
- [x] propagate errors from parsing
- [x] parse/eval ifs
- [x] [BUG] recursive functions don't work