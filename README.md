# ToyML interpreter

## Usage
### Build
```
stack build
```

### Run
```
stack exec toyml                    # run repl
stack exec toyml example/sample1.ml # read from file
```

## Syntax of ToyML
```
e ::= id
    | int_literal | bool_literal
    | prefix_op e
    | e infix_op e
    | `let` id `=` e `in` e
    | `fun` id `->` e
    | e e
    | `if` e `then` e 
    | `if` e `then` e `else` e
    | e `&&` e | e `||` e
id ::= [a-zA-z][a-zA-z'_]*
int_literal ::= digit+
bool_literal ::= `true` | `false`
infix_op ::= `*` | `/` 
           | `+` | `-` 
           | `<` | `>` | `=` | `<>` | `<=` | `>=`  
prefix_op ::= `+` | `-`
```
