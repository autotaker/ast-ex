# ToyML interpreter

## Usage
### Build
```
stack build
```

### Run
```
stack exec ast-ex                    # run repl
stack exec ast-ex example/sample1.ml # read from file
```

## Syntax of ToyML
```
e ::= id
    | int_literal | bool_literal
    | e op e
    | `let` id `=` e `in` e
    | `fun` id `->` e
    | e e
    | `if` e `then` e 
    | `if` e `then` e `else` e
    | e `&&` e | e `||` e
int_literal ::= digit+
bool_literal ::= `true` | `false`
op ::= `*` | `/` | `+` | `-` | `<` | `>` | `=` | `<>` | `<=` | `>=`  
```
