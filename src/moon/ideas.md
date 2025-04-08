# Moon

A spiritual successor to Lua. Yes, Lua is great, but there are a few
things that I think could be improved, especially if we are going to
start from scratch. There are also a few things that I think are
wonderful, and I'd also like to retain this.

Moon will be a very simple extension language, used for doing very
simple interaction with the game world. Therefore it must be able to
perform introspection of the entity component system.

I'd like a Moon file to be a module. Modules are referenced by name, or
by filename. Referencing a module multiple times, possibly from
different modules, will only import the module once.

Each module has some 'global' variables that are global to that module.
All imported modules are imported to variables. Each module has a set of
functions that define code that can be called.

There are a very limited number of types:

 - nil
 - integer (i64)
 - number (f64)
 - string (reference counted extensible memory)
 - table (hash map of key, value pairs)
 - function (reference to a function)

## Grammer

The syntax of Moon is defined in extended BNF.
 - \{A\} means 0 or more As
 - \[A\] means an optional A


```
module ::= statements

statements ::= {stat}

block ::= `{` statements [`return` [expr]] `}`

stat ::= `;` |
         block |
         vardecl |
         funcdecl |
         `while` expr [capture] block |
         `if` expr [capture] block [`else` block] |
         name `=` expr

retstat ::= `return` expr [`;`]

vardecl ::= `var` name `=` expr |
            `const` name `=` expr

funcdecl ::= `fn` name `(` paramlist `)` block

paramlist ::= [name {`,` name} `,`]

expr ::= `nil` |
         `false` |
         `true` |
         number |
         integer |
         literal_string |
         table_constructor |
         prefix |
         expr binop expr |
         unop exp

prefix ::= var | prefix `(` arglist `)` | `(` expr `)`

var ::= name | prefix `.` name | prefix `[` expr `]`

arglist ::= [ expr { `,` expr } [`,`] ]

table_constructor ::= `{` [fields] `}`

fields ::= field { `,` field } [`,`]

field ::= name `=` expr |
          `[` expr `]` `=` expr |
          expr

binop ::= `+` | `-` | `*` | `/` | `%` | `<<` | `>>` |
          `&` | `|` | `^` | `and` | `or` | `<` | `>` |
          `<=` | `>=` | `==` | `!=`

unop ::= `not` | `-` | `~`

```

## Precedence Table

    1 or                        logical or
    2 and                       logical and
    3 < > <= >= != ==           comparative
    4 + - | ^                   additive
    5 * / % << >> &             multiplicative
    6 not - ~                   unary

## Operators

    `or` logical or 
    `and` logical and 
    `<` less than
    `>` greater than
    `<=` less than or equal
    `>=` greater than or equal
    `!=` not equal
    `==` equal
    `+` addition
    `-` subtraction
    `|` binary or
    `^` binary xor
    `*` multiplication
    `/` division
    `%` modulus
    `<<` left shift
    `>>` right shift
    `&` binary and
    `not` logical not
    `-` unary negative
    `~` binary negative
