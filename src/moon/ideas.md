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

## Modules

A module is a single unit of code and data. Each module has zero or more
global variables and zero or more functions. The concept of a global
variable is only global within a module. Any other module defined in the
system cannot access the state of any other module without first having
imported that module.

### API

```zig
var mod = m.create_module ();
mod.create_constant ("pi", .{ .number = 3.1415926 });
mod.create_constant ("days", .{ .integer = 7 });
mod.create_variable ("counter", .{ .integer = 0 });
mod.set ("counter", .{ .integer = 1 });
var counter_value = mod.get ("counter");
var counter = mod.get_integer ("counter") orelse 0;
mod.create_cfunction ("add", add);
```

## Functions

A function is a single block of code and its associated static values.
All computation is performed using the execution stack. Functions are
associated with a single module and can directly load and store values
into the module's global variables.

Any constants used by the function are stored with this function. This
logically means that each function is separate, but also means that if
lots of functions in a module uses a lot of the same static values, then
this data will be duplicated. This does allow for more static values to
be used across a large module, at the expense of some duplication. It
also means that each function is self contained.
