# Syntax & Grammar

Ullage files are plain-old UTF-8. The language itself is built mainly around *words* rather than fancy sigils; more reminiscent of Ruby than C and friends.

[TOC]

## Tokens

Source text is treated as one of four basic token types: *words*, *punctuation*, *literals* and *whitespace*. Words and whitespace are unicode-aware.

### Words

Word tokens start with an alphabetic character or an underscore. They may then contain any number of alphanumeric or underscore characters.

Examples of words are: `foo`, `fn`, `_1` and `∂`. Some words have special meanings in the grammar:

    if unless else while until end fn var let print

### Punctuation

Punctuation characters, such as `-` and `!=` are used to represent operators in the language. Currently a handful of punctuation characters are recognised: `=`, `==`, `!`, `!=`, `+`, `-`, `*`, `/`, `(`, `)`, `[`, `]`, `,`, `:`, `<`, and `>`. 

### Literals

Literals are used to represent constant input values to the program. Literals can be used to specify values of any primitive type (`Number`, `Bool` and `String` so far). Numeric literals consist of one or more consecutive digits: `0`, `42`, `1337`. Although `-47` evaluates to a negative number the `-` isn't part of the literal; in fact it is an operator.

### Whitespace

Whitespace tokens are made up of one or more *space* characters. These *space* characters are either Unicode whitespace, such as tabs & spaces, or comments. Comments are introduced with a `#` and continue to the end of the line.

    # This is a comment!

## Types

There are three main base types: `Number`, `String` and `Bool`. These can be extended by creating arrays and tuples.

### `Bool`

A boolean holds a single bit of information. Boolean values can be created wit the literals `true` and `false`; or as the result of a comparison (`foo == 100`).

### `Number`

Number values hold whole numbers, or integers. Currently only decimal numeric literals are supported. All number values are stored in a 64 bit integer value.

### `String`

String literals define a sequence of unicode code points. All strings in the language are UTF-8.

    'I am a string'

### Arrays

An array type is declared by wrapping an existing type in `[]`. For example `[Number]` is the type for an array of `Number` values. To create a new array an array literal can be used:

    [1, 2, 3, 4]

All the values in an array must be of a single type.

### Tuples

Tuples are similar to arrays but can contain values with different types. Tuples are defined by wrapping a comma-separated list of types in `()`. For example the type `(Number, Bool)` defines a two-element tuple with the first element a `Number` and the second a `Bool`. Instances of a tuple can be created by wrapping values in `()`:

    (100, false)

## Variables

Variables are introduced with the `var` keyword followed by an identifier and an optional type. Immutable variables can be introduced with the `let` keyword.

    let foo = 100
    var bar: Number = 10

## Operators

Ullage has both infix and prefix operators. Operators are grouped by precedence. Precedence can be overridden or enforced with parentheses.

The following infix operators exist, in precedence order from lowest to highest:

 * `==`, `!=`, `<`, `>` - Comparison operators
 * `+`, `-` - Addition and Subtraction
 * `*`, `/` - Multiplication and division

The following infix operators exist. All prefix operators bind directly to the next expression:

 * `-`, `+`, `!`

## Compound Expressions

As well as just simple expressions Ullage supports compound ones too. There aren't that many at this time:

### While loop

A basic while loop has the form:

```
while foo
   do_stuff()
   ..
end
```

This continues until the expression `foo` is false.

There is also an alternate form, the `until` loop which continues until the expression is true:

```
until bar
   do_other_stuff()
end
```

### Print Expression

The `print` word acts as a prefix operator. It will print the expression immediately to the right of it to standard output and return the value.

```
print 'hello world'
```

### Ternary Conditional Expression

The ternary expression in Ullage has two forms: `if` and `ulesss`.

```
foo if bar else baz
```

will evaluate the expression on the left hand side if the condition `bar` is true, or the right hand size `baz` if false.

The `unless` expression does the opposite:

```
biff unless bop else buzz
```

### Function Expression

Finally an `fn` expression can be used to introduce a function:

```
fn mod(n: Number, d: Number): Number
   n if n < d else mod(n - d, d)
end
```

This defines a function named `mod` which takes two `Number` parameters, `n` and `d`, and returns a `Number`.

### Index Expressions

An index expression uses `[]` to access elements from an array. Array indices start at `0` for the first element:

```
let fuzz = [1, 2, 3, 4]
print fuzz[2] # => 3
```

### Call Expression

Functions and function-like types can be invoked with a call expression. A call expression begins with a left-hand side which references an invokeable, such as a function's identifier. This is followed by the parameters to the function enclosed in parenthesis `()`:

```
fn add(n: Number, m: Number): Number
    n + m
end

print add(add(1, 2), 3) # => 6
```

## Grammar

The following grammar defines the language:


```
def foo(bar):
	pass
```


    :::python

    identifier = WORD
               ;
    
    expression = nud [led]
               ;
    
    expressions = expression*
                ;
    
    type_ref = ":" ty
             ;
    
    ty = WORD
        | "[" ty "]"
    	| "(" [(ty (, ty )*] ")"
    	;
    
    optional_type_ref = [type_ref]
                      ;
    
    typed_id = identifier optional_type_ref
             ;
    
    declaration = identifier optional_type_ref "=" expression
                 ;
    
    block = expression* "end"
           ;
    
    ternary_body = expression "else" expression
                 ;
    
    led = "==" expression
        | "!=" expression
        | "<" expression
        | ">" expression
        | "+" expression
        | "-" expression
        | "*" expression
        | "/" expression
        | "[" expression "]"
        | "(" [expression ("," expression)*] ")"
        | "if" ternary_body
        | "unless" ternary_body
        ;
    
    nud = "fn" identifier "(" [typed_id (, typed_id)*] ")" type_ref block "end"
        | ("until" | "while") expression block "end"
        | "let" declaration
        | "var" declaration
        | "print" expression
        | "true"
        | "false"
        | WORD
        | LITERAL
        | "+" expression
        | "-" expression
        | "!" expression
        | "(" expression ")"
        ;

    
