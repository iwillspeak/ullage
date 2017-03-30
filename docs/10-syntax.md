# Syntax & Grammar

Ullage files are plain-old UTF-8. The language itself is built mainly around *words* rather than fancy sigils; more reminiscent of Ruby than C and friends.

[TOC]

## Tokens

Source text is treated as one of four basic token types: *words*, *punctuation*, *literals* and *whitespace*. Words and whitespace are unicode-aware.

### Words

Word tokens start with an alphabetic character or an underscore. They may then contain any number of alphanumeric or underscore characters.

Examples of words are: `foo`, `fn`, `_1` and `∂`. Some words have special meanings in the grammar:

    if unless else while end fn var let print

### Punctuation

Punctuation characters, such as `-` and `!=` are used to represent operators in the language. Currently a handful of punctuation characters are recognised: `=`, `==`, `!`, `!=`, `+`, `-`, `*`, `/`, `(`, `)`, `[`, `]`, `,`, `:`, `<`, and `>`. 

### Literals

Literals are used to represent constant input values to the program. Currently only decimal numeric literals are supported. Decimal consist of one or more consecutive digits: `0`, `42`, `1337`. Although `-47` evaluates to a negative number the `-` isn't part of the literal; in fact it is an operator.

### Whitespace

Whitespace tokens are made up of one or more *space* characters. These *space* characters are either Unicode whitespace, such as tabs & spaces, or comments. Comments are introduced with a `#` and continue to the end of the line.

    # This is a comment!