# `mer` programming language (prototype)

...in early development. i'm just playing around

* [introduction](#introduction)
* [building](#building)
* [usage](#usage)
  * [checking and compiling](#checking-and-compiling)
  * [disassembling and running](#disassembling-and-running)
  * [compiler language](#compiler-language)
* [language walkthrough](#language-walkthrough)

## Introduction
This is a prototype for a programming language, called Mer. It is also my very first "serious" language implementation, so I am learning as I go. Not a whole lot of thought went into the design of this language, so things are likely to change radically and regularly as I figure things out. If I'm pleased with how things turn out, the language will very probably be re-implemented with a proper specification. Hopefully this explains why the language may feel a little strange or flawed at times.

This original goals and traits for the language are:
* statically and strongly typed
* memory safety
* automatic memory management (still unimplemented to this date)
* fast enough
* in the future, be ahead-of-time compiled to machine code (currently run via the vm)
* simple syntax, readable code

Mer is inspired by low level languages like Rust, C (and maybe even Zig), but also by Lua for its syntax.

### Just show me what it looks like
*note: i'm trying to display code blocks with the most accurate syntax highlighting -- this means sometimes changing the language inside code blocks.*

Consider checking the current [language walkthrough](#language-walkthrough).

#### hello, world!
```swift
print &"hello, world!\n"
return
```

#### Numbers and booleans
```swift
var x = 12
var y = 19
var z = (x - y) * 2
debug z

var a = 3.1415
var b = 1.5
var c = (a - 1.0) / (b + 1.0)
debug c

var f = true
var g = false
var h1 = (f and g) xor (f or not g)
var h2 = (f & g) ^ (f | not g)

return
```

#### Tuples, arrays and pointers
```swift
var t = (16.0, (1, 2, 3), 24.5, true)
debug t.0
debug t.1.2

var m = [[1, 0, 0], [0, 1, 0], [0, 0, 1]]
debug m.0.0
debug m.1.1
debug m.2.2

var p = alloc<i64>(3)
p[0] = 1
p[1] = 2
p[2] = 3

var a = (1.0, 2.0)
var b = (true, &"hello")
var c = a ++ b

var a = [true, false]
var b = [true, true, false]
var c = a ++ b

var h = "hello"
var w = "world"
var hw = h ++ " " ++ w

return
```

#### Control flow statements
```lua
var x = 321945
if x % 2 == 0 then {
    debug 1
    x = x / 2
} else {
    debug 0
    x = 3 * x + 1
}

while x > 0 do {
    x = x - 10
}

do {
    x = x + 1
} while x < 0

return
```

#### References
```swift
var x = &4
var y = &[1, 2, 3]

var z = "hello"
var p = &z
@p = "olleh"

return
```

#### Functions
```swift
func id(x: i64) -> i64
    = x

func bump(x: &i64) -> () {
    @x = @x + 1
    return
}

var x = 4
bump(&x)
debug id(x) == x

func map_point(t: (f64, f64), f: f64 -> f64) -> (f64, f64)
    = (f(t.0), f(t.1))

func g(x: f64) -> f64
    = (1.0 - x) / 2.0 + 3.0

var t = (4.56, 8.11)
var d = map_point(t, g)

return
```

#### Data structures
```swift
data Person {
    name: [&]u8
    age: i64
    height: f64
}

func birthday(them: &Person) -> () {
    print &"happy birthday, "
    print (@them).name
    print &"!\n"
    (@them).age = (@them).age + 1
    return
}

var myself = Person {
    name = &"catapille"
    age = 19, height = 1.85
}
birthday(&myself)

return
```

## Building
The only requirements are [Rust 1.74+](https://www.rust-lang.org/) and [Cargo](https://doc.rust-lang.org/stable/cargo/).

Clone the repository, move into the create directory and build the project with the release profile :
```shell
git clone https://github.com/catapillie/mer-proto
cd mer-proto
cargo build --release
```
The produced `mer` executable will be in `./target/release`.

### Testing
There are some tests available, so you can run them if you wish to do so. More tests are being written when possible.
```shell
cargo test --release
```

## Usage
Help is available for all commands and arguments by running
```shell
# general usage
mer help
# command-specific help
mer help <command>
```

### Checking and compiling
Given you have a source code file whose path is `./main.mer`, you can either *check* or *compile* your program. Compiling produces a bytecode program that can be executed with the virtual machine. Checking a program will show all compiler diagnostics, but won't generate anything. Both commands go similarly:

```shell
# checking ./main.mer
mer check ./main.out
# compiling ./main.mer into ./main.out
mer com ./main.out
```

You can provide the output path via the `-o` (or `--out`) option :
```shell
# compiling ./main.mer into ./a.out
mer com ./main.out -o ./a.out
```

By default, the compiler displays its status as it goes. This behavior can be disabled with `-q` (or `--quiet`).

If compilation is successful, you can use the `-r` (or `--run`) flag to run the produced bytecode with the virtual machine.
```shell
# compiling ./main.mer (hiding status), then running it
mer com ./main.out --run --quiet
# (shorter equivalent command)
mer com ./main.out -rq
```

### Disassembling and running
Compiled `mer` programs are bytecode programs and are executed with the provided virtual machine.

The produced bytecode can be formatted and shown using the disassembly command:
```shell
mer dis ./a.out
```
Here is the dissasembly for an empty `mer` program.
```
         ╥
00000000 ╟─── !! entry-point -> <main> [00000005]
00000005 ║ :: <main> (0 params, 0 locals)
00000016 ║              ld_unit
00000017 ║                  ret
         ╨
```

To run a compiled program, you can either use the `-r` flag when compiling, or manually run the produced bytecode. Given the path is `./a.out`, we have:
```shell
mer run ./a.out
```

### Compiler Language
Diagnostic messages are currently available in
* english (`en`)
* french (`fr`)

For any command that can print diagnostics (compiling, checking), you may specify the display language with the `--lang` (or `-l`) option, followed by the language identifier (see the list above).

Contributions for localization are welcome and appreciated! Feel free to open a PR.

## Language walkthrough
*note: everything here is likely to change a lot and frequently!*

table of contents:
* [programs](#programs)
* [expressions](#expressions)
    * [unit](#unit)
    * [integer literal](#integer-literal)
    * [floating-point number literal](#floating-point-number-literal)
    * [boolean literals](#boolean-literals)
    * [string literals](#string-literals)
    * [variable or function name](#variable-or-function-name)
    * [parenthesized expression](#parenthesized-expression)
    * [tuples](#tuples)
        * [tuple immediate index](#tuple-immediate-index)
    * [arrays](#arrays)
        * [array immediate index](#array-immediate-index)
        * [array index](#array-index)
    * [unary operation](#unary-operation)
    * [binary operation](#binary-operation)
    * [concatenation](#concatenation-operation)
    * [assignment](#assignment)
    * [function calls](#function-calls)
    * [references](#references-1)
        * [references of l-values](#references-of-l-values)
        * [references of other values](#references-of-other-values)
    * [dereferences](#dereferences)
    * ["pointers"](#pointers)
    * [data structure initialization](#data-structure-initialization)
    * [data structure field access](#data-structure-field-access)
    * [data structure with-expression](#data-structure-with-expression)
    * [case-then-otherwise expression](#case-then-otherwise-expression)
    * [unreachable and todo](#unreachable-and-todo)
    * [debug expression](#debug-expression)
* [statements](#statements)
    * [empty statement](#empty-statement)
    * [expression statement](#expression-statement)
    * [block statement](#block-statement)
    * [control flow](#control-flow)
        * [if-then](#if-then)
        * [if-then-else](#if-then-else)
        * [while-do](#while-do)
        * [do-while](#do-while)
    * [variable definition](#variable-definition)
    * [function definition](#function-definition)
        * [return statement](#return-statement)
    * [data structure definition](#data-structure-definition)
    * [print statement](#print-statement)


### Programs
Mer programs are a sequence of statements. They must explicitely return. Before we go over each statement in the language, let's first look at expressions.

### Expressions
An expression is part of the code that evaluates to a value. It can be a literal expression, like an integer, a floating-point number, a boolean literal, or a string literal, but also more a more complex one like an operation between two expressions, the value of a variable, an array of values, a call to a function etc...

Each value in Mer is given a type, and compiler ensures type-safety by infering and checking the types of expressions you write in your program.

For each expression, we will describe what it means, what its type is, and how to write it.

#### Unit
Unit is an expression that can only have one value, called "unit". It is of type unit, written `()`. The syntax for the expression is exactly the same as its type, `()`. For instance:
```swift
var u = ()
```

#### Integer literal
An integer literal is quite literally the value of an integer. They are written as a sequence of digits, like `0`, `12`, or `4895164`. Currently, there is no inference on the type of integers, so they are all considered as `i64`, which is the type for signed 64-bit integers.
```swift
12
57
```

All integer literal types are `u8`, `u16`, `u32`, `u64` (respectively 8-bit, 16-bit, 32-bit, 64-bit, all unsigned), and `i8`, `i16`, `i32`, `i64` (respectively 8-bit, 16-bit, 32-bit, 64-bit, all signed). It is currently only possible to write `i64` integer literals.

*note: `u8` literals can also be explicitely written by exploiting how strings work -- the proper way to write byte literals will be available when integer type inference is actually implemented.*

#### Floating-point number literal
Take two integer literals, put a dot in-between, and you get a floating-point number literal, for instance `14.7150`, `0.0`, `14.59`, etc... Similarly to integer literals, there is no type inference yet, so they are all considered as `f64`, which is the type for 64-bit (double-precision) floating-point numbers. 
```swift
3.14159265358979323
481.197
0.0
```
*note: there exists a `f32` type as well, but there is currently no way to write a float literal of this type*

#### Boolean literals
Booleans expressions are either `true` or `false`. They are written exactly as they value they represent, `true` or `false`. The type of booleans is written `bool`.
```swift
true
false
```

#### String literals
Strings in Mer are [UTF-8 encoded](https://en.wikipedia.org/wiki/UTF-8), so string literals are just simply [arrays](#arrays) of bytes, which represent the UTF-8 encoding of that string. String literals are written by wrapping text by double quotation marks `"`. Special characters can also be escaped inside the string with a backslash (`\n`, `\r`, `\"`, `\\` or `\0`). 

The type of a string literal is the array type on `u8` (bytes), and the size of that array is how many bytes it takes to encode the string in UTF-8.

Examples

<table>
<tr>
    <td><b>string literal</b></td>
    <td><b>resulting type</b></td>
</tr>
<tr>
<td>

```swift
"hello, world"
```

</td>
<td>

`[12]u8`

</td>
</tr>
<tr>
<td>

```swift
"let...
 ...me...
   ...be"
```

</td>
<td>

`[27]u8`

</td>
</tr>
<tr>
<td>

```swift
"what's that, some kind of \"carriage return\"?\r\n"
```

</td>
<td>

`[46]u8`

</td>
</tr>
</table>


#### Variable or function name
To refer to the value that is held by a variable, you must write the name of that variable as an expression. You can also refer to the "value" of a function (not to be confused with the value it returns -- this is explained in more detail [below](#function-definition)). Variable or function names are written as an identifier, which may start by any alphabetic character (letter) or an underscore, and may be followed by zero or more alphanumerical characters (letters or digits), or more underscores.
```
my_variable
my_function
abc
x
var1
var2
_variable
_
___
my_cool123___variable
```
The type of the resulting expression is the type of variable (or the function when a function is being referred to).

#### Parenthesized expression
Sometimes you need to wrap an expression in parentheses so that it is evaluated first, for instance where operation precedence comes into play. This is done, naturally, by opening a left parenthesis `(`, writing the inner expression, then closing with a right parenthesis `)`.
```swift
(123)
((((true))))
(())
(2.0 * (3.0 + x))
```
The type of a parenthesized expression is the same type as the inner expression's type.

#### Tuples
Tuples are expressions which contain two or more expressions. To write a tuple, you must first open a left parenthesis `(`, then write the tuple's inner expressions in sequence and separated by a comma `,`, then finally close with a right parenthesis `)`. For instance:
```swift
(1, 2, 3)
(1.0 + 2.0, x, true, 123)
((1, 0, 0), (0, 1, 0), (0, 0, 1))
```
Trying to write a tuple of arity 1 (a single value inside a tuple, also called a singleton tuple) will not result in a tuple expression, but rather a parenthesized expression -- which is not a tuple.

The type of a tuple is called the "tuple type", which itself contains the types of its inner values. It is written the same way a tuple expression is written, but with types instead. Some examples:
* `(1, 2, 3)` is of type `(i64, i64, i64)`
* if `x` is of type `α`, then `(1.0 + 2.0, x, true, 123)` is of type `(f64, α, bool, i64)`
* `((1, 0, 0), (0, 1, 0), (0, 0, 1))` is of type `((i64, i64, i64), (i64, i64, i64), (i64, i64, i64))`

Generally, if we have a tuple with `n` values, and that `a_1`, `a_2`, ..., `a_n` are of type `t_1`, `t_2`, ... `t_n` respectively, then the tuple `(a_1, a_2, ..., a_n)` is of type `(t_1, t_2, ..., t_n)`.

#### Tuple immediate index
You can retrieve the value inside a tuple by "indexing" the tuple with an "immediate index". To do this, you can follow an expression, whose type is a tuple, by `.` then an integer literal that represents the index. Tuples are zero-indexed, so to get the first value of a tuple, you write `.0`.

Immediate indices are called "immediate" because they are known at compile-time, and so, directly refer to which value in the tuple you want to retrieve. You cannot index a tuple with a value which is not an integer, because the compiler needs to known which value you want to extract out of the tuple. In addition, indexing a tuple at an index which is greater or equal to the size of the tuple will cause a compiler error, because it would be an attempt at indexing the tuple at an out-of-bounds index.

```swift
(true, 1.0, x).1
my_tuple.18
((1, 0), (2, 1), (3, 2)).0
((1, 0), (2, 1), (3, 2)).0.1
```

The type of a tuple immediate index expression is the type of the inner value being extracted out of the tuple.
* `(true, 1.0, x).1` is of type `f64`
* if `my_tuple` is a tuple of type `(t_1, t_2, ..., t_18, ...)` of size at least 18, then `my_tuple.18` is of type `t_18`
* `((1, 0), (2, 1), (3, 2)).0` if of type `(i64, i64)`
* `((1, 0), (2, 1), (3, 2)).0.1` if of type `i64`

#### Arrays
Arrays are a special case of [tuples](#tuples) where all the inner values are of the same type. The syntax remains the same, except that the opening and closing parentheses (`(` and `)`) become an opening bracket `[` and a closing bracket `]` respectively. The values inside are still comma-separated. When writing an array expression, the inner expressions must all be of the same time. If this is not guaranteed, the code will not compile, and the compiler will tell you so.
```swift
[1.0, 4.0, 9.0, 16.0, 36.0]
[(true, false), (false, false), (true, true)]
[[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0]]
```
Each array has a known size, and this is reflected in the type of the array itself, called the "array type". If we have an array whose inner values are all of type `α`, and whose size is `N`, then the type of the array is written `[N]α`. This can be read as "array of N alphas" (how you pronounce the type will actually depend on what `α` is).
* `[1.0, 4.0, 9.0, 16.0, 36.0]` is of type `[5]f64` ("array of five f64s")
* `[(true, false), (false, false), (true, true)]` is of type `[3](bool, bool)` ("array of three tuples of bool and bool")
* `[[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0]]` is of type `[3][4]i64` ("array of three arrays of four 64-bit signed integers")

Array types whose inner type differ, or who aren't of the same size, cannot be interchanged. This means that you cannot put an array of type `[M]β` where an array of type `[N]α` is expected, *unless* `N` and `M` are equal, and `α` and `β` are also equal.

#### Array immediate index
Similary to the [tuple immediate index](#tuple-immediate-index) expression, you can index an array with an immediate (known) index, using the exact same syntax. Unsurprisingly, you cannot index the array at an index which is greater or equal to the size of the array.
```swift
[1.0, 4.0, 9.0, 16.0, 36.0].4
my_array.18
[[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0]].0
[[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0]].0.3
```

The type of an array immediate index expression is the inner type of the array, regardless of the index. Indeed, all the values inside the array are of the same type.
* `[1.0, 4.0, 9.0, 16.0, 36.0].4` is of type `f64`
* if `my_array` is a tuple of type `[N]α` where `N` is at least 18, then `my_array.18` is of type `α`
* `[[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0]].0` if of type `[4]i64`
* `[[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0]].0.3` if of type `i64`

#### Array index
Of course, you sometimes don't know in advance where in the array you're going to look, so there is also an expression for indexing an array at an index which is unknown at compile time. To write an array index, follow your array expression with an opening bracket `[`, followed by the expression whose value is the index, then close with a closing bracket `]`. The index expression must be of type `i64` (an integer) -- the compiler prevents compilation if it is of another type.
```
[1, 2, 3][0]
[[true, false], [false, true]][1][0]
my_array[i + 1]
```
The type of an array index expression is always the inner type of the array.
* `[1, 2, 3][0]` is of type `i64`
* `[[true, false], [false, true]][1][0]` is of type `bool`
* if `i` is of type `i64`, and `my_array` is of type `[N]α`, then `my_array[i + 1]` is of type `α`.

#### Unary operation
An unary operation is an operation of arity 1 (on a single expression). All unary operators are prefix operators. Here is the list of all currently available unary operators:

| symbol | name | remarks | defined on types |
|--------|------|---------|------------------|
| `+` | numeral "positive" | this is the identity operator | `u8` `u16` `u32` `u64` `i8` `i16` `i32` `i64` `f32` `f64`
| `-` | numeral negation | only defined on signed integers and floating-point numbers | `i8` `i16` `i32` `i64` `f32` `f64`
| `not` | boolean negation | | `bool`

All unary operators are stable, meaning that if you have an unary operator applied on type `α`, then the resulting expression is also of type `α`.

#### Binary operation
A binary operation is an operation of arity 2, it takes two values in and gives back a new value. All binary operators are written with infix notation (in-between the two operands).

Almost all binary operators operate on values of the same type. They are however not all stable, some of them yielding a resulting type which differs from that of the operands.
| symbol | name | details | defined on types | resulting type |
|--------|------|--------------|------------------|----------------|
|`+`|addition|  |`u8` `u16` `u32` `u64` `i8` `i16` `i32` `i64` `f32` `f64` | stable
|`-`|substraction|  |`u8` `u16` `u32` `u64` `i8` `i16` `i32` `i64` `f32` `f64` | stable
|`*`|multiplication|  |`u8` `u16` `u32` `u64` `i8` `i16` `i32` `i64` `f32` `f64` | stable
|`/`|division|  |`u8` `u16` `u32` `u64` `i8` `i16` `i32` `i64` `f32` `f64` | stable
|`%`|modulo| uses the remainder by [truncated division](https://en.wikipedia.org/wiki/Modulo#Variants_of_the_definition), which means that the result can be negative, unlike the euclidian definition of the remainder |`u8` `u16` `u32` `u64` `i8` `i16` `i32` `i64` `f32` `f64` | stable
|`==`|equality check|  |`u8` `u16` `u32` `u64` `i8` `i16` `i32` `i64` `f32` `f64` `bool` | `bool`
|`!=`|inequality check|  |`u8` `u16` `u32` `u64` `i8` `i16` `i32` `i64` `f32` `f64` `bool` | `bool`
|`<=`|less-than-or-equal check|  |`u8` `u16` `u32` `u64` `i8` `i16` `i32` `i64` `f32` `f64` | `bool`
|`<`|less-than check|  |`u8` `u16` `u32` `u64` `i8` `i16` `i32` `i64` `f32` `f64` | `bool`
|`>=`|greater-than-or-equal check|  |`u8` `u16` `u32` `u64` `i8` `i16` `i32` `i64` `f32` `f64` | `bool`
|`>`|greater-than-or-equal check|  |`u8` `u16` `u32` `u64` `i8` `i16` `i32` `i64` `f32` `f64` | `bool`
|`&`|bitwise and|  |`u8` `u16` `u32` `u64` `i8` `i16` `i32` `i64` `bool` | stable
|`\|`|bitwise or|  |`u8` `u16` `u32` `u64` `i8` `i16` `i32` `i64` `bool` | stable
|`^`|bitwise xor|  |`u8` `u16` `u32` `u64` `i8` `i16` `i32` `i64` `bool` | stable
|`and`|short-circuiting and| skips evalutation of the second operand when the first one determines the result of the operation |`bool` | `bool`
|`or`|short-circuiting or| skips evalutation of the second operand when the first one determines the result of the operation |`bool` | `bool`
|`xor`|boolean xor|  |`bool` | `bool`

*todo: operator precendence table*

*note: the boolean xor operation is actually defined both with `xor` and `^` -- this is because it is not decided which one should be the xor operator on boolean. the current plan is to keep `xor` and ditch `^` for booleans, leaving it only for integer types*

#### Concatenation operation

In addition, there also is a concatenation binary operation between tuples are arrays, written `++`, and appends the right operand onto the left operand.
* For [tuples](#tuples), the type of the operands can be completely different, because the resulting type is just the tuple containing all the values from the first tuple followed by the values of the second tuple.
* For [arrays](#arrays), the two operands must be arrays of the same type, but not necessarily of the same size: the resulting type is an array on the same inner type, with a size equal to the sum of the sizes of the operands.

| kind | left operand type | right operand type | resulting type |
|------|--------------|---------------|----------------|
| tuples | `(a_1, a_2, ..., a_n)` | `(b_1, b_2, ..., b_m)` | `(a_1, a_2, ..., a_n, b_1, b_2, ..., b_m)` |
| arrays | `[N]α` | `[M]α` | `[N + M]α` |

#### Assignment
Assignment are also parsed as binary operations, but they get their own section here because they are a little bit different. The assigment operator is written `=`. The left operand must be an [L-value](https://en.wikipedia.org/wiki/Value_(computer_science)#Assignment:_l-values_and_r-values), which, briefly, means that the expression refers to the location of another value (i.e. a variable) -- or, as Wikipedia puts it
> An l-value refers to an object that persists beyond a single expression.

In Mer, L-values are:
* [variable identifiers](#variable-or-function-name)
* [tuple immediate index on l-values](#tuple-immediate-index)
* [array immediate index on l-values](#array-immediate-index)
* [array indices on l-values](#array-index)
* [pointer indices on l-values](#pointers)
* [dereferences](#dereferences)
* [data structure field accesses](#data-structure-field-access)

Examples
```swift
var x = 0
x = 1

var arr = [0, 1, 2]
arr[0] = 1
arr.1 = 48

var tup = (1, 2, 3)
tup.0 = 120

var c = alloc<bool>(4)
c[0] = true

var p = &&x
@@p = 512

var abc = Abc {
    a = 4
    b = true
    c = 1.5
}
abc.a = 8
abc.b = false
abc.c = 6.28
```

So, the left-hand side of an assignment must be an L-value, and the right-hand type must be some value whose type matches the type behind the L-value. The compiler should help you in case the types mismatch.

#### Function calls
Given a function, you may call that function by providing arguments, and get the resulting value returned by this function.

With a function `f` of type `α_1 α_2 ... α_n -> β`, where `N` is the number of arguments that `f` takes in (also called its arity), then the syntax for calling `f` with arguments `a_1`, `a_2`, ..., `a_n` is `f(a_1, a_2, ..., a_n)`. This gives an expression of type `β`.

A function which does not take any arguments has type `-> β` (where `β`) is the returned type. The call expression for such a function looks like `f()`.

Examples
```swift
add_my_numbers(1, 2, 3)
id(())
for_all([true, false, true])
create_something()
```

See also [function definitions](#function-definition).

#### References
*note: likely to change soon*

Reference expressions can mean two things depending on the context, even though the syntax remains the same. Taking the "reference" of a value is exactly like a prefix unary operator, with the symbol `&`. The way this operation behaves depends on the operand: more specifically, it depends on whether it is an L-value or not.

If `α` is the type of the operand, then taking a reference gives an expression of type `&α`.

#### References of L-values
*note: currently, only variables fall in this category, the rest of L-values has not yet been implemented (and so fall in the next section)*

A reference on an L-value results in the address of the variable behind the L-value. This means you can pass the address to a variable in some other other part of your code, *regardless of whether it lives longer or not*, and use that address indefinitely. The address will remain valid until it is no longer used, after which the garbage collector will free it up.

Examples
```swift
var x = 0
var p = &x
```
```swift
func duplicate_in_heap(x: i64) -> &i64
    = &x
```

As you may have noticed, the second examples returns a reference to its own argument. The compiler will detect whenever a reference to an L-value is taken, and ensure that the variable behind the L-value lies on the heap. We say that **the variable is heap-allocated**. This way, it is never possible to give a reference of a variable to a scope which outlives the variable: the variable lives on the heap, so it outlives any scope (until it is garbage-collected).

#### References of other values
*note: syntax for this __will__ be changed. current considerations are `alloc` or `heap` instead of the ampersand symbol. however this could be a problem for string literals, we don't want to write `alloc "..."` for each string literal we ever want to manipulate on the heap. but it does look good for stuff like `alloc 4` or maybe `heap [1, 2, 3]`.*

Taking the "reference" to a value which is not an L-value (i.e. it is a temporary value) will cause that value to be allocated on the heap.
Examples
```swift
&0
&[1, 2, 3, 4, 5]
&"nous sommes dans l'au-delà, et nous l'avons toujours été"
&&&&(&&true, &&&false)
```
...and the corresponding types
* `&0` is of type `&i64`
* `&[1, 2, 3, 4, 5]` is of type `&[5]i64`
* `&"nous sommes dans l'au-delà, et nous l'avons toujours été"` is of type `&[59]u8` (see [string literals](#string-literals))
* `&&&&(&&true, &&&false)` is of type `&&&&(&&bool, &&&bool)`

#### Dereferences
*note: syntax expected to change soon*

A dereference is the value behind some reference. It is (currently) written with a prefix 'at' symbol `@`. If the reference is of type `&α`, then performing a dereference will give a value of type `α`.
```go
var x = (true, [1, 0], 2.45)
var p = &x

@p
```
Here, `@p` is of type `(bool, [2]i64, f64)`, because `p` is of type `&(bool, [2]i64, f64)`.

Generally, if you have some value whose type starts with `&`, then you can dereference it.

#### "Pointers"
*note: this is a bad name -- other considerations are "span" or "slice"... "pointer" is just off*

"Pointers" are pretty much the same thing as a reference to an [array](#arrays), expect they differ in that you don't know at compile-time the size of the array behind the pointer. The size is instead stored *with* the pointer so that it can be used at runtime.

*note: getting the size of a pointer is not yet implemented*

Creating an array pointer requires knowing the type for which you want to create the array for. If `α` is the type of the values in the array you want to allocate, and `size` is an expression of type `i64` which represents how many values you want to allocate, then creating a pointer is done with the syntax `alloc<α>(size)`.
```swift
var memory = alloc<u8>(32768)
var data_points = alloc<(f64, f64)>(100)
```

The resulting type for a pointer expression is called the "pointer type", and is written `[&]α`. Note how the ampersand replaces the size in arrays of known size. This shows how this value is a pointer to an array, of *some* size.

You cannot dereference a pointer-typed value. Instead, you have to index the underlying array with some index (not necessarily known at compile-time).
```
memory[64]
data_points[0]
```
Here, `memory[64]` is of type `u8`, and `data_points[0]` is of type `(f64, f64)`, based off of the previous example in this section.

#### Data structure initialization
Once you have defined a data structure (see [data structure definition](#data-structure-definition)), you can initialize such a structure by writing the name of the data structure, then opening brackets (`{`, `}`), inside which you set all of the structure's fields with an equal sign (similarly to how an assignment is written).

```swift
var position = Vec3D {
    x = +4.01
    y = +8.55
    z = -4.97
}

var myself = Person {
    name = &"catapille"
    age = 19, height = 1.85
}
```

The type of a data structure initialization is written as the name of data structure itself. So, in the example above, we have types `Vec3D` and `Person` for variables `position` and `myself` respectively.

Note how field initializations can be put together on a single line, by separating them by a comma. Commas only separate fields if they are on the same line; there should not be a comma at the end of a line on which a field is initialized.

When initializing a data structure, all fields must be specified. Missing or non-existing fields are reported as an error by the compiler. The ordre in which fields are specified does not matter

#### Data structure field access
If you have an expression whose type is one of a data structure, you can access an inner field of said data structure with dot `.` notation. The data structure expression must be followed by a dot `.` and the name of the field. The compiler will generate an error if the field doesn't exist.

```swift
var position = Vec3D {
    x = +4.01
    y = +8.55
    z = -4.97
}

var myself = Person {
    name = &"catapille"
    age = 19, height = 1.85
}


position.x
position.y
position.z

myself.name
myself.age
myself.height
```

The type of a field access is the type of the field declared in the data structure.

#### Data structure with-expression
`with`-expressions allow you to copy a data structure and to re-specify a set of given fields. This is essentially sugar syntax, and is very similar to the one for [data structure initialization](#data-structure-initialization).
```cs
data Abc {
    a: i64
    b: (bool, bool)
    c: f64
}

var abc = Abc {
    a = 4
    b = (true, false)
    c = 3.14
}

var xyz = abc with {
    c = 8.00
}
```
In the example above, `xyz` is a copy of `abc`, but with the field `c` replaced with value `8.00`, instead of `3.14`.

The type of a `with`-expression is the same as the type of the data structure being copied and modified. Type-checking works the same for fields as it does when data structures are initialized. It is not required to re-specify all fields.

The compiler will emit a warning if no fields are re-specified:
```cs
var abc_copy = abc with { }
```
In this example, the `with`-expression can be removed (only leaving the copied data structure).

There will also be a warning for when *all* fields are re-specified, because then the original data structure being copied is not used at all:
```cs
var x = abc with {
    a = 42
    b = (false, false)
    c = 8.00
}
```
In the example above, the `with`-expression can simply be replaced by a [data structure initialization](#data-structure-definition) expression.

#### Case-then-otherwise expression
Some languages have a ternary expression that goes like `condition ? a : b`, which means "take `a` if the condition evaluates to `true`, otherwise take `b`". This construct is called the terary operator because it takes three expressions as inputs (the condition and two values).

Mer has a similar feature, but instead of being a ternary (arity 3) expression, `case-then-otherwise` expressions are of arity `2n + 1`, where `n` is the number of conditions (or guards) to check.

The syntax goes like this
```lua
case {
    condition_1 then value_1
    condition_2 then value_2
    ...
    condition_n then value_n
    otherwise value_fallback
}
```

This is a operation of arity `2n + 1`, because we have 1 fallback, and to that add all the conditions and values, both of which we have `n`.

The resulting value of this expression is the first value whose condition evaluates to `true`. Conditions are tested in the order they are written in. If none of the condition evaluate to `true`, then the fallback value is taken instead. All of the `value`s, including the `value_fallback`, must be of the same type, which is also the type of the whole expression. And obviously, all the `condition`s must be of type `bool`.

A special case is the ternary case-then-otherwise: with just one condition, you find yourself with the common "ternary" expression.
```lua
case {
    condition then a
    otherwise b
}
```
In this case (and only in this case), you can omit the braces `{` `}`.
```lua
case
    condition then a
    otherwise b
```

Another degenerate case is when you have no condition at all. 
```swift
case {
    otherwise value
}
```
which obviously equates
```
value
```

#### Unreachable and todo
If there is a path in your code that is unreachable in a specific context, but that the compiler cannot tell, you can throw in an `unreachable` expression where you know the code will never evaluate. This makes the compiler shut up. If the code does actually get run, the virtual machine yields the `reached 'unreachable' opcode` error, and the program halts.

Similarly, you can indicate a piece of code yet unimplemented or left to be done with the `todo` expression. Attempting to evaluate this expression will yield the `reached 'todo' opcode` and the program will halt as well.

`unreachable` and `todo` are similar in that they halt the program. This is reflected in their types, which is called the "never type" (aka. the bottom type), written `!`. The never type is a subtype of any other type, so you can use an expression of type `!` wherever you want. This is okay, because no *value* will have type `!` (hence the name "never"). When the virtual machine encounters `todo` or `unreachable`, it errors and halts, so no "never value" is produced, and so type-safety is still guaranteed.

Example
```swift
func id(x: i64) -> i64
    = x
func make_sure_everything_is_okay(x: i64) -> ()
    = case
        id(x) == x then ()
        otherwise unreachable

var idk_yet = todo
make_sure_everything_is_okay(idk_yet)
```

#### Debug expression
The `debug` expression, followed by any expression whose type it is defined on, will display the value of the expression. The `debug` expression evaluates to its inner value, so it can itself be used as the value it is displaying. Its type is the type of its inner expression. `debug` is currently defined on types `u8`, `u16`, `u32`, `u64`, `i8`, `i16`, `i32`, `i64`, `f32`, `f64`, `bool`, and `()`. This expression is a temporary solution to dealing with basic IO.

### Statements
A statement is the building block that dictates what your code does. Statements can contain other statements, or do nothing other than define something.

#### Empty statement
Most of the time, you won't be able to "write an empty statement", but it's there and does exist, as we'll see in some examples (see [if-then-else](#if-then-else) or [do-while](#do-while)). The empty statement does nothing (it compiles to nothing).

#### Expression statement
An expression statement evaluates an expression then discards the result. This is useless for expressions that don't have side effects, but essential for those that do.

```swift
func do_something() -> () {
    print &"*side effect*\n"
    return
}

do_something()

"nothing" ++ "happens"
```

Here, `do_something()` is an expression statement, the expression being a [function call](#function-calls). It is not useless, as calling the function `do_something` will cause something to be printed. The discarded value is `()`.
However, the expression statement `"nothing" ++ "happens"` (the expression in question this time being a [concatenation operation](#concatenation-operation)) has no side effects, so it does nothing visible (the evaluation may or may not be performed still, but you can't tell). The discarded value here is the result of the concatenation.

#### Block statement
Block statements are a list of statements, wrapped in braces (`{` and `}`).
```swift
{
    func id(x: i64) -> i64
    {
        {
            {}
        }
        {
            return x
        }
    }
    var x = 0
}
```
In the example above, each times there is a pair of braces, it marks a block statement. Block are indented to keep the code clean. Usually you wouldn't want to throw random blocks like in the above, but this code is still valid. Here, the largest block is redundant, the second largest is necessary, and all the smaller ones are redundant.

Code inside a block is part of a smaller scope, so trying to reach declarations and definitions from a scope which you are not in is not possible. For instance, [declaring a variable](#variable-definition) inside a smaller scope using a block limits that variable to that scope, and so the fourth line below is an "unknown variable" error.
```swift
{
    var x = 0
}
debug x
```

Moreover, declarations in an inner scope take priority over declarations in a parent scope. This is called "shadowing". In this example, the second `y` variable in line 5 takes priority over the one declared in line 3, so the assignment on line 6 does not affect the first `y` variable, but the second one. Running this program shows 13 in the console.
```swift
{
    var x = 10
    var y = 3
    {
        var y = 0
        y = 5
    }
    debug x + y
}
```

Reaching for a declaration in a parent scope is perfectly fine.
```swift
{
    var x = 10
    while x >= 0 do
    {
        debug x
        x = x - 1
    }
}
```

These concepts apply to [variables definitions](#variable-definition), [functions definitions](#function-definition), and [data structure definition](#data-structure-definition).

#### Control flow
Control flow statements allow you do execute one piece of code or another depending on some condition (or "guard") evaluated at runtime. They are essentially the whole logic of your program.

#### If-then
Executes a statement if the condition evaluates to `true`. The condition of an `if-then` statement must be of type `bool`.
```lua
if some_condition then
    <statement>

if some_condition then
{
    <statements>
}
```
The inner statement of an `if-then` expression may be any *non-empty* statement, be it a block containing multiple statements, or just a single statement.

#### If-then-else
Executes a `then`-statement if the condition evaluates to `true`, otherwise executes an `else`-statement. The condition of an `if-then-else` statement must be of type `bool`.
```lua
if some_condition then
    <then_statement>
else
    <else_statement>

if some_condition then
{
    <then_statements>
}
else
{
    <else_statements>
}
```
Just like [`if-then`](#if-then) statements, `if-then-else` inner statements may be any other statement, but the `else`-statement must not be empty. It is valid to write the following:
```lua
if some_condition then
else
    <statement>
```
The `then`-statement may be empty.

#### While-do
Similarly to [`if-then`](#if-then), this executes a statement while the condition evaluates to `true`. The loop is left when the condition evaluates to `false`. If the condition evaluates `false` for the first iteration, the inner code is skipped, because it is run zero times. The condition of a `while-do` statement must be of type `bool`.
```lua
while some_condition do
    <statement>

while some_condition do
{
    <statements>
}
```
The inner statement of a `while-do` expression may be any *non-empty* statement, be it a block containing multiple statements, or just a single statement.

#### Do-while
Same as [`while-do`](#while-do), but the inner code is executed before the condition is evaluated to determine whether the code should be run again. This means that the inner statement is executed at least once.
```lua
do <statement> while some_condition

do
{
    <statements>
}
while some_condition
```

It is okay to leave the inner statement empty in a `do-while` statement. However, if the condition being evaluted has no side effects, and that it evaluates to `true`, this will cause an [infinite loop](https://en.wikipedia.org/wiki/Infinite_loop).
```lua
do while some_condition_always_true
```
*note: considering adding a `forever` statement (or perhaps `loop`).*

*note: considering adding `until-do` and `do-until` statements.*

#### Variable definition
To declare a variable in your current scope, use the `var` keyword, followed by the name of the variable, then an equal sign followed by the variable's value (an expression). The type of the variable is the type of the expression you provide, which the compiler will infer.
```swift
var x = 0
var y = 3.1415
var z = "hi"
var u = ()
var b = case {
    x == 1 then (true, false)
    x == 0 then (false, true)
    otherwise todo
}
```
In the example above, we have
* `x` of type `i64`
* `y` of type `f64`
* `z` of type `[2]u8`
* `u` of type `()`
* `b` of type `(bool, bool)`

#### Function definition
Functions are pieces of reusable code that calculate some value or perform some logic given the arguments necessary in order to run the function. They are declared with the `func` keyword, followed by their name, arguments and return type. The definition is the code that is executed when the function is called, which is either a [block statement](#block-statement) or an [expression](#expressions).
```swift
func f(arg_1: t_1, arg_2: t_2, ..., arg_n: t_n) -> ret_ty
{
    <statements>
}

func f(arg_1: t_1, arg_2: t_2, ..., arg_n: t_n) -> ret_ty
    = <expression>
```

In both of these cases we have declared a function `f`, whose arguments are `arg_1`, `arg_2`, ..., `arg_n`, of type `t_1`, `t_2`, ..., `t_n` respectively, and whose return type is `ret_ty`.

In the first definition, we write in the block statement the code that is executed when the code is ran, and which will determine the returned value by the function. The values returned by this code must be of type `ret_ty`.

The second definition directly defines what the returned value is, simply by writing the expression of the result. This expression must also be of type `ret_ty`.

Functions can be considered as values that can be called, just like in any other [function call expression](#function-calls). You may refer to a function as a variable, and use it as a value for another variable. In this case, the type of the function is `t_1 t_2 ... t_n -> ret_ty`. Each function returns exactly one value (but it may be a [tuple](#tuples) or an [array](#arrays), etc...). However functions take in any number of arguments, even zero. The function which takes no arguments, and return type `ret_ty` has type `-> ret_ty` (no arguments before the arrow). *note: this syntax may change*

As such, functions may be used as returned values, which enables a programming (and mathematical) technique called [currying](https://en.wikipedia.org/wiki/Currying). The idea is that instead of making functions take more than one argument, we make a function that takes the first argument, and returns another function, which itself takes the second arguments, and returns a third function which takes in the third argument, ... and so on. This stops once we've applied the numbers of arguments we wanted to provide. Both styles are possible in Mer.
* With a function `f_normal` of type `α β γ -> δ`, we write `f(a, b, c)` and we get a value of type `δ`.
* With a function `f_curryied` of type `α -> β -> γ -> δ` (or `α -> (β -> (γ -> δ))` with extra parentheses):
  * `f_curryied(a)(b)(c)` is of type `δ`
  * `f_curryied(a)(b)` is of type `γ -> δ`
  * `f_curryied(a)` is of type `β -> γ -> δ`.
  * `f_curryied` is of type `α -> β -> γ -> δ`.

Functions which "don't return anything", also called sometimes "procedures", generally perform side effects, and don't calculate any resulting value. Functions like these usually return a unit (`()`), which only takes one possible value (the unit value), and which is discarded when the function call is complete. This is the equivalent of `void` functions in C, C#, etc... It works the same way as Rust functions which return unit (though Rust allows you to omit the unit return type annotation -- Mer does not).
```swift
func do_something(a: i64, b: i64) -> ()
{
    ...
    return
}

do_something(8, 4)
do_something(1, 9)
do_something(0, 0)
```

Note that it is incorrect to say that the function "returns nothing". It *does* return something, it's unit.

Functions which actually "return nothing" would be those that return type "never" (`!`). But it's not that they "return nothing", it's that **they don't return at all**. Because, as mentioned above, all functions must return something, a function that returns `!` *will* halt the program (see also [unreachable and todo](#unreachable-and-todo)). Indeed, no value is of type `!`, so actually "returning" a never-typed value is impossible.

#### Return statement
Inside of a function's body, you should use `return <expression>` statements to make the function return a value. The expression's type should be that of the function's return type. Every function must return, even those who return unit (`()`).

The syntax `return`, without an expression, is an alias for returning unit: `return ()`

```swift
func do_something() -> ()
{
    ...
    return
}

func make_something() -> i64
{
    var result = 0
    ...
    return result
}
```

The compiler will ensure that all paths in your code return a value. A compiler error is raised whenever code is not guaranteed to return. Code that follows a statement which is guaranteed to return is signaled by the compiler. For instance, the following function is not guaranteed to return...
```swift
func oops() -> i64
{
    if 0 == 1 then
        return 42
}
```
...while this one is all good, and has a bit of unreachable code that can be trimmed off (the last `return 0` statement is unreachable because both paths in the above `if-then-else` statement will return).
```swift
func okay() -> i64
{
    if 1 == 0 then
        return 42
    else
        return 24

    return 0
}
```

Note that the program itself acts like a function body, and must return unit. This way, `return` statements are valid anywhere, even in the top scope of the program (which is the "main" function is disguise).

#### Data structure definition
Data structure allow you to pack together values of various type into a single structure with a proper name. The general syntax uses the `data` keyword, and goes like so:
```swift
data StructName
{
    field_1: ty_1
    field_2: ty_2
    ...
    
    field_n: ty_n
}
```
... where `field_1`, `field_2`, ..., `field_n` are the names of the inner fields of your data structure, and which are all respectively of type `ty_1`, `ty_2`, ..., `ty_n`. `StructName` is the name of the data structure.

This statement does nothing other than define the data structure in the current scope. After this statement, you are able to [initialize this data structure](#data-structure-initialization), and [access fields](#data-structure-field-access) of values whose type is this exact structure.

This also creates a new type in the scope whose name is that of your data structure. This type is used to type initializations of this very data structure.

#### Print statement
As of right now, `print` is the only way to display strings, and this is a temporary solution. The `print` keyword must be followed by an expression of type `[&]u8`.
```php
print &"hello, world\n"
```
Note that references to [arrays of known size](#arrays) (`&[N]T`) coerce into [pointer types](#pointers) (`[&]T`), so the code above is still valid even though the expression `&"hello, world\n"` is of type `&[13]u8`: it is coerced into `[&]u8`.

### Miscellaneous
If you have some criticism/feedback, I'd love to hear it. Consider reaching out to me on discord at `@catapillie`.
