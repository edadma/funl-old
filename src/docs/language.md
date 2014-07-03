# The Language

The FunL interpreter can be used under different circumstances.  Usually, it will be used to run a program stored in a source file, which can in turn import modules which are also source files.  Or, it may be used from within an application to execute a code snippet.  It is also used by the REPL (Repeat Evaluate Print Loop) utility to execute individual lines of code.


## Source

If the interpreter is to run a program or import a module, the input is refered to as 'source' in the grammar:

	source = Newline | statements

	statements = statement+
	
A `source` is just a list of statements that are execute one after the other.  If a source file is empty, the lexer will still emit a `Newline` token.


## Statements

A statement in FunL is either an expression or a declaration:

	statement =
		|	expression Newline
			declaration

Expressions return a value and declarations don't.  The value returned by an expression statement may or may not be discarded depending on whether it occurs at the end of a block or not.

## Objects, Values, Data Types

A FunL program manipulates *objects*.  An object is something that lives in the computer's memory and carries some kind of *value*.  Two different objects can represent the same value.  So, a value is something that a human can understand.  Values are categorized into *data types* or just *types*.

There are two kinds of objects: mutable and immutable.  Generally, anything that can be created directly using the syntax of the language is immutable.  Built-in functions and type constructors are used to create mutable objects.  And there are also functions that create immutables too.

Immutable types in FunL are all created using a type of expression called a *literal*.

### Strings

A string literal is a list of characters enclosed in either single or double quotes, such as: `'this is a "string"'` or `"that's cool"`.  Strings used for representing text.  There is no separate character literal in FunL as in other languages, however FunL can deal with Java character objects.

### Numbers

FunL knows about several different types of numbers:

- **integers** can be any number of digits.  Internally FunL uses two different ways of dealing with integers depending on how big they are.  The conversion is handled automatically.
- **decimals** are IEEE 754 double precision (binary64) number objects.  FunL 1.0 will support arbitrary precision BCD numbers as well.
- **rationals** or fractions are what you get if you divide two integers where the first is not a multiple of the second.  So, strictly speaking there's no literal syntax for rationals.  FunL just gives the feeling that there is.  If you type `2/3`, you'll get a rational object with the value of two thirds. There's no limit to the size of the numerator or denominator of a rational number object.
- **complex numbers** are supported directly.  Current, complex numbers are a pair of double precision numbers, however FunL 1.0 will support arbitrary precision as well as complex (Gaussian) integers and complex fractions.  As for rationals, there's no literal syntax for complex numbers.  The variables `i` and `j`, which are equal and represent the imaginary unit, are predefined.  So, you can type `2 + 3i`, for example to the complex number equal to 2 plus 3 times the imaginary unit.  Note that an asterisk (`*`) is not need between the `3` and the `i`.  FunL understands that it's multiplication.

### Booleans

A boolean literal is just `true` or `false`.  Boolean values are the results of comparisons and logical connectives.

### Closures

A closure is an immutable object (though it may refer internally to variables that are mutable) that encapsulates a calculation and all variable references needed by that calculation.  A closure can be *called* or *invoked* using the so-called *application* syntax to get a value from it (which may be compound).

### Lists

A list literal is a list of expressions separated by commas and enclosed in square brackets.  For example, `[1, 2, 3]` creates a list with the numbers 1, 2 and 3 in it.  The empty list is `[]`.

## Expressions

A expression is a way of creating an object by way of applying operations and functions to other objects.  There are a number of categories of expressions.

### Applications

*Applications* refers to the well-known syntax for calling or invoking a function.  The term is not being used here to refer to software applications.  Generally, the application syntax is

    applyExpression ::=
        applyExpression ('(' (expression (',' expression)*)? ')')
      | applyExpression ('.' | '.>') ident
      | primaryExpression

For example to print the number `123` write

	println( 123 )

### Arithmetic

### Comparisons

### Logical

### Bitwise

### Conditional

A conditional expression takes on a value conditionally based on a boolean expression.  The syntax for conditionals is:

	'if' booleanExpression ('then' expressionOrBlock | blockExpression) elif* elsePart

where

    expressionOrBlock ::= expression | blockExpression

    elif ::= optionalNewline 'elif' booleanExpression ('then' expressionOrBlock | blockExpression)

    elsePart ::= (optionalNewline 'else' expressionOrBlock)?

### Case

### Conditional Loops

### Iterator Loop

An iterator loop, also known as a *for* loop, causes an express or block of expressions to be executed iteratively, one iteration for each element of an iterator object.  "for" loops have the syntax:

    'for' generators ('do' expressionOrBlock | blockExpression) elsePart

where

    generator ::=
        pattern '<-' expression ('if' expression)?

    generators ::= generator (',' generator)*

    expressionOrBlock ::= expression | blockExpression

    elsePart ::= (optionalNewline 'else' expressionOrBlock)?

For example

    for (key, value) <- {'one': 1, 'two': 2, 'three': 3}
      println( key + ' -> ' + value )

produces

    one -> 1
    two -> 2
    three -> 3

and

    for i <- 1..5 by 2
      println( i )

produces

    1
    3
    5

### Simple Loop

  The simple loop is for situation where you either want to terminate the loop using a `break` or `return`, or the application doesn't terminate.  For example
  
    n = 1
    
    for println( n++ )
  
  prints numbers starting from 1 without end.
  
### Sections

### Lambda Expressions

## Declarations

### Import

### Definition