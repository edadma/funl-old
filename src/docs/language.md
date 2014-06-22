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

### Immutable Types

Immutable types in FunL are all created using a type of expression called a *literal*.

#### Strings

A string literal is a list of characters enclosed in either single or double quotes, such as: `'this is a "string"'` or `"that's cool"`.  Strings used for representing text.  There is no separate character literal in FunL as in other languages, however FunL can deal with Java character objects.

#### Numbers

FunL knows about several different types of numbers.

## Expressions

## Declarations