# The Language

The FunL interpreter can be used under different circumstances.  Usually, it will be used to run a program stored in a source file, which can in turn import modules which are also source files.  Or, it may be used from within an application to execute a code snippet.  It is also used by the REPL (Repeat Evaluate Print Loop) utility to execute individual lines of code.


## Source

If the interpreter is to run a program or import a module, the input is refered to as 'source' in the grammar:

	source = Newline | statements

	statement = statement+
	
A `source` is just a list of statements that are execute one after the other.  If a source file is empty, the lexer will still emit a `Newline` token.


## Statements

A statement in FunL is either an expression or a declaration:

	statement =
		|	expr <~ Newline
			declaration

Expressions return a value and declarations don't.  The value returned by an expression statement may or may not be discarding depending on whether it occurs at the end of a block or not.

### Expressions

### Declarations