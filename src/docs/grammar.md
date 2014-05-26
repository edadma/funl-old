# Grammar

Here is the actual grammar (without parser actions and other source code boilerplate) used to parse FunL.  The grammar syntax is documented here: <http://www.scala-lang.org/api/2.11.1/scala-parser-combinators/#scala.util.parsing.combinator.Parsers>


## Syntactic Grammar

	snippet = statements
	
	source = rep(statement)

	declaration = imports | natives | constants | variables | data | definitions

	imports =
		"import" ~> importModule |
		"import" ~> Indent ~> rep1(importModule) <~ Dedent <~ Newline

	lazy val importModule = name <~ Newline

	natives =
		"native" ~> name |
		"native" ~> Indent ~> rep1(name) <~ Dedent <~ Newline |
		"function" ~> name |
		"function" ~> Indent ~> rep1(name) <~ Dedent <~ Newline
		
	dottedName = rep1sep(ident, ".")

	qualifier = ident ~ opt("=>" ~> ident)

	name =
		dottedName ~ opt("=>" ~> ident) <~ Newline |
		(dottedName <~ ".") ~ ("{" ~> rep1sep(className, ",") <~ "}" <~ Newline) |
		dottedName <~ "." <~ "*" <~ Newline

	idents = rep1sep( ident, "," )
	
	constants =
		"val" ~> constant |
		"val" ~> Indent ~> rep1(constant) <~ Dedent <~ Newline

	constant =
		(ident <~ "=") ~ expr <~ Newline

	variables =
		"var" ~> variable |
		"var" ~> Indent ~> rep1(variable) <~ Dedent <~ Newline

	variable =
		ident ~ opt("=" ~> expr) <~ Newline

	data =
		"data" ~> datatype |
		"data" ~> Indent ~> rep1(datatype) <~ Dedent <~ Newline

	datatype =
		(ident <~ "=") ~ rep1sep(constructor, "|") <~ Newline |
		constructor <~ Newline

	constructor =
		(ident <~ "(") ~ (idents <~ ")") |
		ident

	definitions =
		"def" ~> definition {case d => List( d )} |
		"def" ~> (Indent ~> rep1(definition) <~ (Dedent ~ Newline))

	definition =
		(ident <~ "(") ~ (repsep(pattern, ",") <~ ")") ~ (part | parts)

	part = opt("|" ~> expr10) ~ ("=" ~> expr) <~ Newline

	subpart =
		"|" ~> ("otherwise" | expr10) ~ ("=" ~> expr) <~ Newline

	parts =
		Indent ~> rep1(subpart) <~ Dedent <~ Newline

	statements =
		rep1(statement)

	onl = opt(Newline)

	expression = expr <~ Newline
	
	statement =
		expr <~ Newline |
		declaration

	expr: PackratParser[ExprAST] =
		rep1sep(leftExpr, ",") ~ ("=" | "+=" | "-=" | "*=" | "/=" | "^=") ~ rep1sep(nonassignmentExpr, ",") |
		nonassignmentExpr

 	mapping =
		("(" ~> rep1sep(pattern, ",") <~ ")" | repN(1, pattern)) ~ opt("|" ~> expr10) ~ ("->" ~> expr)

	caseFunctionExpr = Indent ~> rep1(mapping <~ Newline) <~ Dedent
	
	functionExpr = mapping | caseFunctionExpr

	nonassignmentExpr = expr5
	
	expr5 =
		functionExpr |
		expr7

	elif =
		(onl ~ "elif") ~> (booleanExpr <~ "then") ~ expr

	generator = (pattern <~ "<-") ~ expr ~ opt("if" ~> expr)

	generators = rep1sep(generator, ",")
	
	expr7 =
		("if" ~> booleanExpr) ~ ("then" ~> expr | block) ~ rep(elif) ~ opt(onl ~> "else" ~> expr) |
		"for" ~> generators ~ ("do" ~> expr | block) ~ opt(onl ~> "else" ~> expr) |
		"while" ~> expr ~ ("do" ~> expr | block) ~ opt(onl ~> "else" ~> expr) |
		"do" ~> expr ~ (onl ~> "while" ~> expr) ~ opt(onl ~> "else" ~> expr) |
		"repeat" ~> expr ~ (onl ~> "until" ~> expr) ~ opt(onl ~> "else" ~> expr) |
		"break" |
		"continue" |
		("case" ~> expr) ~ ("of" ~> functionExpr | caseFunctionExpr) |
		expr10

	booleanExpr = expr10

	expr10 =
		expr11 ~ rep(("or" | "xor") ~ expr11)

	expr11 =
		expr12 ~ rep("and" ~ expr12)

	expr12 =
		"not" ~> expr12 (NotExprAST( _ )) |
		expr22

	expr22 =
		expr26 ~ ("==" | "/=" | "<" | ">" | "<=" | ">=" | "in" | "not" ~ "in" | "|" | "/|") ~ expr26 |
		expr26

	expr26 =
		expr27 ~ (":" ~> expr26) |
		expr27

	jsonExpr = expr27

	expr27 =
		expr30 ~ (".." | "until") ~ expr30 ~ opt("by" ~> expr30) |
		expr30

	expr30 =
		expr30 ~ ("+" | "-") ~ expr31 |
		expr31

	expr31 =
		expr31 ~ ("*" | "/" | """\""" | "%") ~ expr32 |
		expr31 ~ leftExpr |
		expr32

	expr32 =
		expr32 ~ "^" ~ expr33 |
		expr33

	expr33 =
		"-" ~> expr34 |
		expr34

	expr34 =
		("++" | "--") ~ expr35 |
		expr35 ~ ("++" | "--") |
		expr35

	leftExpr = expr35

	expr35 =
		expr35 ~ ("(" ~> repsep(expr, ",") <~ ")") |
		expr35 ~ ("." ~> ident) |
		expr40

	entry =
		jsonExpr ~ (":" ~> expr)

	expr40 =
		numericLit |
		stringLit |
		"(" ~> expr <~ ")" |
		ident |
		("true" | "false") |
		"(" ~ ")" |
		("(" ~> expr <~ ",") ~ (rep1sep(expr, ",") <~ ")") |
		("[" ~> repsep(expr, ",") <~ "]") |
		"null" |
		"{" ~> repsep(jsonExpr, ",") <~ "}" |
		"{" ~> repsep(entry, ",") <~ "}" |
		Indent ~> statements <~ Dedent |
		"$" ~> ident |
		"?" ~> ident

	pattern =
		(ident <~ "@") ~ pattern5 |
		pattern5

	pattern5: PackratParser[PatternAST] =
		pattern10 ~ (":" ~> pattern5) |
		pattern10

	pattern10: PackratParser[PatternAST] =
		numericLit |
		stringLit |
		("true" | "false") |
		"(" ~ ")" |
		"null" |
		ident ~ ("(" ~> repsep(pattern, ",") <~ ")") |
		ident ~ opt("::" ~> ident) |
		("(" ~> pattern <~ ",") ~ (rep1sep(pattern, ",") <~ ")") |
		("[" ~> repsep(pattern, ",") <~ "]") |
		("(" ~> pattern <~ "|") ~ (rep1sep(pattern, "|") <~ ")") |
		"(" ~> pattern <~ ")"


## Lexical Grammar

The reserved words in the language are:
				`and`, `break`, `by`, `case`, `class`, `continue`, `data`, `def`, `do`, `elif`,
				`else`, `false`, `for`, `function`, `if`, `import`, `in`, `is`, `mod`, `native`,
				`not`, `null`, `of`, `or`, `otherwise`, `repeat`, `return`, `then`, `true`, `until`,
				`val`, `var`, `while`, `xor`, `yield`.

The special delimiters are: `+`, `*`, `-`, `/`, `^`, `(`, `)`, `[`, `]`, `|`, `{`, `}`, `,`, `=`, `==`, `/=`, `<`, `$`,
`>`, `<-`, `<=`, `>=`, `--`, `++`, `.`, `..`, `<-`, `->`, `=>`, `+=`, `-=`, `*=`, `^=`, `:`, `\\`, `::`, `@`, `?`.

	decimalParser =
		rep1(digit) ~ optFraction ~ optExponent |
		fraction ~ optExponent

	sign = '+' | '-'

	optSign = opt( sign )

	fraction = '.' ~ rep1(digit)

	optFraction = opt( fraction )

	exponent = ('e' | 'E') ~ optSign ~ rep1(digit)

	optExponent = opt(exponent)