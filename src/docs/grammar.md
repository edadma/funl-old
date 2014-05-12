# Grammar

Here is the actual grammar (without parser actions and other source code boilerplate) used to parse FunL.  The grammar syntax is documented here: <http://www.scala-lang.org/api/2.10.4/index.html#scala.util.parsing.combinator.Parsers$Parser>


## Syntactic Grammar

	snippet = statements
	
	source = rep(component)

	component = imports | symbolImports | natives | constants | variables | data | definitions | main

	imports =
		"import" ~> importModule |
		"import" ~> Indent ~> rep1(importModule) <~ Dedent <~ Newline

	symbolImports =
		("from" ~> symbol <~ "import") ~ symbols <~ Newline |
		("from" ~> symbol <~ "import") <~ "*" <~ Newline

	lazy val importModule =
		symbol <~ Newline ^^ (ImportModuleAST( module, _ ))

	natives =
		"class" ~> native |
		"class" ~> Indent ~> rep1(native) <~ Dedent <~ Newline |
		"method" ~> native |
		"method" ~> Indent ~> rep1(native) <~ Dedent <~ Newline |
		"field" ~> native |
		"field" ~> Indent ~> rep1(native) <~ Dedent <~ Newline |
		"function" ~> native |
		"function" ~> Indent ~> rep1(native) <~ Dedent <~ Newline
		
	dottedName = rep1sep(ident, ".")

	className = ident ~ opt("=>" ~> symbol)

	native =
		dottedName ~ opt("=>" ~> symbol) <~ Newline
		(dottedName <~ ".") ~ ("{" ~> rep1sep(className, ",") <~ "}" <~ Newline)

	symbol = ident

	symbols = rep1sep( symbol, "," )
	
	constants =
		"val" ~> constant |
		"val" ~> Indent ~> rep1(constant) <~ Dedent <~ Newline

	constant =
		(symbol <~ "=") ~ expr <~ Newline

	variables =
		"var" ~> variable |
		"var" ~> Indent ~> rep1(variable) <~ Dedent <~ Newline

	variable =
		symbol ~ opt("=" ~> expr) <~ Newline

	data =
		"data" ~> datatype |
		"data" ~> Indent ~> rep1(datatype) <~ Dedent <~ Newline

	datatype =
		(symbol <~ "=") ~ rep1sep(constructor, "|") <~ Newline |
		constructor <~ Newline

	constructor =
		(symbol <~ "(") ~ (rep1sep(symbol, ",") <~ ")") |
		symbol

	definitions =
		"def" ~> definition {case d => List( d )} |
		"def" ~> (Indent ~> rep1(definition) <~ (Dedent ~ Newline))

	definition =
		(symbol <~ "(") ~ (repsep(pattern, ",") <~ ")") ~ (part | parts)

	locals = opt("local" ~> rep1sep(symbol, ","))

	part = opt("|" ~> expr10) ~ locals ~ ("=" ~> expr) <~ Newline

	subpart =
		"|" ~> ("otherwise"^ None | expr10 (e => Some(e))) ~ locals ~ ("=" ~> expr) <~ Newline

	parts =
		Indent ~> rep1(subpart) <~ Dedent <~ Newline

	main =
		"main" ~> statement (s => List(MainAST( s )))

	statements =
		rep1(statement)

	onl = opt(Newline)

	statement =
		statementExpr <~ Newline

	statementExpr: PackratParser[StatementAST] =
		("val" ~> pattern <~ "=") ~ expr5 |
		expr (ExpressionStatementAST( _ ))

	expr: PackratParser[ExprAST] =
		rep1sep(leftExpr, ",") ~ ("=" | "+=" | "-=" | "*=" | "/=" | "^=") ~ rep1sep(expr5, ",") |
		expr5

 	functionCase =
		("(" ~> rep1sep(pattern, ",") <~ ")" | repN(1, pattern)) ~ opt("|" ~> expr10) ~ ("->" ~> expr)

	functionExpr =
		functionCase |
		Indent ~> rep1(functionCase <~ Newline) <~ Dedent

	expr5 =
		functionExpr |
		expr7

	elif =
		(onl ~ "elsif") ~> (booleanExpr <~ "then") ~ expr

	expr7 =
		"if" ~> (booleanExpr <~ "then") ~ expr ~ rep(elif) ~ opt(onl ~> "else" ~> expr) |
		("for" ~> pattern <~ "<-") ~ (expr <~ "do") ~ expr ~ opt(onl ~> "else" ~> expr) |
		("while" ~> expr) ~ ("do" ~> expr) ~ opt(onl ~> "else" ~> expr) |
		("do" ~> expr) ~ (onl ~> "while" ~> expr) ~ opt(onl ~> "else" ~> expr) |
		("repeat" ~> expr) ~ (onl ~> "until" ~> expr) ~ opt(onl ~> "else" ~> expr) |
		"break" |
		"continue" |
		("case" ~> expr) ~ ("of" ~> functionExpr) |
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
		expr26 ~ ("==" | "/=" | "<" | ">" | "<=" | ">=" | "in" | "not" ~ "in") ~ expr26 |
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
		expr31 ~ ("*" | "/" | "\\") ~ expr32 |
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
		expr35 ~ ("." ~> symbol) |
		expr40

	entry =
		jsonExpr ~ (":" ~> expr)

	expr40 =
		numericLit |
		stringLit |
		"(" ~> expr <~ ")" |
		symbol |
		("true" | "false") |
		"(" ~ ")" |
		("(" ~> expr <~ ",") ~ (rep1sep(expr, ",") <~ ")") |
		("[" ~> repsep(expr, ",") <~ "]") |
		"null" |
		"{" ~> repsep(jsonExpr, ",") <~ "}" |
		"{" ~> repsep(entry, ",") <~ "}" |
		Indent ~> statements <~ Dedent |
		"$" ~> symbol

	pattern =
		(symbol <~ "@") ~ pattern5 |
		pattern5

	pattern5: PackratParser[PatternAST] =
		pattern10 ~ (":" ~> pattern5) |
		pattern10

	pattern10: PackratParser[PatternAST] =
		pattern20 |
		ident ~ ("(" ~> repsep(pattern, ",") <~ ")") |
		symbol ~ opt("::" ~> symbol) |
		("(" ~> pattern <~ ",") ~ (rep1sep(pattern, ",") <~ ")") |
		("[" ~> repsep(pattern, ",") <~ "]") |
		("(" ~> pattern30 <~ "|") ~ (rep1sep(pattern30, "|") <~ ")")

	pattern20 =
		numericLit |
		stringLit |
		("true" | "false") |
		"(" ~ ")" |
		"null"

	pattern30 =
		pattern40 ~ (":" ~> pattern30) |
		pattern40

	pattern40 =
		pattern20 |
		("(" ~> pattern30 <~ ",") ~ (rep1sep(pattern30, ",") <~ ")") |
		"[" ~> repsep(pattern30, ",") <~ "]"


## Lexical Grammar

The reserved words in the language are: `do`, `if`, `then`, `for`, `else`, `elsif`, `by`, `while`, `var`, `from`, `import`, `break`, `continue`, `repeat`,
`until`, `of`,
`export`, `class`, `main`, `data`, `def`, `true`, `false`, `val`, `null`, `not`, `and`, `or`, `xor`, `otherwise`, `in`, `case`, `method`, `field`, `function`.

The special delimiters are: `+`, `*`, `-`, `/`, `^`, `(`, `)`, `[`, `]`, `|`, `{`, `}`, `,`, `=`, `==`, `/=`, `<`, `$`,
`>`, `<-`, `<=`, `>=`, `--`, `++`, `.`, `..`, `<-`, `->`, `=>`, `+=`, `-=`, `*=`, `^=`, `:`, `\\`, `::`, `@`.

	decimalParser =
		rep1(digit) ~ optFraction ~ optExponent |
		fraction ~ optExponent

	sign = '+' | '-'

	optSign = opt( sign )

	fraction = '.' ~ rep1(digit)

	optFraction = opt( fraction )

	exponent = ('e' | 'E') ~ optSign ~ rep1(digit)

	optExponent = opt(exponent)