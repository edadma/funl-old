/*     ______            __                                      *\
**    / ____/_  __ ___  / /     FunL Programming Language        **
**   / __/ / / / / __ \/ /      (c) 2014, Edward A. Maxedon, Sr. **
**  / /   / /_/ / / / / /__     http://funl-lang.org/            **
** /_/    \____/_/ /_/____/                                      **
\*                                                               */

package funl.interp

import util.parsing.combinator.PackratParsers
import util.parsing.combinator.syntactical.StandardTokenParsers
import util.parsing.input.Reader

import funl.indentation._


class Parser( module: String ) extends StandardTokenParsers with PackratParsers
{
	override val lexical: IndentationLexical =
		new IndentationLexical( false, true, List("[", "(", "{"), List("]", ")", "}") )
		{
			override def token: Parser[Token] = hexParser | decimalParser | super.token

			private def decimalParser: Parser[Token] =
				rep1(digit) ~ optFraction ~ optExponent ^^
					{case intPart ~ frac ~ exp =>
						NumericLit( (intPart mkString "") :: frac :: exp :: Nil mkString "")} |
				fraction ~ optExponent ^^
					{case frac ~ exp => NumericLit( frac + exp )}

			private def chr( c: Char ): Parser[Char] = elem( "", ch => ch == c )

			private def sign = chr( '+' ) | chr( '-' )

			private def optSign = opt( sign ) ^^
				{
					case None => ""
					case Some(sign) => sign
				}

			private def fraction: Parser[String] =
				'.' ~ rep1(digit) ^^
				{case dot ~ ff => dot :: (ff mkString "") :: Nil mkString ""}

			private def optFraction: Parser[String] =
				opt( fraction ) ^^
				{
					case None => ""
					case Some( fraction ) => fraction
				}

			private def exponent = (chr('e') | chr('E')) ~ optSign ~ rep1(digit) ^^
				{case e ~ optSign ~ exp => e :: optSign :: (exp mkString "") :: Nil mkString ""}

			private def optExponent: Parser[String] =
				opt(exponent) ^^
				{
					case None => ""
					case Some( exponent ) => exponent
				}

			private def hexParser: Parser[Token] =
				'0' ~> 'x' ~> rep1(digit | chr('a') | chr('b') | chr('c') | chr('d') | chr('e') | chr('f') | chr('A') | chr('B') | chr('C') | chr('D') | chr('E') | chr('F')) ^^
					(d => NumericLit( "0x" + (d mkString "") ))
				
			reserved += (
				"and", "break", "by", "case", "class", "continue", "data", "def", "do", "elif",
				"else", "false", "for", "forever", "function", "if", "import", "in", "is", "mod",
				"native", "not", "null", "of", "or", "otherwise", "private", "return", "then", "true",
				"until", "val", "var", "while", "xor", "yield", "rightshift", "leftshift", "rightrotate", "leftrotate"
				)
			delimiters += ("+", "*", "-", "/", "%", "^", "(", ")", "[", "]", "|", "/|", "{", "}", ",",
				"=", "==", "!=", "<", "$", "?", ">", "<-", "<=", ">=", "--", "++", ".", "..", "<-", "->",
				"=>", "+=", "-=", "*=", "/=", "^=", ":", "#", "\\", "::", "@")
		}

	import lexical.{Newline, Indent, Dedent}

	def parseSource( r: Reader[Char] ) = phrase( source )( lexical.read(r) )

	def parseExpression( r: Reader[Char] ) = phrase( expression )( lexical.read(r) )

	def parseStatement( r: Reader[Char] ) = phrase( statement )( lexical.read(r) )

	lazy val source: PackratParser[ModuleAST] =
		Newline ^^^ (ModuleAST( module, Nil )) |
		statements ^^ {case l => ModuleAST( module, l )}

	lazy val declaration: PackratParser[DeclarationBlockAST] = imports | natives | constants | variables | data | definitions

	lazy val imports =
		"import" ~> importModule ^^ {case m => DeclarationBlockAST( List(m) )} |
		"import" ~> Indent ~> rep1(importModule) <~ Dedent <~ Newline ^^ (DeclarationBlockAST( _ ))

	lazy val importModule =
		name ^^ {case (qual, names) => ImportAST( qual, names )}

	lazy val natives =
		"native" ~> name ^^ {case (pkg, names) => DeclarationBlockAST( List(NativeAST(pkg, names)) )} |
		"native" ~> Indent ~> rep1(name) <~ Dedent <~ Newline ^^ (cs => DeclarationBlockAST( cs map {case (pkg, names) => NativeAST( pkg, names )})) |
		"function" ~> name ^^ {case (cls, names) => DeclarationBlockAST(List( FunctionAST(cls, names) ))} |
		"function" ~> Indent ~> rep1(name) <~ Dedent <~ Newline ^^ (cs => DeclarationBlockAST(cs map {case (cls, names) => FunctionAST( cls, names )}))

	lazy val dottedName = rep1sep(ident, ".")

	lazy val qualifier = ident ~ opt("=>" ~> ident) ^^ {case name ~ alias => (name, alias)}

	lazy val name: PackratParser[(String, List[(String, Option[String])])] =
		dottedName ~ opt("=>" ~> ident) <~ Newline ^^
			{case name ~ alias => (name.init.mkString( "." ), List((name.last, alias)))} |
		(dottedName <~ ".") ~ ("{" ~> rep1sep(qualifier, ",") <~ "}" <~ Newline) ^^
			{case pkg ~ names => (pkg.mkString( "." ), names)} |
		dottedName <~ "." <~ "*" <~ Newline ^^
			{case name => (name.mkString( "." ), null)}

	lazy val idents = rep1sep( ident, "," )

	lazy val constants =
		"val" ~> constant ^^ {case c => DeclarationBlockAST( List(c) )} |
		"val" ~> Indent ~> rep1(constant) <~ Dedent <~ Newline ^^ (DeclarationBlockAST( _ ))

	lazy val constant =
		(pattern <~ "=") ~ nonassignmentExpr <~ Newline ^^
			{case pat ~ exp => ValAST( pat, exp )}

	lazy val variables =
		"var" ~> variable ^^ {case v => DeclarationBlockAST( List(v) )} |
		"var" ~> Indent ~> rep1(variable) <~ Dedent <~ Newline ^^ (DeclarationBlockAST( _ ))

	lazy val variable =
		ident ~ opt("=" ~> expr) <~ Newline ^^
			{case n ~ v => VarAST( n, v )}

	lazy val data =
		"data" ~> datatype ^^ {case v => DeclarationBlockAST( List(v) )} |
		"data" ~> Indent ~> rep1(datatype) <~ Dedent <~ Newline ^^ (DeclarationBlockAST( _ ))

	lazy val datatype =
		(ident <~ "=") ~ rep1sep(constructor, "|") <~ Newline ^^
			{case typename ~ constructors => DataAST( typename, constructors )} |
		constructor <~ Newline ^^
			{case c => DataAST( c._1, List(c) )}

	lazy val constructor =
		(ident <~ "(") ~ (idents <~ ")") ^^
			{case name ~ fields => (name, fields)} |
		ident ^^
			{case name => (name, Nil)}

	lazy val definitions =
		"def" ~> definition ^^ {case d => DeclarationBlockAST(List( d ))} |
		"def" ~> (Indent ~> rep1(definition) <~ (Dedent ~ Newline)) ^^ (DeclarationBlockAST( _ ))

	lazy val definition =
		ident ~ opt("(" ~> repsep(pattern, ",") <~ ")") ~ (part | parts) ^^
			{	case n ~ None ~ gs => DefAST( n, FunctionExprAST(Nil, gs) )
				case n ~ Some(p) ~ gs => DefAST( n, FunctionExprAST(p, gs) )}

	lazy val part = opt("|" ~> expr10) ~ ("=" ~> expr) <~ Newline ^^
		{case g ~ b => List(FunctionPartExprAST(g, b))}

	lazy val subpart =
		"|" ~> ("otherwise" ^^^ None | expr10 ^^ (e => Some(e))) ~ ("=" ~> expr) <~ Newline ^^
			{case g ~ b => FunctionPartExprAST(g, b)}

	lazy val parts =
		Indent ~> rep1(subpart) <~ Dedent <~ Newline

	lazy val statements =
		rep1(statement)

	lazy val onl = opt(Newline)

	lazy val expression: PackratParser[ExprAST] = expr <~ Newline
	
	lazy val statement: PackratParser[StatementAST] =
		expr <~ Newline ^^ (ExpressionStatementAST( _ )) |
		declaration

	lazy val expr: PackratParser[ExprAST] =
		rep1sep(leftExpr, ",") ~ ("=" | "+=" | "-=" | "*=" | "/=" | "^=") ~ rep1sep(nonassignmentExpr, ",") ^^
			{case lhs ~ op ~ rhs => AssignmentExprAST( lhs, Symbol(op), rhs )} |
		nonassignmentExpr

 	lazy val mapping =
 	("(" ~> rep1sep(pattern, ",") <~ ")" | repN(1, pattern)) ~ opt("|" ~> expr10) ~ ("->" ~> expr) ^^
		{case p ~ g ~ b => FunctionExprAST( p, List(FunctionPartExprAST(g, b)) )}

	lazy val caseFunctionExpr =
		Indent ~> rep1(mapping <~ Newline) <~ Dedent ^^
			{case c => CaseFunctionExprAST( c )}
			
	lazy val functionExpr =
		mapping | caseFunctionExpr

	lazy val nonassignmentExpr = expr5
	
	lazy val expr5 =
		functionExpr |
		expr7

	lazy val elif =
		(onl ~ "elif") ~> booleanExpr ~ ("then" ~> expr | block) ^^ {case c ~ t => (c, t)}

	lazy val generator =
		(pattern <~ "<-") ~ expr ~ opt("if" ~> expr) ^^ {case p ~ t ~ f => GeneratorAST( p, t, f )}

	lazy val generators = rep1sep(generator, ",")
	
	lazy val expr7 =
		("if" ~> booleanExpr) ~ ("then" ~> expr | block) ~ rep(elif) ~ opt(onl ~> "else" ~> expr) ^^
			{case c ~ t ~ ei ~ e => ConditionalExprAST( (c, t) +: ei, e )} |
		"forever" ~> expr ^^
			(ForeverExprAST( _ )) |
		"for" ~> generators ~ ("do" ~> expr | block) ~ opt(onl ~> "else" ~> expr) ^^
			{case g ~ b ~ e => ForExprAST( g, b, e )} |
		"while" ~> expr ~ ("do" ~> expr | block) ~ opt(onl ~> "else" ~> expr) ^^
			{case c ~ b ~ e => WhileExprAST( c, b, e )} |
		"do" ~> expr ~ (onl ~> "while" ~> expr) ~ opt(onl ~> "else" ~> expr) ^^
			{case b ~ c ~ e => DoWhileExprAST( b, c, e )} |
		"do" ~> expr ~ (onl ~> "until" ~> expr) ~ opt(onl ~> "else" ~> expr) ^^
			{case b ~ c ~ e => RepeatExprAST( b, c, e )} |
		"break" ^^^ BreakExprAST |
		"continue" ^^^ ContinueExprAST |
		("case" ~> expr) ~ ("of" ~> functionExpr | caseFunctionExpr) ^^
			{case e ~ f => ApplyExprAST( f, List(e), false )} |
		expr10

	lazy val booleanExpr = expr10
	
	lazy val expr10 =
		expr11 ~ rep(("or" | "xor") ~ expr11) ^^
			{case lhs ~ list => (lhs /: list){case (x, op ~ y) => BinaryExprAST( x, Symbol(op), y )}}

	lazy val expr11 =
		expr12 ~ rep(("and" | "rightrotate" | "leftrotate" | "rightshift" | "leftshift") ~ expr12) ^^
			{case lhs ~ list => (lhs /: list){case (x, op ~ y) => BinaryExprAST( x, Symbol(op), y )}}

	lazy val expr12: PackratParser[ExprAST] =
		"not" ~> expr12 ^^ (NotExprAST( _ )) |
		expr22

	lazy val expr22: PackratParser[ExprAST] =
		expr24 ~ ("==" | "!=" | "<" | ">" | "<=" | ">=" | "in" | "not" ~ "in" ^^^ "notin" | "|" | "/|") ~ expr24 ^^
			{case l ~ o ~ r => BinaryExprAST( l, Symbol(o), r )} |
		expr24 ~ "is" ~ ident ^^ {case e ~ _ ~ t => TypeExprAST( e, t )} |
		expr24

	lazy val expr24: PackratParser[ExprAST] =
		(comprehensionExpr <~ "|") ~ generators ^^
			{case e ~ gs => IteratorExprAST( e, gs )} |
		expr26

	lazy val comprehensionExpr = expr26
	
	lazy val expr26: PackratParser[ExprAST] =
		expr27 ~ (":" ~> expr26) ^^ {case h ~ t => ConsExprAST( h, t )} |
		expr27 ~ ("#" ~> expr26) ^^ {case h ~ t => StreamExprAST( h, t )} |
		expr27

	lazy val jsonExpr = expr27
	
	lazy val expr27: PackratParser[ExprAST] =
		expr30 ~ (".." | "until") ~ expr30 ~ opt("by" ~> expr30) ^^
			{case f ~ op ~ t ~ b => RangeExprAST( f, t, b, if (op == "..") true else false )} |
		expr30

	lazy val expr30: PackratParser[ExprAST] =
		expr30 ~ ("+" | "-") ~ expr31 ^^
			{case l ~ o ~ r => BinaryExprAST( l, Symbol(o), r )} |
		expr31

	lazy val expr31: PackratParser[ExprAST] =
		expr31 ~ ("*" | "/" | """\""" | "%") ~ expr32 ^^
			{case l ~ o ~ r => BinaryExprAST( l, Symbol(o), r )} |
		expr31 ~ leftExpr ^^
			{case l ~ r => BinaryExprAST( l, '*, r )} |
		expr32

	lazy val expr32: PackratParser[ExprAST] =
		expr32 ~ "^" ~ expr33 ^^
			{case l ~ o ~ r => BinaryExprAST( l, Symbol(o), r )} |
		expr33

	lazy val expr33: PackratParser[ExprAST] =
		"-" ~> expr34 ^^
			(UnaryExprAST( '-, _ )) |
		expr34

	lazy val expr34: PackratParser[ExprAST] =
		("++" | "--") ~ block ^^
			{case o ~ e => UnaryExprAST( Symbol("pre" + o), e )} |
		block ~ ("++" | "--") ^^
			{case e ~ o => UnaryExprAST( Symbol("post" + o), e )} |
		block

	lazy val block =
		Indent ~> statements <~ Dedent ^^
			(BlockExprAST( _ )) |
		expr35

	lazy val leftExpr = expr35
	
	lazy val expr35: PackratParser[ExprAST] =
		expr35 ~ ("(" ~> repsep(expr, ",") <~ ")") ^^
			{case f ~ args => ApplyExprAST( f, args, false )} |
		expr35 ~ ("." ~> ident) ^^ {case e ~ f => DotExprAST( e, f )} |
		expr40

	lazy val entry =
		jsonExpr ~ (":" ~> expr) ^^ {case k ~ v => TupleExprAST( k, v )}

	lazy val iteratorExpr =
		comprehensionExpr ~ ("|" ~> generators) ^^
			{case e ~ g => IteratorExprAST( e, g )}
	
	lazy val expr40: PackratParser[ExprAST] =
		numericLit ^^
			(n =>
				if (n matches ".*(\\.|e|E).*")
					LiteralExprAST( n.toDouble )
				else if (n startsWith "0x")
					if (n.length <= 10)		// 8 digits (32 bits) + 2 for "0x"
						LiteralExprAST( Integer.parseInt(n substring 2, 16) )
					else
						LiteralExprAST( BigInt(n substring 2, 16) )
				else
				{
				val bi = BigInt( n )

					if (bi.isValidInt)
						LiteralExprAST( bi.intValue )
					else
						LiteralExprAST( bi )
				}) |
		stringLit ^^
			(StringLiteralExprAST( _ )) |
    "(" ~> infix <~ ")" ^^
      (o => SectionExprAST( Symbol(o) )) |
    "(" ~> expr ~ infix <~ ")" ^^
      {case e ~ o => LeftSectionExprAST( e, Symbol(o) )} |
    "(" ~> infix ~ expr <~ ")" ^^
      {case o ~ e => RightSectionExprAST( Symbol(o), e )} |
		"(" ~> expr <~ ")" |
		ident ^^
			{case v => VariableExprAST( v )} |
		("true" | "false") ^^
			(b => LiteralExprAST( b.toBoolean )) |
		"(" ~ ")" ^^^
			UnitExprAST |
		("(" ~> nonassignmentExpr <~ ",") ~ (rep1sep(nonassignmentExpr, ",") <~ ")") ^^
			{case e ~ l => VectorExprAST( e +: l )} |
		"[" ~> iteratorExpr <~ "]" ^^
			(ListComprehensionExprAST( _ )) |
		"[" ~> repsep(nonassignmentExpr, ",") <~ "]" ^^
			{case l => ListExprAST( l )} |
		"null" ^^
			(_ => NullExprAST) |
		"{" ~> repsep(jsonExpr, ",") <~ "}" ^^
			(SetExprAST( _ )) |
		"{" ~> rep1sep(entry, ",") <~ "}" ^^
			(MapExprAST( _ )) |
		"$" ~> ident ^^
			(SysvarExprAST( _ )) |
		"?" ~> ident ^^
			(TestExprAST( _ ))

	lazy val infix = "+" | "-" | "*" | "/" | """\""" | "^" | "%" | "==" | "!=" | "<" | ">" | "<=" | ">=" | ":" | "#" | "and" | "or" | "xor"
	
	lazy val pattern: PackratParser[PatternAST] =
		(ident <~ "@") ~ pattern3 ^^
			{case alias ~ pat => AliasPatternAST( alias, pat )} |
		pattern3

	lazy val pattern3: PackratParser[PatternAST] =
		pattern5 ~ ("::" ~> ident) ^^
			{case pat ~ typename => TypePatternAST( pat, typename )} |
		pattern5
		
	lazy val pattern5: PackratParser[PatternAST] =
		pattern10 ~ (":" ~> pattern5) ^^ {case h ~ t => ConsPatternAST( h, t )} |
		pattern10

	lazy val pattern10: PackratParser[PatternAST] =
		numericLit ^^
			(n =>
				if (n matches ".*(\\.|e|E).*")
					LiteralPatternAST( n.toDouble )
				else if (n startsWith "0x")
					if (n.length <= 10)		// 8 digits (32 bits) + 2 for "0x"
						LiteralPatternAST( Integer.parseInt(n substring 2, 16) )
					else
						LiteralPatternAST( BigInt(n substring 2, 16) )
				else
				{
				val bi = BigInt( n )

					if (bi.isValidInt)
						LiteralPatternAST( bi.intValue )
					else
						LiteralPatternAST( bi )
				}) |
		stringLit ^^
			(LiteralPatternAST( _ )) |
		("true" | "false") ^^
			(b => LiteralPatternAST( b.toBoolean )) |
		"(" ~ ")" ^^^
			UnitPatternAST |
		"null" ^^^
			NullPatternAST |
		ident ~ ("(" ~> repsep(pattern, ",") <~ ")") ^^
			{case n ~ l => RecordPatternAST( n, l )} |
		ident ^^
			{case v => VariablePatternAST( v )} |
		("(" ~> pattern <~ ",") ~ (rep1sep(pattern, ",") <~ ")") ^^
			{case e ~ l => TuplePatternAST( e +: l )} |
		"[" ~> repsep(pattern, ",") <~ "]" ^^
			{case l => ListPatternAST( l )} |
		"{" ~ "}" ^^^
			EmptySetPatternAST |
		("(" ~> pattern <~ "|") ~ (rep1sep(pattern, "|") <~ ")") ^^
			{case e ~ l => AltPatternAST( e +: l )} |
		"(" ~> pattern <~ ")"
}