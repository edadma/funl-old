/*     ______            __                                     *\
**    / ____/_  __ ___  / /     FunL Programming Language       **
**   / __/ / / / / __ \/ /      (c) 2014 Edward A. Maxedon, Sr. **
**  / /   / /_/ / / / / /__     http://funl-lang.org/           **
** /_/    \____/_/ /_/____/                                     **
\*                                                              */

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
				"else", "false", "for", "function", "if", "import", "in", "is", "mod",
				"native", "not", "null", "of", "or", "otherwise", "private", "return", "then", "true",
				"until", "val", "var", "while", "xor", "yield", "shiftright", "shiftleft", "rotateright", "rotateleft"
				)
			delimiters += ("+", "*", "-", "/", "%", "^", "(", ")", "[", "]", "|", "/|", "{", "}", ",",
				"=", "==", "!=", "<", "$", "?", ">", "<-", "<=", ">=", "--", "++", ".", ".>", "..", "<-", "->",
				"=>", "+=", "-=", "*=", "/=", "\\=", "^=", ":", "#", "\\", "::", "@")
		}

	import lexical.{Newline, Indent, Dedent}

	def parseSource( r: Reader[Char] ) = phrase( source )( lexical.read(r) )

	def parseExpression( r: Reader[Char] ) = phrase( expressionStatement )( lexical.read(r) )

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

	lazy val identifiers = rep1sep( ident, "," )

	lazy val constants =
		"val" ~> constant ^^ {case c => DeclarationBlockAST( List(c) )} |
		"val" ~> Indent ~> rep1(constant) <~ Dedent <~ Newline ^^ (DeclarationBlockAST( _ ))

	lazy val constant =
		(pattern <~ "=") ~ expressionOrBlock <~ Newline ^^
			{case pat ~ exp => ValAST( pat, exp )}

	lazy val variables =
		"var" ~> variable ^^ {case v => DeclarationBlockAST( List(v) )} |
		"var" ~> Indent ~> rep1(variable) <~ Dedent <~ Newline ^^ (DeclarationBlockAST( _ ))

	lazy val variable =
		ident ~ opt("=" ~> expressionOrBlock) <~ Newline ^^
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
		(ident <~ "(") ~ (identifiers <~ ")") ^^
			{case name ~ fields => (name, fields)} |
		ident ^^
			{case name => (name, Nil)}

	lazy val definitions =
		"def" ~> definition ^^ {case d => DeclarationBlockAST(List( d ))} |
		"def" ~> (Indent ~> rep1(definition) <~ (Dedent ~ Newline)) ^^ (DeclarationBlockAST( _ ))

	lazy val definition =
		ident ~ opt("(" ~> repsep(pattern, ",") <~ ")") ~ (optionallyGuardedPart | guardedParts) ^^
			{	case n ~ None ~ gs => DefAST( n, FunctionExprAST(Nil, gs) )
				case n ~ Some(p) ~ gs => DefAST( n, FunctionExprAST(p, gs) )}

	lazy val optionallyGuardedPart = opt("|" ~> booleanExpression) ~ ("=" ~> expressionOrBlock) <~ Newline ^^
		{case g ~ b => List(FunctionPartExprAST(g, b))}

	lazy val guardedPart =
		"|" ~> ("otherwise" ^^^ None | booleanExpression ^^ (e => Some(e))) ~ ("=" ~> expressionOrBlock) <~ Newline ^^
			{case g ~ b => FunctionPartExprAST(g, b)}

	lazy val guardedParts =
		Indent ~> rep1(guardedPart) <~ Dedent <~ Newline

	lazy val statements =
		rep1(statement)

	lazy val optionalNewline = opt(Newline)

	lazy val expressionStatement: PackratParser[ExprAST] = expression <~ Newline
	
	lazy val statement: PackratParser[StatementAST] =
		expressionStatement ^^ (ExpressionStatementAST( _ )) |
		declaration
		
	lazy val blockExpression =
		Indent ~> statements <~ Dedent ^^
			(BlockExprAST( _ ))

	lazy val expression: PackratParser[ExprAST] =
		rep1sep(lvalueExpression, ",") ~ ("=" | "+=" | "-=" | "*=" | "/=" | "\\=" | "^=") ~ rep1sep(nonAssignmentExpression, ",") ^^
			{case lhs ~ op ~ rhs => AssignmentExprAST( lhs, Symbol(op), rhs )} |
  nonAssignmentExpression

 	lazy val lambdaExpression =
 	("(" ~> rep1sep(pattern, ",") <~ ")" | repN(1, pattern)) ~ opt("|" ~> booleanExpression) ~ ("->" ~> expression) ^^
		{case p ~ g ~ b => FunctionExprAST( p, List(FunctionPartExprAST(g, b)) )}

	lazy val caseFunctionExpression =
		Indent ~> rep1(lambdaExpression <~ Newline) <~ Dedent ^^
			{case c => CaseFunctionExprAST( c )}
			
	lazy val functionExpression =
		lambdaExpression | caseFunctionExpression
	
	lazy val nonAssignmentExpression =
		functionExpression |
		controlExpression

	lazy val elif =
		(optionalNewline ~ "elif") ~> booleanExpression ~ ("then" ~> expressionOrBlock | blockExpression) ^^ {case c ~ t => (c, t)}

	lazy val generator =
		(pattern <~ "<-") ~ expression ~ opt("if" ~> expression) ^^ {case p ~ t ~ f => GeneratorAST( p, t, f )}

	lazy val generators = rep1sep(generator, ",")

	lazy val expressionOrBlock = expression | blockExpression
	
	lazy val controlExpression =
		("if" ~> booleanExpression) ~ ("then" ~> expressionOrBlock | blockExpression) ~ rep(elif) ~ opt(optionalNewline ~> "else" ~> expressionOrBlock) ^^
			{case c ~ t ~ ei ~ e => ConditionalExprAST( (c, t) +: ei, e )} |
		"for" ~> generators ~ ("do" ~> expressionOrBlock | blockExpression) ~ opt(optionalNewline ~> "else" ~> expressionOrBlock) ^^
			{case g ~ b ~ e => ForExprAST( g, b, e )} |
    "for" ~> expressionOrBlock ^^
      (ForeverExprAST( _ )) |
		"while" ~> expression ~ ("do" ~> expressionOrBlock | blockExpression) ~ opt(optionalNewline ~> "else" ~> expressionOrBlock) ^^
			{case c ~ b ~ e => WhileExprAST( c, b, e )} |
		"do" ~> expression ~ (optionalNewline ~> "while" ~> expression) ~ opt(optionalNewline ~> "else" ~> expressionOrBlock) ^^
			{case b ~ c ~ e => DoWhileExprAST( b, c, e )} |
		"do" ~> expression ~ (optionalNewline ~> "until" ~> expression) ~ opt(optionalNewline ~> "else" ~> expressionOrBlock) ^^
			{case b ~ c ~ e => RepeatExprAST( b, c, e )} |
		"break" ^^^ BreakExprAST |
		"continue" ^^^ ContinueExprAST |
		"return" ~> opt(expression) ^^
			{case e => ReturnExprAST( e.getOrElse(VoidExprAST) )} |
		("case" ~> expression) ~ ("of" ~> functionExpression | caseFunctionExpression) ^^
			{case e ~ f => ApplyExprAST( f, List(e), false )} |
		orExpression

	lazy val booleanExpression = orExpression
	
	lazy val orExpression: PackratParser[ExprAST] =
		orExpression ~ ("or" | "xor") ~ andExpression ^^
			{case lhs ~ op ~ rhs => BinaryExprAST( lhs, Symbol(op), rhs )} |
		andExpression

	lazy val andExpression: PackratParser[ExprAST] =
		andExpression ~ ("and" | "rotateright" | "rotateleft" | "shiftright" | "shiftleft") ~ notExpression ^^
			{case lhs ~ op ~ rhs => BinaryExprAST( lhs, Symbol(op), rhs )} |
		notExpression

	lazy val notExpression: PackratParser[ExprAST] =
		"not" ~> notExpression ^^ (NotExprAST( _ )) |
		comparisonExpression

	lazy val comparisonExpression: PackratParser[ExprAST] =
		iteratorExpression ~ ("==" | "!=" | "<" | ">" | "<=" | ">=" | "in" | "not" ~ "in" ^^^ "notin" | "|" | "/|") ~ iteratorExpression ^^
			{case l ~ o ~ r => BinaryExprAST( l, Symbol(o), r )} |
		iteratorExpression ~ "is" ~ ident ^^ {case e ~ _ ~ t => TypeExprAST( e, t )} |
  iteratorExpression

	lazy val iteratorExpression: PackratParser[ExprAST] =
		(consExpression <~ "|") ~ generators ^^
			{case e ~ gs => IteratorExprAST( e, gs )} |
		consExpression
	
	lazy val consExpression: PackratParser[ExprAST] =
		rangeExpression ~ (":" ~> consExpression) ^^ {case h ~ t => ConsExprAST( h, t )} |
		rangeExpression ~ ("#" ~> consExpression) ^^ {case h ~ t => StreamExprAST( h, t )} |
		rangeExpression

	lazy val keyExpression = rangeExpression
	
	lazy val rangeExpression: PackratParser[ExprAST] =
		additiveExpression ~ (".." | "until") ~ additiveExpression ~ opt("by" ~> additiveExpression) ^^
			{case f ~ op ~ t ~ b => RangeExprAST( f, t, b, if (op == "..") true else false )} |
		(additiveExpression <~ "..") ~ opt("by" ~> additiveExpression) ^^
      {case f ~ b => UnboundedStreamExprAST( f, b )} |
  additiveExpression

	lazy val additiveExpression: PackratParser[ExprAST] =
		additiveExpression ~ ("+" | "-") ~ multiplicativeExpression ^^
			{case l ~ o ~ r => BinaryExprAST( l, Symbol(o), r )} |
		multiplicativeExpression

	lazy val multiplicativeExpression: PackratParser[ExprAST] =
		multiplicativeExpression ~ ("*" | "/" | """\""" | "%") ~ exponentialExpression ^^
			{case l ~ o ~ r => BinaryExprAST( l, Symbol(o), r )} |
		multiplicativeExpression ~ applyExpression ^^
			{case l ~ r => BinaryExprAST( l, '*, r )} |
  exponentialExpression

	lazy val exponentialExpression: PackratParser[ExprAST] =
		exponentialExpression ~ "^" ~ negationExpression ^^
			{case l ~ o ~ r => BinaryExprAST( l, Symbol(o), r )} |
  negationExpression

	lazy val negationExpression: PackratParser[ExprAST] =
		"-" ~> incrementExpression ^^
			(UnaryExprAST( '-, _ )) |
		incrementExpression

	lazy val incrementExpression: PackratParser[ExprAST] =
		("++" | "--") ~ applyExpression ^^
			{case o ~ e => UnaryExprAST( Symbol("pre" + o), e )} |
		applyExpression ~ ("++" | "--") ^^
			{case e ~ o => UnaryExprAST( Symbol("post" + o), e )} |
  applyExpression

	lazy val lvalueExpression = applyExpression
	
	lazy val applyExpression: PackratParser[ExprAST] =
		applyExpression ~ ("(" ~> repsep(expression, ",") <~ ")") ^^
			{case f ~ args => ApplyExprAST( f, args, false )} |
		applyExpression ~ ("." | ".>") ~ ident ^^ {case e ~ op ~ f => DotExprAST( e, f, op == "." )} |
  primaryExpression

	lazy val MapEntry =
		keyExpression ~ (":" ~> expression) ^^ {case k ~ v => TupleExprAST( k, v )}

	lazy val comprehensionExpression =
		consExpression ~ ("|" ~> generators) ^^
			{case e ~ g => IteratorExprAST( e, g )}
	
	lazy val primaryExpression: PackratParser[ExprAST] =
		numericLit ^^
			(n =>
				if (n startsWith "0x")
				{
				val num = BigInt( n substring 2, 16 )

					if (num.isValidInt)
						LiteralExprAST( num.intValue )
					else
						LiteralExprAST( num )
				}
				else if (n matches ".*(\\.|e|E).*")
					LiteralExprAST( n.toDouble )
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
    "(" ~> expression ~ infix <~ ")" ^^
      {case e ~ o => LeftSectionExprAST( e, Symbol(o) )} |
    "(" ~> infixNoMinus ~ expression <~ ")" ^^
      {case o ~ e => RightSectionExprAST( Symbol(o), e )} |
		"(" ~> expression <~ ")" |
		ident ^^
			{case v => VariableExprAST( v )} |
		("true" | "false") ^^
			(b => LiteralExprAST( b.toBoolean )) |
		"(" ~ ")" ^^^
			VoidExprAST |
		("(" ~> nonAssignmentExpression <~ ",") ~ (rep1sep(nonAssignmentExpression, ",") <~ ")") ^^
			{case e ~ l => VectorExprAST( e +: l )} |
		"[" ~> comprehensionExpression <~ "]" ^^
			(ListComprehensionExprAST( _ )) |
		"[" ~> repsep(nonAssignmentExpression, ",") <~ "]" ^^
			{case l => ListExprAST( l )} |
		"null" ^^
			(_ => NullExprAST) |
		"{" ~> repsep(keyExpression, ",") <~ "}" ^^
			(SetExprAST( _ )) |
		"{" ~> rep1sep(MapEntry, ",") <~ "}" ^^
			(MapExprAST( _ )) |
		"$" ~> ident ^^
			(SysvarExprAST( _ )) |
		"?" ~> ident ^^
			(TestExprAST( _ ))

	lazy val infixNoMinus = "+" | "*" | "/" | """\""" | "^" | "%" | "==" | "!=" | "<" | ">" | "<=" | ">=" | ":" | "#" | "and" | "or" | "xor"
	
	lazy val infix = infixNoMinus | "-"
	
	lazy val pattern: PackratParser[PatternAST] =
		(ident <~ "@") ~ typePattern ^^
			{case alias ~ pat => AliasPatternAST( alias, pat )} |
  typePattern

	lazy val typePattern: PackratParser[PatternAST] =
		consPattern ~ ("::" ~> ident) ^^
			{case pat ~ typename => TypePatternAST( pat, typename )} |
  consPattern
		
	lazy val consPattern: PackratParser[PatternAST] =
		primaryPattern ~ (":" ~> consPattern) ^^ {case h ~ t => ConsPatternAST( h, t )} |
  primaryPattern

	lazy val primaryPattern: PackratParser[PatternAST] =
		numericLit ^^
			(n =>
				if (n startsWith "0x")
				{
				val num = BigInt( n substring 2, 16 )

					if (num.isValidInt)
						LiteralPatternAST( num.intValue )
					else
						LiteralPatternAST( num )
				}
				else if (n matches ".*(\\.|e|E).*")
					LiteralPatternAST( n.toDouble )
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
			VoidPatternAST |
		"null" ^^^
			NullPatternAST |
		ident ~ ("(" ~> rep1sep(pattern, ",") <~ ")") ^^
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