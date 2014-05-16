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
			override def token: Parser[Token] = decimalParser | super.token

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

			private def exponent = (chr( 'e' ) | chr( 'E' )) ~ optSign ~ rep1(digit) ^^
				{case e ~ optSign ~ exp => e :: optSign :: (exp mkString "") :: Nil mkString ""}

			private def optExponent: Parser[String] =
				opt(exponent) ^^
				{
					case None => ""
					case Some( exponent ) => exponent
				}
				
			reserved += ("do", "if", "then", "for", "else", "elsif", "by", "while", "var", "from", "import", "break", "continue", "repeat", "until", "of",
				"export", "class", "main", "data", "def", "true", "false", "val", "null", "not", "and", "or", "xor", "otherwise", "in", "case",
				"method", "field", "function")
			delimiters += ("+", "*", "-", "/", "^", "(", ")", "[", "]", "|", "{", "}", ",", "=", "==", "/=", "<", "$",
				">", "<-", "<=", ">=", "--", "++", ".", "..", "<-", "->", "=>", "+=", "-=", "*=", "^=", ":", "\\", "::", "@")
		}

	import lexical.{Newline, Indent, Dedent}

	def parseSource( r: Reader[Char] ) = phrase( source )( lexical.read(r) )

	def parseExpression( r: Reader[Char] ) = phrase( expr )( lexical.read(r) )

	def parseStatement( r: Reader[Char] ) = phrase( statement )( lexical.read(r) )

	def parseSnippet( r: Reader[Char] ) = phrase( snippet )( lexical.read(r) )

	lazy val snippet: PackratParser[BlockExprAST] =
		statements ^^ (BlockExprAST( _ ))
		
	lazy val source: PackratParser[ModuleAST] = rep(statement) ^^ {case l => ModuleAST(module, l)}

// 	lazy val component: PackratParser[List[ComponentAST]] = imports | symbolImports | natives | constants | variables | data | definitions | main
// 
// 	lazy val imports =
// 		"import" ~> importModule ^^ (List(_)) |
// 		"import" ~> Indent ~> rep1(importModule) <~ Dedent <~ Newline
// 
// 	lazy val symbolImports =
// 		("from" ~> ident <~ "import") ~ idents <~ Newline ^^
// 			{case m ~ s => List( ImportSymbolsAST(module, m, s) )} |
// 		("from" ~> ident <~ "import") <~ "*" <~ Newline ^^
// 			{case m => List( ImportSymbolsAST(module, m, null) )}
// 
// 	lazy val importModule =
// 		ident <~ Newline ^^ (ImportModuleAST( module, _ ))
// 
// 	lazy val natives =
// 		"class" ~> native ^^ {case (pkg, names) => List( ClassAST(module, pkg, names) )} |
// 		"class" ~> Indent ~> rep1(native) <~ Dedent <~ Newline ^^
// 			(cs => cs map {case (pkg, names) => ClassAST( module, pkg, names )}) |
// 		"method" ~> native ^^ {case (cls, names) => List( MethodAST(module, cls, names) )} |
// 		"method" ~> Indent ~> rep1(native) <~ Dedent <~ Newline ^^ (cs => cs map {case (cls, names) => MethodAST( module, cls, names )}) |
// 		"field" ~> native ^^ {case (cls, names) => List( FieldAST(module, cls, names) )} |
// 		"field" ~> Indent ~> rep1(native) <~ Dedent <~ Newline ^^ (cs => cs map {case (cls, names) => FieldAST( module, cls, names )}) |
// 		"function" ~> native ^^ {case (cls, names) => List( FunctionAST(module, cls, names) )} |
// 		"function" ~> Indent ~> rep1(native) <~ Dedent <~ Newline ^^ (cs => cs map {case (cls, names) => FunctionAST( module, cls, names )})
// 
// 	lazy val dottedName = rep1sep(ident, ".")
// 
// 	lazy val className = ident ~ opt("=>" ~> ident) ^^ {case name ~ alias => (name, alias)}
// 	
// 	lazy val native: PackratParser[(String, List[(String, Option[String])])] =
// 		dottedName ~ opt("=>" ~> ident) <~ Newline ^^
// 			{case name ~ alias => (name.init.mkString( "." ), List((name.last, alias)))} |
// 		(dottedName <~ ".") ~ ("{" ~> rep1sep(className, ",") <~ "}" <~ Newline) ^^
// 			{case pkg ~ names => (pkg.mkString( "." ), names)}
// 
// 	lazy val idents = rep1sep( ident, "," )
// 	
// 	lazy val constants =
// 		"val" ~> constant ^^ {case c => List( c )} |
// 		"val" ~> Indent ~> rep1(constant) <~ Dedent <~ Newline
// 
// 	lazy val constant =
// 		(ident <~ "=") ~ expr <~ Newline ^^
// 			{case n ~ c => ConstAST( module, n, c )}
// 
// 	lazy val variables =
// 		"var" ~> variable ^^ {case v => List( v )} |
// 		"var" ~> Indent ~> rep1(variable) <~ Dedent <~ Newline
// 
// 	lazy val variable =
// 		ident ~ opt("=" ~> expr) <~ Newline ^^
// 			{case n ~ v => VarAST( module, n, v )}
// 
// 	lazy val data =
// 		"data" ~> datatype ^^ {case v => List( v )} |
// 		"data" ~> Indent ~> rep1(datatype) <~ Dedent <~ Newline
// 
// 	lazy val datatype =
// 		(ident <~ "=") ~ rep1sep(constructor, "|") <~ Newline ^^
// 			{case typename ~ constructors => DataAST( module, typename, constructors )} |
// 		constructor <~ Newline ^^
// 			{case c => DataAST( module, c._1, List(c) )}
// 
// 	lazy val constructor =
// 		(ident <~ "(") ~ (idents <~ ")") ^^
// 			{case name ~ fields => (name, fields)} |
// 		ident ^^
// 			{case name => (name, Nil)}
// 		
// 	lazy val definitions =
// 		"def" ~> definition ^^ {case d => List( d )} |
// 		"def" ~> (Indent ~> rep1(definition) <~ (Dedent ~ Newline))
// 
// 	lazy val definition =
// 		ident ~ opt("(" ~> repsep(pattern, ",") <~ ")") ~ (part | parts) ^^
// 			{	case n ~ None ~ gs => DefAST( module, n, FunctionExprAST(module, Nil, gs) )
// 				case n ~ Some(p) ~ gs => DefAST( module, n, FunctionExprAST(module, p, gs) )}
// 
// 	lazy val locals = opt("local" ~> idents)
// 	
// 	lazy val part = opt("|" ~> expr10) ~ locals ~ ("=" ~> expr) <~ Newline ^^
// 		{case g ~ l ~ b => List(FunctionPartExprAST(g, l, b))}
// 
// 	lazy val subpart =
// 		"|" ~> ("otherwise" ^^^ None | expr10 ^^ (e => Some(e))) ~ locals ~ ("=" ~> expr) <~ Newline ^^
// 			{case g ~ l ~ b => FunctionPartExprAST(g, l, b)}
// 
// 	lazy val parts =
// 		Indent ~> rep1(subpart) <~ Dedent <~ Newline
// 
// 	lazy val main =
// 		"main" ~> statement ^^ (s => List(MainAST( module, s )))

	lazy val statements =
		rep1(statement)

	lazy val onl = opt(Newline)
	
	lazy val statement: PackratParser[StatementAST] =
// 		ident ~ rep1sep( expr, "," ) <~ Newline ^^
// 			{case f ~ args =>
// 				if (args == List(UnitExprAST))
// 					ExpressionStatementAST( ApplyExprAST(VariableExprAST(Symbol(f)), Nil, false) )
// 				else
// 					ExpressionStatementAST( ApplyExprAST(VariableExprAST(Symbol(f)), args, false) )} |
		statementExpr <~ Newline
	
	lazy val statementExpr: PackratParser[StatementAST] =
		("val" ~> pattern <~ "=") ~ expr5 ^^
			{case pat ~ exp => ValStatementAST( pat, exp )} |
		expr ^^ (ExpressionStatementAST( _ ))

	lazy val expr: PackratParser[ExprAST] =
		rep1sep(leftExpr, ",") ~ ("=" | "+=" | "-=" | "*=" | "/=" | "^=") ~ rep1sep(expr5, ",") ^^
			{case lhs ~ op ~ rhs => AssignmentExprAST( lhs, Symbol(op), rhs )} |
		expr5

 	lazy val mapping =
 	("(" ~> rep1sep(pattern, ",") <~ ")" | repN(1, pattern)) ~ opt("|" ~> expr10) ~ ("->" ~> expr) ^^
		{case p ~ g ~ b => FunctionExprAST( module, p, List(FunctionPartExprAST(g, None, b)) )}

	lazy val caseFunctionExpr =
		Indent ~> rep1(mapping <~ Newline) <~ Dedent ^^
			{case c => CaseFunctionExprAST( module, c )}
			
	lazy val functionExpr =
		mapping | caseFunctionExpr

	lazy val expr5 =
		functionExpr |
		expr7

	lazy val elif =
		(onl ~ "elsif") ~> booleanExpr ~ ("then" ~> expr | block) ^^ {case c ~ t => (c, t)}

	lazy val expr7 =
		("if" ~> booleanExpr) ~ ("then" ~> expr | block) ~ rep(elif) ~ opt(onl ~> "else" ~> expr) ^^
			{case c ~ t ~ ei ~ e => ConditionalExprAST( (c, t) +: ei, e )} |
		("for" ~> pattern <~ "<-") ~ expr ~ ("do" ~> expr | block) ~ opt(onl ~> "else" ~> expr) ^^
			{case v ~ r ~ b ~ e => ForExprAST( v, r, b, e )} |
		("while" ~> expr) ~ ("do" ~> expr | block) ~ opt(onl ~> "else" ~> expr) ^^
			{case c ~ b ~ e => WhileExprAST( c, b, e )} |
		("do" ~> expr) ~ (onl ~> "while" ~> expr) ~ opt(onl ~> "else" ~> expr) ^^
			{case b ~ c ~ e => DoWhileExprAST( b, c, e )} |
		("repeat" ~> expr) ~ (onl ~> "until" ~> expr) ~ opt(onl ~> "else" ~> expr) ^^
			{case b ~ c ~ e => RepeatExprAST( b, c, e )} |
		"break" ^^^ BreakExprAST |
		"continue" ^^^ ContinueExprAST |
		("case" ~> expr) ~ ("of" ~> functionExpr | caseFunctionExpr) ^^
			{case e ~ f => ApplyExprAST( f, List(e), false )} |
		expr10

	lazy val booleanExpr = expr10
	
	lazy val expr10 =
		expr11 ~ rep(("or" | "xor") ~ expr11) ^^
			{case lhs ~ list => (lhs /: list){case (x, op ~ y) => BooleanConnectiveExprAST( x, Symbol(op), y )}}

	lazy val expr11 =
		expr12 ~ rep("and" ~ expr12) ^^
			{case lhs ~ list => (lhs /: list){case (x, op ~ y) => BooleanConnectiveExprAST( x, Symbol(op), y )}}

	lazy val expr12: PackratParser[ExprAST] =
		"not" ~> expr12 ^^ (NotExprAST( _ )) |
		expr22

	lazy val expr22: PackratParser[ExprAST] =
		expr26 ~ ("==" | "/=" | "<" | ">" | "<=" | ">=" | "in" | "not" ~ "in" ^^^ "notin") ~ expr26 ^^
			{case l ~ o ~ r => BinaryExprAST( l, Symbol(o), r )} |
		expr26

	lazy val expr26: PackratParser[ExprAST] =
		expr27 ~ (":" ~> expr26) ^^ {case h ~ t => ConsExprAST( h, t )} |
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
		expr31 ~ ("*" | "/" | "\\") ~ expr32 ^^
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

	lazy val expr40: PackratParser[ExprAST] =
		numericLit ^^
			(n =>
				if (n matches ".*(\\.|e|E).*")
					DoubleLiteralExprAST( n.toDouble )
				else
					IntegerLiteralExprAST( n.toInt )) |
		stringLit ^^
			(StringLiteralExprAST( _ )) |
		"(" ~> expr <~ ")" |
		ident ^^
			{case v => VariableExprAST( module, v )} |
		("true" | "false") ^^
			(b => BooleanLiteralExprAST( b.toBoolean )) |
		"(" ~ ")" ^^^
			UnitExprAST |
		("(" ~> expr <~ ",") ~ (rep1sep(expr, ",") <~ ")") ^^
			{case e ~ l => VectorExprAST( e +: l )} |
		("[" ~> repsep(expr, ",") <~ "]") ^^
			{case l => ListExprAST( l )} |
		"null" ^^
			(_ => NullExprAST) |
		"{" ~> repsep(jsonExpr, ",") <~ "}" ^^
			(SetExprAST( _ )) |
		"{" ~> repsep(entry, ",") <~ "}" ^^
			(MapExprAST( _ )) |
		"$" ~> ident ^^
			(SysvarExprAST( _ ))
			
	lazy val pattern =
		(ident <~ "@") ~ pattern5 ^^
			{case alias ~ pat => AliasPatternAST( alias, pat )} |
		pattern5
			
	lazy val pattern5: PackratParser[PatternAST] =
		pattern10 ~ (":" ~> pattern5) ^^ {case h ~ t => ConsPatternAST( h, t )} |
		pattern10

	lazy val pattern10: PackratParser[PatternAST] =
		pattern20 |
		ident ~ ("(" ~> repsep(pattern, ",") <~ ")") ^^
			{case n ~ l => RecordPatternAST( n, l )} |
		ident ~ opt("::" ~> ident) ^^
			{case v ~ t => VariablePatternAST( v, t )} |
		("(" ~> pattern <~ ",") ~ (rep1sep(pattern, ",") <~ ")") ^^
			{case e ~ l => TuplePatternAST( e +: l )} |
		("[" ~> repsep(pattern, ",") <~ "]") ^^
			{case l => ListPatternAST( l )} |
		("(" ~> pattern30 <~ "|") ~ (rep1sep(pattern30, "|") <~ ")") ^^
			{case e ~ l => AltPatternAST( e +: l )} |
		"(" ~> pattern <~ ")"

	lazy val pattern20 =
		numericLit ^^
			(n =>
				if (n matches ".*(\\.|e|E).*")
					DoubleLiteralPatternAST( n.toDouble )
				else
					IntegerLiteralPatternAST( n.toInt )) |
		stringLit ^^
			(StringLiteralPatternAST( _ )) |
		("true" | "false") ^^
			(b => BooleanLiteralPatternAST( b.toBoolean )) |
		"(" ~ ")" ^^^
			UnitPatternAST |
		"null" ^^^
			NullPatternAST

	lazy val pattern30: PackratParser[PatternAST] =
		pattern40 ~ (":" ~> pattern30) ^^ {case h ~ t => ConsPatternAST( h, t )} |
		pattern40

	lazy val pattern40: PackratParser[PatternAST] =
		pattern20 |
		("(" ~> pattern30 <~ ",") ~ (rep1sep(pattern30, ",") <~ ")") ^^
			{case e ~ l => TuplePatternAST( e +: l )} |
		"[" ~> repsep(pattern30, ",") <~ "]" ^^
			{case l => ListPatternAST( l )} |
		"(" ~> pattern30 <~ ")"
}