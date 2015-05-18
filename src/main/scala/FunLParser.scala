/*     ______            __                                     *\
**    / ____/_  __ ___  / /     FunL Programming Language       **
**   / __/ / / / / __ \/ /      (c) 2014 Edward A. Maxedon, Sr. **
**  / /   / /_/ / / / / /__     http://funl-lang.org/           **
** /_/    \____/_/ /_/____/                                     **
\*                                                              */

package funl.interp

import util.parsing.combinator.PackratParsers
import util.parsing.combinator.syntactical.StandardTokenParsers
import util.parsing.input.{Reader, CharSequenceReader}
import collection.mutable.ListBuffer

import ca.hyperreal.indentation._
import ca.hyperreal.lia.Math


class FunLParser( module: String ) extends StandardTokenParsers with PackratParsers
{
	private val VARIABLE_PATTERN = """\$(?:([a-zA-Z_]+\d*)|\{([^}]+)\})"""r
	private val INTERPOLATED_PATTERN = """[\ue000-\ue002]([^\ue000-\ue002]+)"""r
	private val INTERPOLATION_DELIMITER = '\ue000'
	private val INTERPOLATION_LITERAL = '\ue000'
	private val INTERPOLATION_VARIABLE = '\ue001'
	private val INTERPOLATION_EXPRESSION = '\ue002'
	
	override val lexical: IndentationLexical =
		new IndentationLexical( false, true, List("[", "(", "{"), List("]", ")", "}") )
		{
			override def token: Parser[Token] = stringParser | hexParser | decimalParser | super.token

			private def stringParser: Parser[Token] =
        (''' ~ ''' ~ ''') ~> rep(guard(not(''' ~ ''' ~ ''')) ~> elem("", ch => true)) <~ (''' ~ ''' ~ ''') ^^
          {case l => StringLit( l mkString "" )} |
        ('"' ~ '"' ~ '"') ~> rep(guard(not('"' ~ '"' ~ '"')) ~> elem("", ch => true)) <~ ('"' ~ '"' ~ '"') ^^
          {case l => StringLit( interpolate(l mkString "", true) )} |
        ''' ~> rep(guard(not(''')) ~> (('\\' ~ ''' ^^^ "\\'") | elem("", ch => true))) <~ ''' ^^
          {case l => StringLit( escape(l mkString "") )} |
        '"' ~> rep(guard(not('"')) ~> (('\\' ~ '"' ^^^ "\\\"") | elem("", ch => true))) <~ '"' ^^
          {case l => StringLit( interpolate(l mkString "", true) )}

			private def decimalParser: Parser[Token] =
				rep1(digit) ~ optFraction ~ optExponent ^^
					{case intPart ~ frac ~ exp =>
						NumericLit( (intPart mkString "") :: frac :: exp :: Nil mkString "")} |
				fraction ~ optExponent ^^
					{case frac ~ exp => NumericLit( frac + exp )}

			private def sign = elem('+') | elem('-')

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

			private def exponent = (elem('e') | elem('E')) ~ optSign ~ rep1(digit) ^^
				{case e ~ optSign ~ exp => e :: optSign :: (exp mkString "") :: Nil mkString ""}

			private def optExponent: Parser[String] =
				opt(exponent) ^^
				{
					case None => ""
					case Some( exponent ) => exponent
				}

			private def hexParser: Parser[Token] =
				'0' ~> 'x' ~> rep1(digit | elem('a') | elem('b') | elem('c') | elem('d') | elem('e') | elem('f') |
					elem('A') | elem('B') | elem('C') | elem('D') | elem('E') | elem('F')) ^^
					(d => NumericLit( "0x" + (d mkString "") ))
				
			reserved += (
				"and", 			"break",		"by", 			"case", 		"class",		"continue",		"data",			"def",			"do", 			
				"elif",			"else", 		"false", 		"for", 			"function",		"if", 			"import",		"in", 			"is", 
				"loop",			"mod",			"native", 		"not",			"null", 		"of",			"or", 			"otherwise",	"private",
				"return", 		"repeat", 		"rotateleft",	"rotateright",	"then",			"true",			"until",		"val", 			"var",
				"while",		"xor", 			"yield"
				)
			delimiters += ("+", "*", "-", "/", "%", "^", "(", ")", "[", "]", "|", "/|", "{", "}", ",", ";",
				"=", "==", "!=", "<", "$", "?", ">", "<-", "<=", ">=", "--", "++", ".", ".>", "..", "<-", "->",
				"=>", "+=", "++=", "-=", "--=", "*=", "/=", "\\=", "^=", ":", "#", "\\", "\\%", "::", "@", ">>>", "<<")
		}
		
		def interpolate( s: String, handleEscape: Boolean ): String =
		{
		val buf = new StringBuilder
		var last = 0
		var nonliteral = false
		
			def append( code: Char, s: String )
			{
				buf += code
				buf append s
			}
			
			def literal( s: String ) = append( INTERPOLATION_LITERAL, if (handleEscape) escape(s) else s )
			
			for (m <- VARIABLE_PATTERN.findAllMatchIn( s ))
			{
				if (m.start > last)
					literal( s.substring(last, m.start) )
					
				if (m.matched.charAt( 1 ) == '{')
					append( INTERPOLATION_EXPRESSION, m.group(2) )
				else
					append( INTERPOLATION_VARIABLE, m.group(1) )
					
				nonliteral = true
				last = m.end
			}
		
			if (last < s.length)
				literal( s.substring(last) )
				
			if (!nonliteral)
				buf.deleteCharAt( 0 )
				
			buf.toString
		}
		
		def escape( s: String) =
		{
		val buf = new StringBuilder
			
			def chr( r: Reader[Char] )
			{
					if (!r.atEnd)
					{
						if (r.first == '\\')
						{
							if (r.rest.atEnd)
								RuntimeException( "unexpected end of string" )

							if (r.rest.first == 'u')
							{
							var u = r.rest.rest
							
								def nextc =
									if (u.atEnd)
										RuntimeException( "unexpected end of string inside unicode sequence" )
									else
									{
									val res = u.first

										u = u.rest
										res
									}

								buf append Integer.valueOf( new String(Array(nextc, nextc, nextc, nextc)), 16 ).toChar
								chr( u )
							}
							else
							{
								buf.append(
									Map (
										'\\' -> '\\', '\'' -> '\'', '"' -> '"', '$' -> '$', '/' -> '/', 'b' -> '\b', 'f' -> '\f', 'n' -> '\n', 'r' -> '\r', 't' -> '\t'
									).get(r.rest.first) match
									{
										case Some( c ) => c
										case _ => RuntimeException( "illegal escape character " + r.rest.first )
									} )

								chr( r.rest.rest )
							}
						}
						else
						{
							buf append r.first	
							chr( r.rest )
						}
					}
			}

			chr( new CharSequenceReader(s) )
			buf.toString()
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

	lazy val assignment = "=" | "+=" | "++=" | "-=" | "--=" | "*=" | "/=" | "\\=" | "^="
	
	lazy val expression: PackratParser[ExprAST] =
		compoundExpression | assignmentExpression

	lazy val compoundExpression: PackratParser[ExprAST] =
		rep1sep( assignmentExpression, ";" ) ^^
			(l =>
				if (l.length == 1)
					l.head
				else
					CompoundExprAST( l map (e => ExpressionStatementAST(e)) ))

	lazy val assignmentExpression: PackratParser[ExprAST] =
		rep1sep(lvalueExpression, ",") ~ assignment ~ (rep1sep(nonAssignmentExpression, ",") | blockExpression ^^ (List( _ ))) ^^
			{case lhs ~ op ~ rhs => AssignmentExprAST( lhs, op, rhs )} |
  nonAssignmentExpression

 	lazy val lambdaExpression =
// 	("(" ~> rep1sep(pattern, ",") <~ ")" | repN(1, pattern)) ~ opt("|" ~> booleanExpression) ~ ("->" ~> expressionOrBlock) ^^
 	("\\" ~> rep1sep(pattern, ",") | repN(1, pattern)) ~ opt("|" ~> booleanExpression) ~ ("->" ~> opt(expressionOrBlock)) ^^
		{case p ~ g ~ b => FunctionExprAST( p, List(FunctionPartExprAST(g, b.getOrElse(VoidExprAST))) )} |
	"otherwise" ~> "->" ~> opt(expressionOrBlock) ^^
		{case b => FunctionExprAST( List(VariablePatternAST("_")), List(FunctionPartExprAST(None, b.getOrElse(VoidExprAST))) )}

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

	lazy val elsePart: PackratParser[Option[ExprAST]] = opt(optionalNewline ~> "else" ~> expressionOrBlock)
	
	lazy val controlExpression: PackratParser[ExprAST] =
		("if" ~> booleanExpression) ~ ("then" ~> expressionOrBlock | blockExpression) ~ rep(elif) ~ elsePart ^^
			{case c ~ t ~ ei ~ e => ConditionalExprAST( (c, t) +: ei, e )} |
		"for" ~> generators ~ ("do" ~> expressionOrBlock | blockExpression) ~ elsePart ^^
			{case g ~ b ~ e => ForExprAST( g, b, e )} |
		"loop" ~> expressionOrBlock ^^
			(ForeverExprAST( _ )) |
		"repeat" ~> expression ~ ("do" ~> expressionOrBlock | blockExpression) ~ elsePart ^^
			{case c ~ b ~ e => RepeatExprAST( c, b, e )} |
		"while" ~> expression ~ ("do" ~> expressionOrBlock | blockExpression) ~ elsePart ^^
			{case c ~ b ~ e => WhileExprAST( c, b, e )} |
		"do" ~> expression ~ (optionalNewline ~> "while" ~> expression) ~ elsePart ^^
			{case b ~ c ~ e => DoWhileExprAST( b, c, e )} |
		"do" ~> expression ~ (optionalNewline ~> "until" ~> expression) ~ elsePart ^^
			{case b ~ c ~ e => DoUntilExprAST( b, c, e )} |
		"break" ^^^ BreakExprAST |
		"continue" ^^^ ContinueExprAST |
		"return" ~> opt(expression) ^^
			{case e => ReturnExprAST( e.getOrElse(VoidExprAST) )} |
		"yield" ~> expression ^^
			(YieldExprAST( _ )) |
		("case" ~> expression) ~ ("of" ~> functionExpression | caseFunctionExpression) ^^
			{case e ~ f => ApplyExprAST( f, List(e), false )} |
		orExpression

	lazy val booleanExpression = orExpression
	
	lazy val mathSymbols = Set( '+, '-, '*, '/, Symbol("\\"), Symbol("\\%"), '^, '%, 'mod, '|, '/|, '==, '!=, '<, '>, '<=, '>= )
	
	def lookup( s: Symbol ) =
		if (mathSymbols contains s)
			Math.lookup( s )
		else
			null
	
	lazy val orExpression: PackratParser[ExprAST] =
		orExpression ~ ("or" | "xor") ~ andExpression ^^
			{case lhs ~ op ~ rhs =>
				val s = Symbol(op)
				BinaryExprAST( lhs, s, Math.lookup(s), rhs )} |
		andExpression

	lazy val andExpression: PackratParser[ExprAST] =
		andExpression ~ "and" ~ notExpression ^^
			{case lhs ~ op ~ rhs => BinaryExprAST( lhs, 'and, Math.lookup('and), rhs )} |
		notExpression

	lazy val notExpression: PackratParser[ExprAST] =
		"not" ~> notExpression ^^ (NotExprAST( _ )) |
		comparisonExpression

	lazy val comparisonExpression: PackratParser[ExprAST] =
		iteratorExpression ~ rep1(("==" | "!=" | "<" | ">" | "<=" | ">=") ~ iteratorExpression) ^^
			{case l ~ comps => ComparisonExprAST( l, comps map {
				case o ~ e =>
					val s = Symbol( o )
					
					(s, Math.lookup(s), e)} )} |
		iteratorExpression ~ ("in" | "not" ~ "in" ^^^ "notin" | "|" | "/|") ~ iteratorExpression ^^
			{case l ~ o ~ r =>
				val s = Symbol( o )
					
				BinaryExprAST( l, s, lookup(s), r )} |
		iteratorExpression ~ "is" ~ ident ^^ {case e ~ _ ~ t => TypeExprAST( e, t )} |
		iteratorExpression ~ "is" ~ "not" ~ ident ^^ {case e ~ _ ~ _ ~ t => NotExprAST( TypeExprAST(e, t) )} |
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
			{case l ~ o ~ r =>
				val s = Symbol(o)
				
				BinaryExprAST( l, s, lookup(s), r )} |
		multiplicativeExpression

	lazy val multiplicativeExpression: PackratParser[ExprAST] =
		multiplicativeExpression ~ ("*" | "/" | """\""" | "%" | "\\%" | "mod" | "rotateright" | "rotateleft" | ">>>" | "<<") ~ exponentialExpression ^^
			{case l ~ o ~ r =>
				val s = Symbol(o)
				
				BinaryExprAST( l, s, lookup(s), r )} |
		multiplicativeExpression ~ applyExpression ^^
			{case l ~ r => BinaryExprAST( l, '*, lookup('*), r )} |
		exponentialExpression

	lazy val exponentialExpression: PackratParser[ExprAST] =
		exponentialExpression ~ "^" ~ negationExpression ^^
			{case l ~ _ ~ r => BinaryExprAST( l, '^, lookup('^), r )} |
		negationExpression

	lazy val negationExpression: PackratParser[ExprAST] =
		"-" ~> incrementExpression ^^
			{
				case LiteralExprAST( n: Number ) => LiteralExprAST( Math('-, n) )
				case v                           => UnaryExprAST( '-, v )
			} |
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
			(s =>
				if (s.length > 0 && s.charAt(0) >= INTERPOLATION_DELIMITER)
				{
				val buf = new ListBuffer[ExprAST]
				
					for (m <- INTERPOLATED_PATTERN.findAllMatchIn( s ))
						m.matched.charAt( 0 ) match
						{
							case INTERPOLATION_LITERAL => buf.append( StringLiteralExprAST(m.group(1)) )
							case INTERPOLATION_VARIABLE => buf.append( VariableExprAST(m.group(1)) )
							case INTERPOLATION_EXPRESSION =>
								val parser = new FunLParser( "-expression-" )

								parser.parseExpression( new CharSequenceReader(m.group(1)) ) match
								{
									case parser.Success( l, _ ) =>
										buf.append( l )
									case parser.Failure( m, r ) => sys.error( r.pos + ": " + m + '\n' + r.pos.longString )
									case parser.Error( m, r ) => sys.error( r.pos + ": " + m )
								}
						}
						
					InterpolationExprAST( buf.toList )
				}
				else
					StringLiteralExprAST( s )) |
		"(" ~> infix <~ ")" ^^
			{o =>
				val s = Symbol(o)
				
				SectionExprAST( s, lookup(s) )} |
		"(" ~> expression ~ infix <~ ")" ^^
			{case e ~ o =>
				val s = Symbol(o)
				
				LeftSectionExprAST( e, s, lookup(s) )} |
		"(" ~> infixNoMinus ~ expression <~ ")" ^^
			{case o ~ e =>
				val s = Symbol(o)
				
				RightSectionExprAST( s, lookup(s), e )} |
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
		"null" ^^^
			NullExprAST |
		"{" ~> comprehensionExpression <~ "}" ^^
		(SetComprehensionExprAST( _ )) |
		"{" ~> repsep(keyExpression, ",") <~ "}" ^^
			(SetExprAST( _ )) |
		"{" ~> rep1sep(MapEntry, ",") <~ "}" ^^
			(MapExprAST( _ )) |
		"{" ~ ":" ~ "}" ^^^
		EmptyMapExprAST |
		"$" ~> ident ^^
			(SysvarExprAST( _ )) |
		"?" ~> ident ^^
			(TestExprAST( _ ))

	lazy val infixNoMinus =
		"+" | "*" | "/" | """\""" | "\\%" | "^" | "%" | "mod" | "|" | "/|" | "==" | "!=" | "<" | ">" | "<=" | ">=" | "in" | "not" ~ "in" ^^^ "notin" |
		":" | "#" | "and" | "or" | "xor"
	
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
