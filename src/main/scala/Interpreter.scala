/*     ______            __                                     *\
**    / ____/_  __ ___  / /     FunL Programming Language       **
**   / __/ / / / / __ \/ /      (c) 2014 Edward A. Maxedon, Sr. **
**  / /   / /_/ / / / / /__     http://funl-lang.org/           **
** /_/    \____/_/ /_/____/                                     **
\*                                                              */

package funl.interp

import java.io.{File, InputStream, FileInputStream, ByteArrayOutputStream}

// import scala.util.parsing.input.PagedSeqReader
// import scala.collection.immutable.PagedSeq
import util.parsing.input.{Reader, CharSequenceReader}
import io.Source

import funl.lia.Math


object Interpreter
{
	val PREDEF = "-predef-"
	val VERSION =
		{
			val p = getClass.getPackage
			val name = p.getImplementationTitle
			
			p.getImplementationVersion
		}
	
	val NATURAL_ORDERING =
		new Ordering[Any]
		{
			def compare( x: Any, y: Any ): Int = naturalCompare( x, y )
		}

	def naturalCompare( x: Any, y: Any ): Int =
		(x, y) match
		{
			case (a: Number, b: Number) =>
				if (Math( '<, a, b ).asInstanceOf[Boolean])
					-1
				else if (Math( '>, a, b ).asInstanceOf[Boolean])
					1
				else
					0
			case (a: String, b: String) => a compare b
			case (a: Seq[Any], b: Seq[Any]) => lexicographicalCompare( a, b )
			case (a: Product, b: Product) if a.productPrefix == b.productPrefix => lexicographicalCompare( a.productIterator.toSeq, b.productIterator.toSeq )
			case _ => RuntimeException( "non-comparable: " + x + ", " + y )
		}

	def lexicographicalCompare( a: Seq[Any], b: Seq[Any] ): Int =
	{
		for ((u, v) <- a zip b)
			if (u != v)
				return naturalCompare( u, v )
	
		val (alen, blen) = (a.length, b.length)
		
		if (alen < blen)
			-1
		else if (alen > blen)
			1
		else
			0
	}
	
	def markTailRecursion( m: ModuleAST )
	{
		for (s <- m.statements)
			markTailRecursion( s )
	}
	
	def markTailRecursion( s: StatementAST )
	{
		s match
		{
			case DeclarationBlockAST( decls ) =>
				for (c <- decls)
					c match
					{
						case DefAST( name, func ) =>
							for (p <- func.parts)
								markTailRecursion( name, p.body )
						case _ =>
					}
			case _ =>
		}
	}
	
	def markTailRecursion( n: String, e: ExprAST )
	{
		e match
		{
			case BlockExprAST( l ) =>
				for (s <- l)
					s match
					{
						case DeclarationBlockAST( decls ) =>
							for (c <- decls)
								c match
								{
									case DefAST( name, func ) =>
										for (p <- func.parts)
											markTailRecursion( name, p.body )
									case _ =>
								}
						case _ =>
					}

				l.last match
				{
					case ExpressionStatementAST( e ) => markTailRecursion( n, e )
					case _ =>
				}
			case a@ApplyExprAST( f, _, _ ) =>
				f match
				{
					case VariableExprAST( v ) =>
						if (v == n)
							a.tailrecursive = true
					case CaseFunctionExprAST( cases ) =>
						for (c <- cases)
							markTailRecursion( n, c.parts.head.body )
					case FunctionExprAST( _, parts ) =>
						markTailRecursion( n, parts.head.body )
					case _ =>
				}
			case ConditionalExprAST( cond, no ) =>
				for ((_, thenpart) <- cond)
					markTailRecursion( n, thenpart )

				if (no != None)
					markTailRecursion( n, no.get )
			case BinaryExprAST( LiteralExprAST(false), 'or | 'xor, e ) => 	markTailRecursion( n, e )
			case BinaryExprAST( LiteralExprAST(true), 'and, e ) => 	markTailRecursion( n, e )
			case BinaryExprAST( e, 'or | 'xor, LiteralExprAST(false) ) => 	markTailRecursion( n, e )
			case BinaryExprAST( e, 'and, LiteralExprAST(true) ) => 	markTailRecursion( n, e )
			case _ =>
		}
	}

	def displayQuoted( a: Any ): String =
		a match
		{
			case s: String =>
				var t = s
				
				for ((k, v) <- List( "\\" -> "\\\\", "\"" -> "\\\"", "\t" -> "\\t", "\b" -> "\\b", "\f" -> "\\f", "\n" -> "\\n", "\r" -> "\\r", "\b" -> "\\b" ))
					t = t.replaceAllLiterally( k, v )
					
				'"' + t + '"'
			case _ => display( a )
		}

	def display( a: Any ): String =
		a match
		{
			case a: Array[_] => a.map( display(_) ).mkString( "Array(", ", ", ")" )
			case l: List[_] => l.map( displayQuoted(_) ).mkString( "[", ", ", "]" )
			case s: Stream[_] =>
				val howMany = 100
				val bunch = s take (howMany + 1)

				if (s isDefinedAt (howMany + 1))
					bunch take howMany map (display(_)) mkString( "[", ", ", ", ...]" )
				else
					display( bunch toList )
			case s: collection.Set[_] => s.map( display(_) ).mkString( "{", ", ", "}" )
			case m: collection.Map[_, _] => m.toList.map( {case (k, v) => displayQuoted(k) + ": " + displayQuoted(v)} ).mkString( "{", ", ", "}" )
			case t: Vector[_] => t.map( display(_) ).mkString( "(", ", ", ")" )
			case p: Product if p.productArity > 0 && !p.productPrefix.startsWith( "Tuple" ) => p.productPrefix + '(' + p.productIterator.map( display(_) ).mkString( ", " ) + ')'
			case p: Product if p.productArity == 0 => p.productPrefix
//			case Some( a ) => "Some(" + display(a) + ")"
			case _ => String.valueOf( a )
		}

	def parse( module: String, input: String ): AST = parse( module, new CharSequenceReader(input) )

	def parse( module: String, input: Reader[Char] ): AST =
	{
	val parser = new FunLParser( module )

		parser.parseSource( input ) match
		{
			case parser.Success( l, _ ) =>
				markTailRecursion( l )
//println( l )
				l
			case parser.Failure( m, r ) => sys.error( r.pos + ": " + m + '\n' + r.pos.longString )
			case parser.Error( m, r ) => sys.error( r.pos + ": " + m )
		}
	}

	def parse( module: String, input: InputStream ): AST =
	{
	val lines = Source.fromInputStream( input ).getLines
	val source = new StringBuilder

			for (l <- lines)
			{
				source append l
				source append '\n'
			}

		parse( module, new CharSequenceReader(source) )
	}

	def parseLiterate( module: String, input: InputStream ): AST =
	{
	val lines = Source.fromInputStream( input ).getLines
	val source = new StringBuilder
	var appending = false

		def append( line: String )
		{
			source append line
			source append '\n'
		}
		
		for (l <- lines)
		{
			l.indexOf( "~~" ) match
			{
				case -1 =>
					if (appending)
						append( l )
				case idx =>
					if (appending)
					{
						append( l.substring(0, idx) )
						appending = false
					}
					else
						l.indexOf( "~~", idx + 2 ) match
						{
							case -1 =>
								append( l.substring(idx + 2) )
								appending = true
							case end => append( l.substring(idx + 2, end) )
						}
			}
		}

		parse( module, new CharSequenceReader(source) )
	}

//		val r = new PagedSeqReader( PagedSeq fromFile (args.head + ".fun") )
	def parse( module: String, name: Option[String] = None ): AST =
	{
	val m = name.getOrElse( module )
	
		moduleInput( module, "funl" ) match
		{
			case Some( input ) => parse( m, input )
			case None =>
				moduleInput( module, "lf" ) match
				{
					case Some( input ) => parseLiterate( m, input )
					case None => sys.error( "module '" + module + "' not found" )
				}
		}
	}

	def moduleInput( module: String, ending: String ) =
	{
	val filename = module + "." + ending
	val resource = funl.Main.getClass.getResourceAsStream( filename )
	
		if (resource eq null)
		{
		val file = new File( filename )

			if (!file.exists || !file.isFile)
				None
			else
				Some( new FileInputStream(file) )
		}
		else
			Some( resource )
	}

	def execute( module: String, name: Option[String], vs: (String, Any)* )
	{
	val m = name.getOrElse( module )
	val l = parse( module, name )
	val eval = new Evaluator

		eval.assign( m, vs: _* )
		eval( l )( new eval.Environment )
	}
	
	def executeCaptureOutput( module: String, name: Option[String], vs: (String, Any)* ) =
	{
	val out = new ByteArrayOutputStream
	
		Console.withOut( out )( execute(module, name, vs: _*) )
		out.toString
	}
	
	case class PARSE_FAILURE( message: String )

	def statement( m: String, s: String ): Any =
	{
	val eval = new Evaluator
	implicit val env = new eval.Environment
	
		eval.enterActivation( null, null, eval.module(m) )
		statement( m, s, eval )
	}

	def statement( m: String, s: String, eval: Evaluator )( implicit env: eval.Environment ) =
	{
	val parser = new FunLParser( m )

		parser.parseStatement( new CharSequenceReader(s) ) match
		{
			case parser.Success( l, _ ) =>
				markTailRecursion( l )
				eval.apply( l )
				eval.last
			case parser.Failure( m, r ) => PARSE_FAILURE( m )
			case parser.Error( m, r ) => PARSE_FAILURE( m )
		}
	}

	def statement( s: String ): Any = statement( "-statement-", s )

	def snippet( code: String, module: String = "-snippet-" ) =
	{
	val eval = new Evaluator
	val parser = new FunLParser( module )
	implicit val env = new eval.Environment

		parser.parseSource( new CharSequenceReader(code) ) match
		{
			case parser.Success( l, _ ) =>
				markTailRecursion( l )
				eval.enterActivation( null, null, eval.module(module) )
				eval.apply( l )
				eval.last
			case parser.Failure( m, r ) => PARSE_FAILURE( m )
			case parser.Error( m, r ) => PARSE_FAILURE( m )
		}
	}

	def snippetWithMargin( code: String, module: String = "-snippet-" ) = snippet( code.stripMargin, module )
	
	def expression( s: String, vs: (String, Any)* ) =
	{
	val eval = new Evaluator
	val parser = new FunLParser( "-expression-" )
	implicit val env = new eval.Environment

		parser.parseExpression( new CharSequenceReader(s) ) match
		{
			case parser.Success( l, _ ) =>
				eval.enterActivation( null, null, eval.module("-expression-") )
				eval.assign( "-expression-", vs: _* )
				eval.eval( l )
			case parser.Failure( m, r ) => PARSE_FAILURE( m )
			case parser.Error( m, r ) => PARSE_FAILURE( m )
		}
	}
}
