/*     ______            __                                      *\
**    / ____/_  __ ___  / /     FunL Programming Language        **
**   / __/ / / / / __ \/ /      (c) 2014, Edward A. Maxedon, Sr. **
**  / /   / /_/ / / / / /__     http://funl-lang.org/            **
** /_/    \____/_/ /_/____/                                      **
\*                                                               */

package funl.interp

import java.io.{File, InputStream, FileInputStream}

// import scala.util.parsing.input.PagedSeqReader
// import scala.collection.immutable.PagedSeq
import util.parsing.input.{Reader, CharSequenceReader}
import io.Source


object Interpreter
{
	val PREDEF = "-predef-"
	val VERSION = "0.7-SNAPSHOT"
	
	def markTailRecursion( m: ModuleAST )
	{
		for (s <- m.statements)
			s match
			{
				case DeclStatementAST( decls ) =>
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
					case CaseFunctionExprAST( _, cases ) =>
						for (c <- cases)
							markTailRecursion( n, c.parts.head.body )
					case FunctionExprAST( _, _, parts ) =>
						markTailRecursion( n, parts.head.body )
					case _ =>
				}
			case ConditionalExprAST( cond, no ) =>
				for ((_, thenpart) <- cond)
					markTailRecursion( n, thenpart )

				if (no != None)
					markTailRecursion( n, no.get )
			case BooleanConnectiveExprAST( _, _, right ) =>
				markTailRecursion( n, right )
			case _ =>
		}
	}

	def displayQuoted( a: Any ): String =
		a match
		{
			case s: String => "'" + s + "'"
			case _ => display( a )
		}

	def display( a: Any ): String =
		a match
		{
			case a: Array[_] => a.map( display(_) ).mkString( "Array(", ", ", ")" )
			case l: List[_] => l.map( display(_) ).mkString( "[", ", ", "]" )
			case s: collection.Set[_] => s.map( display(_) ).mkString( "{", ", ", "}" )
			case m: collection.Map[_, _] => m.toList.map( {case (k, v) => displayQuoted(k) + ": " + display(v)} ).mkString( "{", ", ", "}" )
			case t: Vector[_] => t.map( display(_) ).mkString( "(", ", ", ")" )
			case _ => String.valueOf( a )
		}

	def parse( module: String, input: String ): AST = parse( module, new CharSequenceReader(input) )

	def parse( module: String, input: Reader[Char] ): AST =
	{
	val parser = new Parser( module )

		parser.parseSource( input ) match
		{
			case parser.Success( l, _ ) =>
				markTailRecursion( l )
//			println( l )
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

//		val r = new PagedSeqReader( PagedSeq fromFile (args.head + ".fun") )
	def parse( module: String ): AST =
	{
	val filename = module + ".funl"
	val resource = funl.Main.getClass.getResourceAsStream( filename )
	val input =
		if (resource eq null)
		{
		val file = new File( filename )

			if (!file.exists || !file.isFile)
				sys.error( "module '" + module + "' not found" )
			else
				new FileInputStream( file )
		}
		else
			resource
	
		parse( module, input )
	}

	case class PARSE_FAILURE( message: String )

	def statement( m: String, s: String ): Any =
	{
	val eval = new Evaluator
	implicit val env = new eval.Environment
	
		eval.enterActivation( null, eval.module(m) )
		statement( m, s, eval )
	}

	def statement( m: String, s: String, eval: Evaluator )( implicit env: eval.Environment ) =
	{
	val parser = new Parser( m )

		parser.parseStatement( new CharSequenceReader(s) ) match
		{
			case parser.Success( l, _ ) =>
				eval.apply( l )
				eval.last
			case parser.Failure( m, r ) => PARSE_FAILURE( m )
			case parser.Error( m, r ) => PARSE_FAILURE( m )
		}
	}

	def statement( s: String ): Any = statement( "module", s )

	def snippet( s: String ) =
	{
	val eval = new Evaluator
	val parser = new Parser( "module" )
	implicit val env = new eval.Environment

		parser.parseSnippet( new CharSequenceReader(s) ) match
		{
			case parser.Success( l, _ ) =>
				eval.enterActivation( null, eval.module("module") )
				eval.eval( l )
			case parser.Failure( m, r ) => PARSE_FAILURE( m )
			case parser.Error( m, r ) => PARSE_FAILURE( m )
		}
	}

	def expression( s: String, vs: (String, Any)* ) =
	{
	val eval = new Evaluator
	val parser = new Parser( "module" )
	implicit val env = new eval.Environment

		parser.parseExpression( new CharSequenceReader(s) ) match
		{
			case parser.Success( l, _ ) =>
				eval.enterActivation( null, eval.module("module") )
				eval.assign( "module", vs: _* )
				eval.eval( l )
			case parser.Failure( m, r ) => PARSE_FAILURE( m )
			case parser.Error( m, r ) => PARSE_FAILURE( m )
		}
	}
}