package funl

// import scala.util.parsing.input.PagedSeqReader
// import scala.collection.immutable.PagedSeq
import util.parsing.input.CharSequenceReader
import io.Source
// import pickling._
// import binary._

import funl.interp.Evaluator
import funl.interp.Interpreter._


object Main extends App
{
	val opts =
		Options( args )
		{
			case "-b" :: t => ('b -> "set", t)
			case o :: _ if o startsWith "-" => sys.error( "bad option: " + o )
			case f :: t => ('input -> f, t)
		}

	if (opts isEmpty)
		REPL.main( args )
	else
	{
	val r = reader( opts('input) )
	val m = Symbol(opts('input))
	val parser = new FunLParser( m )
	
		parser.parseSource( r ) match
		{
			case parser.Success( l, _ ) =>
				markTailRecursion( l )
				
				if (opts contains 'b)
				{
					println( "binary" )
				}
				else
					new Evaluator().assign( m, 'args -> args.toIndexedSeq )( l )
			case parser.Failure( m, r ) => println( r.pos + ": " + m + '\n' + r.pos.longString )
			case parser.Error( m, r ) => println( r.pos + ": " + m )
		}
	}

	def reader( file: String ) =
	{
//		val r = new PagedSeqReader( PagedSeq fromFile (args.head + ".fun") )
	val filename = file + (if (file endsWith ".funl") "" else ".funl")
	val lines = Source.fromFile( filename ).getLines
	val source = new StringBuilder

			for (l <- lines)
			{
				source append l
				source append '\n'
			}

		new CharSequenceReader( source )
	}
}