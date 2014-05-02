package funl

// import scala.util.parsing.input.PagedSeqReader
// import scala.collection.immutable.PagedSeq
import util.parsing.input.CharSequenceReader
import io.Source
// import pickling._
// import binary._

import funl.options._

import funl.interp.{Parser, Evaluator, TailRecursion}


object InterpreterMain extends App
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
	
		Parser.parseSource( r ) match
		{
			case Parser.Success( l, _ ) =>
				TailRecursion( l )
				
				if (opts contains 'b)
				{
					println( "binary" )
				}
				else
					new Evaluator().assign( 'args -> args.toIndexedSeq )( l )
			case Parser.Failure( m, r ) => println( r.pos + ": " + m + '\n' + r.pos.longString )
			case Parser.Error( m, r ) => println( r.pos + ": " + m )
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