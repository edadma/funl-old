/*     ______            __                                      *\
**    / ____/_  __ ___  / /     FunL Programming Language        **
**   / __/ / / / / __ \/ /      (c) 2014, Edward A. Maxedon, Sr. **
**  / /   / /_/ / / / / /__     http://funl-lang.org/            **
** /_/    \____/_/ /_/____/                                      **
\*                                                               */

package funl

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
	val m = opts('input)
	val l = parse( m )
	
		if (opts contains 'b)
		{
			println( "binary" )
		}
		else
			new Evaluator().assign( m, "args" -> args.toIndexedSeq )( l )
	}
}