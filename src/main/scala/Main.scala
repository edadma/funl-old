/*     ______            __                                      *\
**    / ____/_  __ ___  / /     FunL Programming Language        **
**   / __/ / / / / __ \/ /      (c) 2014 Edward A. Maxedon, Sr. **
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
		Options( args, 'args -> "", 'path -> "." )
		{
	//		case "-b" :: t => ('b -> "set", t)
			case "-a" :: a :: t => ('args -> a, t)
			case "-p" :: p :: t => ('path -> p, t)
			case o :: _ if o startsWith "-" => sys.error( "bad option: " + o )
			case f :: t => ('input -> f, t)
		}

	modulePath = opts('path).split( ";" ).toList
	
	if (!(opts contains 'input))
		REPL.main( args )
	else
	{
	val m = opts('input)
  
		execute( m, Some("-main-"), "args" -> opts('args).split(" +").toVector )
	}
}
