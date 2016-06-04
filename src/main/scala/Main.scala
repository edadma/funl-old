/*     ______            __                                     *\
**    / ____/_  __ ___  / /     FunL Programming Language       **
**   / __/ / / / / __ \/ /      (c) 2014 Edward A. Maxedon, Sr. **
**  / /   / /_/ / / / / /__     http://funl-lang.org/           **
** /_/    \____/_/ /_/____/                                     **
\*                                                              */

package funl

// import pickling._
// import binary._

import funl.interp.Evaluator
import funl.interp.Interpreter._

import xyz.hyperreal.options.Options


object Main extends App
{
	val opts = new Options( Nil, List("-p"), Nil, "-p" -> "." )
	val moduleArgs = opts parse args
	
	modulePath = opts("-p").split( ";" ).toList
	
	if (moduleArgs isEmpty)
		REPL.main( args )
	else
	{
	val m = moduleArgs.head
  
		execute( m, Some("-main-"), "args" -> moduleArgs.tail.toVector )
	}
}
