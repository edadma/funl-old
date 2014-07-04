/*     ______            __                                      *\
**    / ____/_  __ ___  / /     FunL Programming Language        **
**   / __/ / / / / __ \/ /      (c) 2014 Edward A. Maxedon, Sr. **
**  / /   / /_/ / / / / /__     http://funl-lang.org/            **
** /_/    \____/_/ /_/____/                                      **
\*                                                               */

package funl

import funl.interp.Evaluator
import funl.interp.Interpreter._


object TestMain extends App
{
	print( executeCaptureOutput( "test/t1", Some("-main-") ).trim == "123" )
}
