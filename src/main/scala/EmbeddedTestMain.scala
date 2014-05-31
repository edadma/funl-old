/*     ______            __                                      *\
**    / ____/_  __ ___  / /     FunL Programming Language        **
**   / __/ / / / / __ \/ /      (c) 2014, Edward A. Maxedon, Sr. **
**  / /   / /_/ / / / / /__     http://funl-lang.org/            **
** /_/    \____/_/ /_/____/                                      **
\*                                                               */

package funl

import funl.interp.Interpreter._


object EmbeddedTestMain extends App
{
	println( snippetWithMargin( """	|a = 2 + 3
																	|b = 'asdf'
																	|(a, b)
																	""") )
}