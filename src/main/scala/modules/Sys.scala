/*     ______            __                                     *\
**    / ____/_  __ ___  / /     FunL Programming Language       **
**   / __/ / / / / __ \/ /      (c) 2014 Edward A. Maxedon, Sr. **
**  / /   / /_/ / / / / /__     http://funl-lang.org/           **
** /_/    \____/_/ /_/____/                                     **
\*                                                              */

package funl.modules

import sys.process._


object Sys
{
	def execute( a: Any ) =
		a match
		{
			case List( cmd: String ) => cmd!
		}
		
	def executeReturnString( a: Any ) =
		a match
		{
			case List( cmd: String ) => cmd!!
		}
}