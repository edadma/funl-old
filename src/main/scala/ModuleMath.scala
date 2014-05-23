/*     ______            __                                      *\
**    / ____/_  __ ___  / /     FunL Programming Language        **
**   / __/ / / / / __ \/ /      (c) 2014, Edward A. Maxedon, Sr. **
**  / /   / /_/ / / / / /__     http://funl-lang.org/            **
** /_/    \____/_/ /_/____/                                      **
\*                                                               */

package funl.modules

import funl.lia.{Complex, Math}
import funl.interp.Interpreter._


object ModuleMath
{
	def sqrt( a: Vector[Any] ) =
		a match
		{
			case Vector( n: Number ) => Math.squareRoot( n )
		}
}