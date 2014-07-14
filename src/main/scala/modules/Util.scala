/*     ______            __                                     *\
**    / ____/_  __ ___  / /     FunL Programming Language       **
**   / __/ / / / / __ \/ /      (c) 2014 Edward A. Maxedon, Sr. **
**  / /   / /_/ / / / / /__     http://funl-lang.org/           **
** /_/    \____/_/ /_/____/                                     **
\*                                                              */

package funl.modules

import util.Random.{nextInt, nextDouble}
import funl.interp.ArgList


object Util
{
	def rnd( a: Any ): Any =
		a match
		{
			case ArgList( Nil ) => nextDouble
			case ArgList( List(l: Int, u: Int) ) if l <= u => nextInt( u - l ) + l
			case n: Int => nextInt( n )
			case r: collection.immutable.Range => nextInt( r.last + 1 - r.start ) + r.start
		}
}
