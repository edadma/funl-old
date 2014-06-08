/*     ______            __                                     *\
**    / ____/_  __ ___  / /     FunL Programming Language       **
**   / __/ / / / / __ \/ /      (c) 2014 Edward A. Maxedon, Sr. **
**  / /   / /_/ / / / / /__     http://funl-lang.org/           **
** /_/    \____/_/ /_/____/                                     **
\*                                                              */

package funl.modules

import util.Random.{nextInt, nextDouble}


object Util
{
	def rnd( a: List[Any] ): Any =
		a match
		{
			case Nil => nextDouble
			case List( n: Int ) => nextInt( n )
			case List( l: Int, u: Int ) if l <= u => nextInt( u - l ) + l
			case List( r: collection.immutable.Range ) => nextInt( r.last + 1 - r.start ) + r.start
		}

	def sleep( a: List[Any] ) =
		a match
		{
			case List( n: Int ) => Thread.sleep( n )
		}
}