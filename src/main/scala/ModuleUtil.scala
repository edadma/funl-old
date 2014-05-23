/*     ______            __                                      *\
**    / ____/_  __ ___  / /     FunL Programming Language        **
**   / __/ / / / / __ \/ /      (c) 2014, Edward A. Maxedon, Sr. **
**  / /   / /_/ / / / / /__     http://funl-lang.org/            **
** /_/    \____/_/ /_/____/                                      **
\*                                                               */

package funl.modules

import util.Random.{nextInt, nextDouble}

import funl.interp.Interpreter._


object ModuleUtil
{
	def rnd( a: Vector[Any] ): Any =
		a match
		{
			case NIL => nextDouble
			case Vector( n: Int ) => nextInt( n )
			case Vector( l: Int, u: Int ) if l <= u => nextInt( u - l ) + l
			case Vector( r: collection.immutable.Range ) => nextInt( r.last + 1 - r.start ) + r.start
		}
}