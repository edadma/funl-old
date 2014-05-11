/*     ______            __                                      *\
**    / ____/_  __ ___  / /     FunL Programming Language        **
**   / __/ / / / / __ \/ /      (c) 2014, Edward A. Maxedon, Sr. **
**  / /   / /_/ / / / / /__     http://funl-lang.org/            **
** /_/    \____/_/ /_/____/                                      **
\*                                                               */

package funl.interp

import collection.mutable.ArrayStack


class Activation( val closure: Closure, val module: Module ) extends Types
{
	val scope = new ArrayStack[SymbolMap]

	override def toString = "Activation( " + closure + ", " + scope + " )"
}