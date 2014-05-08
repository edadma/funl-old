/*     ______            __                                      *\
**    / ____/_  __ ___  / /     FunL Programming Language        **
**   / __/ / / / /  _ \/ /      (c) 2014, Edward A. Maxedon, Sr. **
**  / /   / /_/ / / / / /__     http://funl-lang.org/            **
** /_/    \____/_/ /_/____/                                      **
\*                                                               */

package funl.interp

import collection.mutable.HashMap


class Module( val name: Symbol )
{
	val symbols = new HashMap[Symbol, Any]
}
