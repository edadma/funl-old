/*     ______            __                                      *\
**    / ____/_  __ ___  / /     FunL Programming Language        **
**   / __/ / / / / __ \/ /      (c) 2014, Edward A. Maxedon, Sr. **
**  / /   / /_/ / / / / /__     http://funl-lang.org/            **
** /_/    \____/_/ /_/____/                                      **
\*                                                               */

package funl.interp

import collection.mutable.HashSet


class Module( val name: String ) extends Types
{
	val symbols = new SymbolMap
	val datatypes = new HashSet[String]
	val exports = new HashSet[String]

	def apply( key: String ) = synchronized (symbols( key ))
	
	def get( key: String ) = synchronized (symbols get key)

	def contains( key: String ) = synchronized (symbols contains key)

	def iterator = synchronized (symbols.iterator)
}