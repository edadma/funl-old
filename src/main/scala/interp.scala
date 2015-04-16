/*     ______            __                                     *\
**    / ____/_  __ ___  / /     FunL Programming Language       **
**   / __/ / / / / __ \/ /      (c) 2014 Edward A. Maxedon, Sr. **
**  / /   / /_/ / / / / /__     http://funl-lang.org/           **
** /_/    \____/_/ /_/____/                                     **
\*                                                              */

package funl

import ca.hyperreal.lia.Math


package object interp
{
	type SymbolMap = collection.immutable.Map[String, Any]
	
	def symbolMap: SymbolMap = collection.immutable.HashMap.empty
	
	type Function = Any => Any
	
	case class ArgList( args: List[Any] )
    
	type LSeq = collection.LinearSeq[Any]

	trait SymbolMapContainer
	{
		def apply( key: String ): Any
		
		def update( key: String, value: Any ): Unit

		def contains( key: String ): Boolean

		def remove( key: String ): Unit

		def get( key: String ): Option[Any]

		def clear: Unit
	}
}
