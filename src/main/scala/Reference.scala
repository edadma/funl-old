/*     ______            __                                      *\
**    / ____/_  __ ___  / /     FunL Programming Language        **
**   / __/ / / / / __ \/ /      (c) 2014, Edward A. Maxedon, Sr. **
**  / /   / /_/ / / / / /__     http://funl-lang.org/            **
** /_/    \____/_/ /_/____/                                      **
\*                                                               */

package funl.interp

import collection.mutable.{Seq => MutableSeq, Map => MutableMap}
import collection.immutable.{Seq => ImmutableSeq, Map => ImmutableMap}


trait Reference
{
	def value: Any

	def assign( v: Any )
}

class VariableReference( init: Any ) extends Reference
{
	def this() = this( null )
	
	private var _value: Any = init

	def value = _value

	def assign( v: Any ) = _value = v
}

class MutableSeqReference( seq: MutableSeq[Any], index: Int ) extends Reference
{
	def value = seq( index )

	def assign( v: Any ) = seq(index) = v
}

class MutableMapReference( map: MutableMap[Any, Any], key: Any ) extends Reference
{
	def value = map( key )

	def assign( v: Any ) = map(key) = v
}

trait ReadOnlyReference extends Reference
{
	def name: String

	def assign( v: Any ) = RuntimeException( name + " is read-only" )
}

class ConstantReference( val name: String, val value: Any ) extends ReadOnlyReference

class ImmutableSeqReference( seq: ImmutableSeq[Any], index: Int ) extends ReadOnlyReference
{
	val name = "tried to assign to index " + index + ", but sequence"
	
	def value = seq( index )
}

class ImmutableMapReference( map: ImmutableMap[Any, Any], key: Any ) extends ReadOnlyReference
{
	val name = "tried to assign to key '" + key + "', but map"

	def value = map( key )
}

class ByNameReference( val name: String )( thunk: => Any ) extends ReadOnlyReference
{
	def value = thunk
}

class SystemReference( sysvar: String )( thunk: => Any ) extends ByNameReference( "$" + sysvar )( thunk )