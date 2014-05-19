/*     ______            __                                      *\
**    / ____/_  __ ___  / /     FunL Programming Language        **
**   / __/ / / / / __ \/ /      (c) 2014, Edward A. Maxedon, Sr. **
**  / /   / /_/ / / / / /__     http://funl-lang.org/            **
** /_/    \____/_/ /_/____/                                      **
\*                                                               */

package funl.interp


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

trait ReadOnlyReference extends Reference
{
	def name: String

	def assign( v: Any ) = RuntimeException( name + " is read-only" )
}

class ConstantReference( val name: String, val value: Any ) extends ReadOnlyReference

class ByNameReference( val name: String )( thunk: => Any ) extends ReadOnlyReference
{
	def value = thunk
}

class SystemReference( sysvar: String )( thunk: => Any ) extends ByNameReference( "$" + sysvar )( thunk )