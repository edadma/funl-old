/*     ______            __                                      *\
**    / ____/_  __ ___  / /     FunL Programming Language        **
**   / __/ / / / / __ \/ /      (c) 2014, Edward A. Maxedon, Sr. **
**  / /   / /_/ / / / / /__     http://funl-lang.org/            **
** /_/    \____/_/ /_/____/                                      **
\*                                                               */

package funl.interp


trait Reference
{
	def name: String
	
	def get: Any

	def set( v: Any )
}

trait ReadOnlyReference extends Reference
{
	def set( v: Any ) = sys.error( name + " is read-only" )
}

class ByNameReference( val name: String )( thunk: => Any ) extends ReadOnlyReference
{
	def get = thunk
}

class SystemReference( sysvar: String )( thunk: => Any ) extends ByNameReference( "$" + sysvar )( thunk )