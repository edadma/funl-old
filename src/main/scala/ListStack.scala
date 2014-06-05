/*     ______            __                                      *\
**    / ____/_  __ ___  / /     FunL Programming Language        **
**   / __/ / / / / __ \/ /      (c) 2014 Edward A. Maxedon, Sr. **
**  / /   / /_/ / / / / /__     http://funl-lang.org/            **
** /_/    \____/_/ /_/____/                                      **
\*                                                               */

package funl.interp


class ListStack[T]
{
	private[interp] var stack: List[T] = Nil

	private def check = require( !stack.isEmpty, "stack empty" )
	
	def push( a: T )
	{
		stack = a :: stack
	}

	def pop: T =
	{
	val res = top
	
		stack = stack.tail
		res
	}

	def top =
	{
		check
		stack.head
	}

	def top( a: T )
	{
		check
		stack = a :: stack.tail
	}
	
	def copy =
	{
	val res = new ListStack[T]

		res.stack = stack
		res
	}

	def find( p: T => Boolean ) = stack find p

	def size = stack.size

	override def toString = stack.mkString( "ListStack( ", ", ", " )" )
}