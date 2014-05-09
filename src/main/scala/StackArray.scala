/*     ______            __                                      *\
**    / ____/_  __ ___  / /     FunL Programming Language        **
**   / __/ / / / / __ \/ /      (c) 2014, Edward A. Maxedon, Sr. **
**  / /   / /_/ / / / / /__     http://funl-lang.org/            **
** /_/    \____/_/ /_/____/                                      **
\*                                                               */

package funl.interp

import collection.mutable.ArrayBuffer


class StackArray[T] extends ArrayBuffer[T]
{
	def push( a: T )
	{
		append( a )
	}

	def pop: T =
	{
	val res = top

		reduceToSize( size - 1 )
		res
	}

	def top =
	{
		require( !isEmpty, "stack underflow" )
		apply( size - 1 )
	}
}