/*     ______            __                                      *\
**    / ____/_  __ ___  / /     FunL Programming Language        **
**   / __/ / / / / __ \/ /      (c) 2014, Edward A. Maxedon, Sr. **
**  / /   / /_/ / / / / /__     http://funl-lang.org/            **
** /_/    \____/_/ /_/____/                                      **
\*                                                               */

package funl

import collection.mutable.{HashSet, HashMap}

import interp.Interpreter._


object Predef
{
	def println( a: List[Any] ) = Console.println( a map (display(_)) mkString(", ") )
	
	def print( a: List[Any] ) = Console.print( a map (display(_)) mkString(", ") )

	def printf( a: List[Any] ) = Console.printf( a.head.asInstanceOf[String], a.tail: _* )

	def error( a: List[Any] ) {error( a.head.toString )}

	def error( msg: String )
	{
		Console.err.println( msg )
		sys.exit( 1 )
	}
	
	def require( a: List[Any] ) =
		if (!a.head.asInstanceOf[Boolean])
			error( a.last.toString )
			
	def Array( a: List[Any] ) = new scala.Array[Any]( a.head.asInstanceOf[Int] )

	def Set( a: List[Any] ) =
		if (a isEmpty)
			new HashSet[Any]
		else if (a.head.isInstanceOf[collection.Set[Any]])
			HashSet( a.head.asInstanceOf[collection.Set[Any]].toArray: _* )
		else
			HashSet( a: _* )
	
	def Map( a: List[Any] ) = 
		if (a isEmpty)
			new HashMap[Any, Any]
		else
			HashMap( a.head.asInstanceOf[collection.Map[Any, Any]].toArray: _* )
}