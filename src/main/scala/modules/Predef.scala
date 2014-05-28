/*     ______            __                                      *\
**    / ____/_  __ ___  / /     FunL Programming Language        **
**   / __/ / / / / __ \/ /      (c) 2014, Edward A. Maxedon, Sr. **
**  / /   / /_/ / / / / /__     http://funl-lang.org/            **
** /_/    \____/_/ /_/____/                                      **
\*                                                               */

package funl.modules

import collection.mutable.{HashSet, HashMap, ArrayBuffer}

import funl.interp.Interpreter._
import funl.interp.RuntimeException


object Predef
{
	def println( a: List[Any] ) = Console.println( a map (display(_)) mkString(", ") )
	
	def print( a: List[Any] ) = Console.print( a map (display(_)) mkString(", ") )

	def printf( a: List[Any] ) =
		a match
		{
			case (text: String) :: args => Console.printf( text, args: _* )
		}

	def readLine( a: List[Any] ) =
		a match
		{
			case Nil => Console.readLine
			case (text: String) :: args => Console.readLine( text, args: _* )
		}

	def error( a: List[Any] ) {error( a.head.toString )}

	def error( msg: String )
	{
		throw new RuntimeException( msg )
// 		Console.err.println( msg )
// 		sys.exit( 1 )
	}
	
	def require( a: List[Any] ) =
		if (!a.head.asInstanceOf[Boolean])
			error( a.last.toString )

	def array( a: List[Any] ) =
		a match
		{
			case Nil => new ArrayBuffer[Any]
			case List( n: Int ) => ArrayBuffer.fill[Any]( n )( null )
			case List( init: Array[Any] ) => ArrayBuffer[Any]( init: _* )
			case List( init: Seq[Any] ) => ArrayBuffer[Any]( init: _* )
		}

	def list( a: List[Any] ) =
		a match
		{
			case Nil => Nil
			case List( init: Array[Any] ) => List[Any]( init: _* )
			case List( init: Seq[Any] ) => List[Any]( init: _* )
		}

	def set( a: List[Any] ) =
		if (a isEmpty)
			new HashSet[Any]
		else if (a.head.isInstanceOf[Seq[Any]])
			HashSet( a.head.asInstanceOf[Seq[Any]]: _* )
		else
			HashSet( a: _* )
	
	def dict( a: List[Any] ) =
		if (a isEmpty)
			new HashMap[Any, Any]
		else
			HashMap( a.head.asInstanceOf[collection.Map[Any, Any]].toSeq: _* )

	def tuple( a: List[Any] ) =
		a match
		{
			case Nil => Vector()
			case List( c: Iterable[_] ) => c.toVector
			case List( a: Array[_] ) => a.toVector
		}

	def int( a: List[Any] ) =
		a match
		{
			case List( n: Number ) => n.intValue
			case List( s: String ) => s.toInt
		}
}