/*     ______            __                                      *\
**    / ____/_  __ ___  / /     FunL Programming Language        **
**   / __/ / / / / __ \/ /      (c) 2014, Edward A. Maxedon, Sr. **
**  / /   / /_/ / / / / /__     http://funl-lang.org/            **
** /_/    \____/_/ /_/____/                                      **
\*                                                               */

package funl

import collection.mutable.{HashSet, HashMap, ArrayBuffer}
import util.Random.{nextInt, nextDouble}

import interp.Interpreter._


object Predef
{
	private val NIL = Vector.empty[Any]
	
	def println( a: Vector[Any] ) = Console.println( a map (display(_)) mkString(", ") )
	
	def print( a: Vector[Any] ) = Console.print( a map (display(_)) mkString(", ") )

	def printf( a: Vector[Any] ) = Console.printf( a.head.asInstanceOf[String], a.tail: _* )

	def readLine( a: Vector[Any] ) = Console.readLine
	
	def error( a: Vector[Any] ) {error( a.head.toString )}

	def error( msg: String )
	{
		Console.err.println( msg )
		sys.exit( 1 )
	}
	
	def require( a: Vector[Any] ) =
		if (!a.head.asInstanceOf[Boolean])
			error( a.last.toString )

	def list( a: Vector[Any] ) =
		a match
		{
			case NIL => new ArrayBuffer[Any]
			case Vector( n: Int ) => ArrayBuffer.fill[Any]( n )( null )
			case Vector( init: Seq[Any] ) => ArrayBuffer[Any]( init: _* )
		}

	def set( a: Vector[Any] ) =
		if (a isEmpty)
			new HashSet[Any]
		else if (a.head.isInstanceOf[collection.Set[Any]])
			HashSet( a.head.asInstanceOf[collection.Set[Any]].toArray: _* )
		else
			HashSet( a: _* )
	
	def dict( a: Vector[Any] ) =
		if (a isEmpty)
			new HashMap[Any, Any]
		else
			HashMap( a.head.asInstanceOf[collection.Map[Any, Any]].toArray: _* )

	def tuple( a: Vector[Any] ) =
		a match
		{
			case NIL => ()
			case Vector( c: Iterable[_] ) => c.toVector
			case Vector( a: Array[_] ) => a.toVector
		}
		
	def rnd( a: Vector[Any] ): Any =
		a match
		{
			case NIL => nextDouble
			case Vector( n: Int ) => nextInt( n )
			case Vector( l: Int, u: Int ) if l <= u => nextInt( u - l ) + l
			case Vector( r: collection.immutable.Range ) => nextInt( r.last + 1 - r.start ) + r.start
		}
}