/*     ______            __                                     *\
**    / ____/_  __ ___  / /     FunL Programming Language       **
**   / __/ / / / / __ \/ /      (c) 2014 Edward A. Maxedon, Sr. **
**  / /   / /_/ / / / / /__     http://funl-lang.org/           **
** /_/    \____/_/ /_/____/                                     **
\*                                                              */

package funl.modules

import collection.mutable.{HashSet, HashMap, ArrayBuffer, ArraySeq}

import funl.interp.Function
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

	def format( a: List[Any] ) =
		a match
		{
			case (text: String) :: args => text format (args : _*)
		}

	def readLine( a: List[Any] ) =
		a match
		{
			case Nil => io.StdIn.readLine
			case (text: String) :: args => io.StdIn.readLine( text, args: _* )
		}

	def error( a: List[Any] ) {error( a.head.toString )}

	def error( msg: String )
	{
		throw new RuntimeException( msg )
// 		Console.err.println( msg )
// 		sys.exit( 1 )
	}
	
	def assert( a: List[Any] ) =
		a match
		{
			case List( b: Boolean ) => if (!b) error( "assertion failed" )
			case List( b: Boolean, s: String ) => if (!b) error( s )
		}

	def require( a: List[Any] ) =
		a match
		{
			case List( b: Boolean ) => if (!b) error( "requirement failed" )
			case List( b: Boolean, s: String ) => if (!b) error( s )
		}

	def array( a: List[Any] ) =
		a match
		{
			case List( n: Int ) => ArraySeq.fill[Any]( n )( null )
			case List( n1: Int, n2: Int ) => ArraySeq.fill[Any]( n1, n2 )( null )
			case List( init: Array[Any] ) => ArraySeq[Any]( init: _* )
			case List( init: Array[Byte] ) => ArraySeq[Any]( init: _* )
			case List( init: Array[Int] ) => ArraySeq[Any]( init: _* )
			case List( init: Seq[Seq[Any]] ) if !init.isEmpty && init.head.isInstanceOf[Seq[Any]] =>
				ArraySeq[Any]( (init map (e => ArraySeq[Any](e: _*))): _* )
			case List( init: Seq[Any] ) => ArraySeq[Any]( init: _* )
			case List( init: Iterable[Any] ) => ArraySeq[Any]( init.toSeq: _* )
		}

	def vector( a: List[Any] ) =
		a match
		{
// 			case List( n: Int, f: Function ) => Vector.fill[Any]( n )( null )
// 			case List( n1: Int, n2: Int, f: Function ) => Vector.fill[Any]( n1, n2 )( null )
			case List( init: Array[Any] ) => Vector[Any]( init: _* )
			case List( init: Array[Byte] ) => Vector[Any]( init: _* )
			case List( init: Array[Int] ) => Vector[Any]( init: _* )
			case List( init: Seq[Seq[Any]] ) if !init.isEmpty && init.head.isInstanceOf[Seq[Any]] =>
				Vector[Any]( (init map (e => Vector[Any](e: _*))): _* )
			case List( init: Seq[Any] ) => Vector[Any]( init: _* )
			case List( init: Iterable[Any] ) => Vector[Any]( init.toSeq: _* )
		}

	def seq( a: List[Any] ) =
		a match
		{
			case Nil => new ArrayBuffer[Any]
//			case List( n: Int ) => ArrayBuffer.fill[Any]( n )( null )
			case List( init: Array[Any] ) => ArrayBuffer[Any]( init: _* )
			case List( init: Array[Byte] ) => ArrayBuffer[Any]( init: _* )
			case List( init: Array[Int] ) => ArrayBuffer[Any]( init: _* )
			case List( init: Seq[Any] ) => ArrayBuffer[Any]( init: _* )
			case List( init: Iterable[Any] ) => ArrayBuffer[Any]( init.toSeq: _* )
			case _ => ArrayBuffer[Any]( a: _* )
		}

	def list( a: List[Any] ) =
		a match
		{
			case Nil => Nil
			case List( init: Array[Any] ) => List[Any]( init: _* )
			case List( init: Array[Byte] ) => List[Any]( init: _* )
			case List( init: Array[Int] ) => List[Any]( init: _* )
			case List( init: Seq[Any] ) => List[Any]( init: _* )
			case List( init: Iterable[Any] ) => List[Any]( init.toSeq: _* )
		}

	def set( a: List[Any] ) =
		if (a isEmpty)
			new HashSet[Any]
		else if (a.head.isInstanceOf[collection.Set[Any]])
			HashSet( a.head.asInstanceOf[collection.Set[Any]].toSeq: _* )
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
			case List( n: BigInt ) => n
			case List( n: Number ) => n.intValue
			case List( s: String ) => s.toInt
		}

	def bin( a: List[Any] ) =
		a match
		{
			case List( n: BigInt ) => n.toString( 2 )
			case List( n: Int ) => Integer.toBinaryString( n )
		}

	def oct( a: List[Any] ) =
		a match
		{
			case List( n: BigInt ) => n.toString( 8 )
			case List( n: Int ) => Integer.toOctalString( n )
		}

	def hex( a: List[Any] ) =
		a match
		{
			case List( n: BigInt ) => n.toString( 16 )
			case List( n: Int ) => Integer.toHexString( n )
		}

  def chr( code: Int ) = code.toChar.toString

  def ord( ch: String ) = ch.head.toInt

  def isalpha( ch: String ) = ch.head.toChar.isLetter

  def isupper( ch: String ) = ch.head.toChar.isUpper

  def islower( ch: String ) = ch.head.toChar.isLower

  def sum( t: TraversableOnce[Any] ) =
  {
	var res: Any = 0

		for (a <- t)
			res = funl.lia.Math( '+, res, a )

		res
  }

  def none = None

// Scala interop: implicit Ordering

// 	def min( a: List[Any] ) =
// 		a match
// 		{
// 			case List( c: Iterable[Any] ) => c.min
// 		}
}
