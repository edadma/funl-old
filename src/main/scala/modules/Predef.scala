/*     ______            __                                     *\
**    / ____/_  __ ___  / /     FunL Programming Language       **
**   / __/ / / / / __ \/ /      (c) 2014 Edward A. Maxedon, Sr. **
**  / /   / /_/ / / / / /__     http://funl-lang.org/           **
** /_/    \____/_/ /_/____/                                     **
\*                                                              */

package funl.modules

import collection.mutable.{HashSet, HashMap, ArrayBuffer, ArraySeq}
import util.Random.{nextInt, nextDouble}

import funl.interp.{Function, ArgList, RuntimeException}
import funl.interp.Interpreter._


object Predef
{
	def println( a: Any ) =
		a match
		{
			case ArgList( l ) => Console.println( l map (display(_)) mkString(", ") )
			case _ => Console.println( display(a) )
		}
	
	def print( a: Any ) =
		a match
		{
			case ArgList( l ) => Console.print( l map (display(_)) mkString(", ") )
			case _ => Console.print( display(a) )
		}


	def printf( a: Any ) =
		a match
		{
			case ArgList( (text: String) :: args ) => Console.printf( text, args: _* )
		}

	def format( a: Any ) =
		a match
		{
			case ArgList( (text: String) :: args ) => text format (args : _*)
		}

	def readLine( a: Any ) =
		a match
		{
			case ArgList( Nil ) => io.StdIn.readLine
			case ArgList( (text: String) :: args ) => io.StdIn.readLine( text, args: _* )
		}

	def error( a: Any ) {error( a.toString )}

	def error( msg: String )
	{
		Console.err.println( msg )
		sys.exit( 1 )
	}
	
	def assert( a: Any ) =
		a match
		{
			case ArgList( List(b: Boolean, s: String) ) => if (!b) error( s )
			case b: Boolean => if (!b) error( "assertion failed" )
		}

	def require( a: Any ) =
		a match
		{
			case b: Boolean => if (!b) error( "requirement failed" )
			case ArgList( List(b: Boolean, s: String) ) => if (!b) error( s )
		}

	def array( a: Any ) =
		a match
		{
			case n: Int => ArraySeq.fill[Any]( n )( null )
			case ArgList( List(n1: Int, n2: Int) ) => ArraySeq.fill[Any]( n1, n2 )( null )
			case init: Array[Any] => ArraySeq[Any]( init: _* )
			case init: Array[Byte] => ArraySeq[Any]( init: _* )
			case init: Array[Int] => ArraySeq[Any]( init: _* )
			case init: Seq[Seq[Any]] if !init.isEmpty && init.head.isInstanceOf[Seq[Any]] =>
				ArraySeq[Any]( (init map (e => ArraySeq[Any](e: _*))): _* )
			case init: Seq[Any] => ArraySeq[Any]( init: _* )
			case init: TraversableOnce[Any] => ArraySeq[Any]( init.toSeq: _* )
		}

	def vector( a: Any ) =
		a match
		{
			case ArgList( Nil ) => Vector()
			case ArgList( List(n: Int, f: (Int => Any)) ) => Vector.tabulate[Any]( n )( f )
			case ArgList( List(n1: Int, n2: Int, f: ((Int, Int) => Any)) ) => Vector.tabulate[Any]( n1, n2 )( f )
			case init: Array[Any] => Vector[Any]( init: _* )
			case init: Array[Byte] => Vector[Any]( init: _* )
			case init: Array[Int] => Vector[Any]( init: _* )
			case init: Seq[Seq[Any]] if !init.isEmpty && init.head.isInstanceOf[Seq[Any]] =>
				Vector[Any]( (init map (e => Vector[Any](e: _*))): _* )
			case init: Seq[Any] => Vector[Any]( init: _* )
			case init: TraversableOnce[Any] => Vector[Any]( init.toSeq: _* )
		}

	def seq( a: Any ) =
		a match
		{
			case ArgList( Nil ) => new ArrayBuffer[Any]
			case n: Int => ArrayBuffer.fill[Any]( n )( null )
			case init: Array[Any] => ArrayBuffer[Any]( init: _* )
			case init: Array[Byte] => ArrayBuffer[Any]( init: _* )
			case init: Array[Int] => ArrayBuffer[Any]( init: _* )
			case init: Seq[Seq[Any]] if !init.isEmpty && init.head.isInstanceOf[Seq[Any]] =>
				ArrayBuffer[Any]( (init map (e => ArrayBuffer[Any](e: _*))): _* )
			case init: Seq[Any] => ArrayBuffer[Any]( init: _* )
			case init: TraversableOnce[Any] => ArrayBuffer[Any]( init.toSeq: _* )
		}

	def list( a: Any ) =
		a match
		{
			case ArgList( Nil ) => Nil
			case init: Array[Any] => List[Any]( init: _* )
			case init: Array[Byte] => List[Any]( init: _* )
			case init: Array[Int] => List[Any]( init: _* )
			case init: Seq[Seq[Any]] if !init.isEmpty && init.head.isInstanceOf[Seq[Any]] =>
				List[Any]( (init map (e => List[Any](e: _*))): _* )
			case init: Seq[Any] => List[Any]( init: _* )
			case init: TraversableOnce[Any] => List[Any]( init.toSeq: _* )
		}

	def set( a: Any ) =
		a match
		{
			case ArgList( Nil ) => new HashSet[Any]
			case ArgList( l ) => HashSet( l: _* )
			case x: collection.Set[Any] => HashSet( x.toSeq: _* )
			case x: Seq[Any] => HashSet( x: _* )
			case _ => HashSet( a )
		}
		
	def dict( a: Any ) =
		a match
		{
			case ArgList( Nil ) => new HashMap[Any, Any]
			case m: collection.Map[Any, Any] => HashMap( m.toSeq: _* )
			case s: collection.TraversableOnce[Vector[Any]] => HashMap( s.toSeq.map(v => (v(0), v(1))): _* )
		}
		
	def tuple( a: Any ) =
		a match
		{
			case ArgList( Nil ) => Vector()
			case a: Array[_] => a.toVector
			case c: Iterable[_] => c.toVector
		}

	def int( a: Any ) =
		a match
		{
			case n: BigInt => n
			case d: BigDecimal => funl.lia.Math.maybeDemote( d.toBigInt )
			case n: Number => n.intValue
			case s: String => s.toInt
		}

	def float( a: Any ) =
		a match
		{
			case (_: Double) | (_: BigDecimal) => a
			case n: Number => n.doubleValue
			case s: String => s.toDouble
		}

	def bin( a: Any ) =
		a match
		{
			case n: BigInt => n.toString( 2 )
			case n: Int => Integer.toBinaryString( n )
		}

	def oct( a: Any ) =
		a match
		{
			case n: BigInt => n.toString( 8 )
			case n: Int => Integer.toOctalString( n )
		}

	def hex( a: Any ) =
		a match
		{
			case n: BigInt => n.toString( 16 )
			case n: Int => Integer.toHexString( n )
		}

	def chr( code: Int ) = code.toChar.toString

	def ord( ch: String ) = ch.head.toInt

	def isalpha( ch: String ) = ch.head.toChar.isLetter

	def isupper( ch: String ) = ch.head.toChar.isUpper

	def islower( ch: String ) = ch.head.toChar.isLower

	private val INTEGER = """-?(?:0|[1-9]\d*)""".r.pattern
	
	private val DECIMAL = """-?(?:(?:0|[1-9]\d*)(?:\.\d*)?|\.\d+)(?:(?:e|E)(?:\+|-|)\d+)?""".r.pattern

	def number( s: String ) =
		if (INTEGER.matcher( s ).matches)
			Some( s.toInt )
		else if (DECIMAL.matcher( s ).matches)
			Some( s.toDouble )
		else
			None

	def sum( t: TraversableOnce[Any] ) =
	{
	var res: Any = 0

		for (a <- t)
			res = funl.lia.Math( '+, res, a )

		res
	}

	def none = None
	
	def rnd( a: Any ): Any =
		a match
		{
			case ArgList( Nil ) => nextDouble
			case ArgList( List(l: Int, u: Int) ) if l <= u => nextInt( u - l ) + l
			case n: Int => nextInt( n )
			case r: collection.immutable.Range => nextInt( r.last + 1 - r.start ) + r.start
		}

	def eval( a: Any ): Any =
		a match
		{
			case s: String => expression( s )
		}
		
		
		
// Scala interop: implicit Ordering

// 	def min( a: List[Any] ) =
// 		a match
// 		{
// 			case List( c: Iterable[Any] ) => c.min
// 		}
}
