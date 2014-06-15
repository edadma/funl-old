/*     ______            __                                     *\
**    / ____/_  __ ___  / /     FunL Programming Language       **
**   / __/ / / / / __ \/ /      (c) 2014 Edward A. Maxedon, Sr. **
**  / /   / /_/ / / / / /__     http://funl-lang.org/           **
** /_/    \____/_/ /_/____/                                     **
\*                                                              */

package funl.interp

import collection.mutable.{Seq => MutableSeq, Map => MutableMap, Buffer, ArrayBuffer}
import collection.immutable.{Seq => ImmutableSeq, Map => ImmutableMap}


trait Reference
{
	def value: Any

	def assign( v: Any )
}

abstract class RangeReference extends Reference
{
	protected def end( r: Range ) =
		if (r.isInclusive)
			r.end + 1
    else
			r.end

	protected def slice( seq: Seq[Any], range: Range ) =
		if (range.step == 1)
			seq.slice( range.start, end(range) )
		else
			for ((e, i) <- seq.zipWithIndex if range contains i)
				yield e
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

	def assign( v: Any ) =
	{
		seq match
		{
			case buf: Buffer[Any] if buf.length <= index =>
				buf.appendAll( Iterator.fill(index - buf.length + 1)(null) )
			case _ =>
		}
		
		seq(index) = v
	}
}

abstract class SeqRangeReference( seq: Seq[Any], range: Range ) extends RangeReference
{
	def value = slice( seq, range )
}

class MutableSeqRangeReference( seq: MutableSeq[Any], range: Range ) extends SeqRangeReference( seq, range )
{
  def assign( v: Any ) =
  {
		seq match
		{
			case buf: Buffer[Any] if buf.length < end( range ) =>
				buf.appendAll( Iterator.fill(end(range) - buf.length)(null) )
			case _ =>
		}
		
		v match
		{
			case s: Seq[Any] =>
				for ((e, i) <- s zip range)
					seq(i) = e
		}
	}
}

class Mutable2DSeqReference( seq: MutableSeq[MutableSeq[Any]], row: Int, col: Int ) extends Reference
{
	def value = seq( row )( col )

	def assign( v: Any ) = seq(row)(col) = v
}

abstract class SeqRowRangeReference( seq: Seq[Seq[Any]], row: Range, col: Int ) extends Reference
{
	protected val end =
		if (row.isInclusive)
			row.end + 1
    else
			row.end

	def value =
		for ((r, i) <- seq.zipWithIndex if row contains i)
			yield r(col)
}

class MutableSeqRowRangeReference( seq: MutableSeq[MutableSeq[Any]], row: Range, col: Int ) extends SeqRowRangeReference( seq, row, col ) with Reference
{
	def assign( v: Any ) =
  {
		seq match
		{
			case buf: Buffer[MutableSeq[Any]] if buf.length < end =>
				buf.appendAll( Iterator.fill(end - buf.length)(ArrayBuffer.fill(seq.head.length)(null)) )
			case _ =>
		}

		v match
		{
			case s: Seq[Any] =>
				for ((r, i) <- s zip row)
					seq(i)(col) = r
		}
	}
}

abstract class SeqColumnRangeReference( seq: Seq[Seq[Any]], row: Int, col: Range ) extends RangeReference
{
	def value = slice( seq(row), col )
}

class MutableSeqColumnRangeReference( seq: MutableSeq[MutableSeq[Any]], row: Int, col: Range ) extends SeqColumnRangeReference( seq, row, col ) with Reference
{
	def assign( v: Any ) =
  {
		seq(row) match
		{
			case buf: Buffer[Any] if buf.length < end(col) =>
				buf.appendAll( Iterator.fill(end(col) - buf.length)(null) )
			case _ =>
		}

		v match
		{
			case s: Seq[Any] =>
				for ((e, i) <- s zip col)
					seq(row)(i) = e
		}
	}
}

// abstract class Seq2DRangeReference( seq: Seq[Seq[Any]], row: Range, col: Range ) extends Reference
// {
// 	protected val rend =
// 		if (row.isInclusive)
// 			row.end + 1
//     else
// 			row.end
// 
// 	protected val cend =
// 		if (col.isInclusive)
// 			col.end + 1
//     else
// 			col.end
// 
// 	def value =
// 		for ((r, i) <- seq.zipWithIndex if row contains i)
// 			yield
// 				
// }

// class MutableSeq2DRangeReference( seq: MutableSeq[MutableSeq[Any]], row: Int, col: Int ) extends Seq2DRangeReference( seq, row, col ) with Reference
// {
// 	def assign( v: Any ) =
//   {
// 		seq match
// 		{
// 			case buf: Buffer[MutableSeq[Any]] if buf.length < end =>
// 				buf.appendAll( Iterator.fill(end - buf.length)(ArrayBuffer.fill(seq.head.length)(null)) )
// 			case _ =>
// 		}
// 
// 		v match
// 		{
// 			case s: Seq[Seq[Any]] =>
// 				for ((r, i) <- s.zipWithIndex if row contains i)
// 					seq(i)(col) = r(col)
// 		}
// 	}
// }

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

class ImmutableSeqRangeReference( seq: ImmutableSeq[Any], range: Range ) extends SeqRangeReference(seq, range) with ReadOnlyReference
{	
	val name = "tried to assign to slice " + (range.start, end(range)) + ", but sequence"
}

class Immutable2DSeqReference( seq: ImmutableSeq[ImmutableSeq[Any]], row: Int, col: Int ) extends ReadOnlyReference
{
	val name = "tried to assign to row " + row + ", column " + col + ", but sequence"

	def value = seq( row )( col )
}

class ImmutableSeqRowRangeReference( seq: ImmutableSeq[ImmutableSeq[Any]], row: Range, col: Int ) extends SeqRowRangeReference( seq, row, col ) with ReadOnlyReference
{
	val name = "tried to assign to row range" + row + ", column " + col + ", but sequence"
}

class ImmutableSeqColumnRangeReference( seq: ImmutableSeq[ImmutableSeq[Any]], row: Int, col: Range ) extends SeqColumnRangeReference( seq, row, col ) with ReadOnlyReference
{
	val name = "tried to assign to column range" + row + ", column " + col + ", but sequence"
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