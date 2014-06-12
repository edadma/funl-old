/*     ______            __                                     *\
**    / ____/_  __ ___  / /     FunL Programming Language       **
**   / __/ / / / / __ \/ /      (c) 2014 Edward A. Maxedon, Sr. **
**  / /   / /_/ / / / / /__     http://funl-lang.org/           **
** /_/    \____/_/ /_/____/                                     **
\*                                                              */

package funl.interp

import collection.mutable.{Seq => MutableSeq, Map => MutableMap, Buffer}
import collection.immutable.{Seq => ImmutableSeq, Map => ImmutableMap}


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

abstract class SeqRangeReference( seq: Seq[Any], range: Range ) extends Reference
{
	protected val end =
		if (range.isInclusive)
			range.end + 1
    else
			range.end

	def value = seq.slice( range.start, end )
}

class MutableSeqRangeReference( seq: MutableSeq[Any], range: Range ) extends SeqRangeReference(seq, range) with Reference
{
  def assign( v: Any ) =
  {
		seq match
		{
			case buf: Buffer[Any] if buf.length < end =>
				buf.appendAll( Iterator.fill(end - buf.length)(null) )
			case _ =>
		}
		
		v match
		{
			case s: Seq[Any] =>
				for ((i, e) <- range zip s)
					seq(i) = e
		}
	}
}

class Mutable2DSeqReference( seq: MutableSeq[MutableSeq[Any]], row: Int, col: Int ) extends Reference
{
	def value = seq( row )( col )

	def assign( v: Any ) = seq(row)(col) = v
}

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
	val name = "tried to assign to slice " + (range.start, end) + ", but sequence"
}

class Immutable2DSeqReference( seq: ImmutableSeq[ImmutableSeq[Any]], row: Int, col: Int ) extends ReadOnlyReference
{
	val name = "tried to assign to row " + row + ", column " + col + ", but sequence"

	def value = seq( row )( col )
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