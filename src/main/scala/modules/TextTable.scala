/*     ______            __                                     *\
**    / ____/_  __ ___  / /     FunL Programming Language       **
**   / __/ / / / / __ \/ /      (c) 2014 Edward A. Maxedon, Sr. **
**  / /   / /_/ / / / / /__     http://funl-lang.org/           **
** /_/    \____/_/ /_/____/                                     **
\*                                                              */

package funl.modules

import collection.mutable.{ArrayBuffer, HashSet, HashMap}
import java.sql.ResultSet
import java.sql.Types._


object TextTable
{
	trait Alignment
	case object LEFT extends Alignment
	case object RIGHT extends Alignment
	case object CENTER extends Alignment

//	val BOLD = 0x100
//	val BLINK = 0x200
//	val UNDERLINED = 0x400

	def apply( result: ResultSet ) =
	{
	val t = new TextTable
	val metadata = result.getMetaData
	val cols = metadata.getColumnCount

		t.header( (for (i <- 1 to cols) yield metadata.getColumnName(i)): _* )

		for (i <- 1 to cols)
			metadata.getColumnType( i ) match
			{
				case BIGINT|DECIMAL|DOUBLE|FLOAT|INTEGER|NUMERIC|REAL|SMALLINT|TINYINT => t.colalignment( i, RIGHT )
				case _ =>
			}

		t.line

		while (result.next)
			t.row( (for (i <- 1 to cols) yield result.getObject(i).toString): _* )

		t
	}
}

class TextTable
{
	import TextTable._

	private val table = new ArrayBuffer[Seq[String]]
	private val lines = new HashSet[Int]
	private val aligns = new HashMap[(Int, Int), Alignment]
	private val styles = new HashMap[(Int, Int), String]

	private var ansi = true
	private var columns: Int = _
	private var colaligns: Array[Alignment] = _
	private var widths: IndexedSeq[Int] = _
	private var width: Int = _

	def noansi = ansi = false

	if (System.getProperty( "os.name" ).startsWith( "Windows" ))
		noansi

	def rightAlignment( col: Int ) = colalignment( col, RIGHT )
	
	def colalignment( col: Int, align: Alignment )
	{
		require( !table.isEmpty, "empty table" )
		require( col >= 1 && col <= columns, "column number out of range" )

		colaligns(col - 1) = align
	}

	def header( s: String* )
	{
		row( s: _* )

		for (i <- 1 to columns)
		{
			alignment( i, CENTER )
			style( i, Console.BOLD )
		}
	}

	def row( s: Any* )
	{
		require( s.length > 0, "need at least one column" )

		if (table.isEmpty)
		{
			columns = s.length
			colaligns = Array.fill[Alignment]( columns )( LEFT )
		}
		else if (columns != s.length)
			sys.error( "table is " + columns + "column(s) wide" )

		table += s.map( _.toString ).toIndexedSeq
		widths = null
	}

	def alignment( col: Int, align: Alignment )
	{
		require( !table.isEmpty, "empty table" )
		require( col >= 1 && col <= columns, "column number out of range" )

		aligns((table.size - 1, col - 1)) = align
	}

	def style( col: Int, style: String )
	{
		require( !table.isEmpty, "empty table" )
		require( col >= 1 && col <= columns, "column number out of range" )

		styles((table.size - 1, col - 1)) = style
	}

	def line
	{
		lines += table.size
	}

	def format
	{
		require( !table.isEmpty, "empty table" )

		if (widths eq null)
		{
			widths = table.transpose.map( _.aggregate(0)(_ max _.length, _ max _) ).toIndexedSeq
			width = widths.reduce( _ + _ ) + widths.length*3 + 1
		}
	}

	override def toString =
	{
		format

	val buf = new StringBuilder

		def line
		{
			buf append  '+'

			for (j <- 0 until columns)
			{
				buf append  "-"*(widths(j) + 2)
				buf append  '+'
			}

			buf append '\n'
		}

		line

		for (i <- 0 until table.size)
		{
			if (lines contains i)
				line

			for (j <- 0 until columns)
			{
				buf append "| "

			val elem = table(i)(j)
			val diff = widths(j) - elem.length
			val (pre, post) =
				aligns.get((i, j)).getOrElse( colaligns(j) ) match
				{
					case LEFT => (0, diff)
					case RIGHT => (diff, 0)
					case CENTER =>
						val before = diff/2

						(before, diff - before)
				}
			val (pres, posts) =
				styles.get((i, j)) match
				{
				case Some( style ) if ansi => (style, Console.RESET)
				case _ => ("", "")
				}

				buf append " "*pre
				buf append pres
				buf append elem
				buf append posts
				buf append " "*(post + 1)
			}

			buf append "|\n"
		}

		line
		buf.toString
	}
}