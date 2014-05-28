/*     ______            __                                      *\
**    / ____/_  __ ___  / /     FunL Programming Language        **
**   / __/ / / / / __ \/ /      (c) 2014, Edward A. Maxedon, Sr. **
**  / /   / /_/ / / / / /__     http://funl-lang.org/            **
** /_/    \____/_/ /_/____/                                      **
\*                                                               */

package funl.modules

import java.io.{File, RandomAccessFile}

object IO
{
	def readFile( a: List[Any] ) =
	{
		val file =
			a match
			{
				case List( f: File ) => new RandomAccessFile( f, "r" )
				case List( f: String ) => new RandomAccessFile( f, "r" )
			}
		val data = new Array[Byte](file.length.toInt)
		
		file.readFully( data )
		data
	}
}