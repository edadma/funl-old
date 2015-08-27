/*     ______            __                                     *\
**    / ____/_  ______  / /     FunL Programming Language       **
**   / /_  / / / / __ \/ /      (c) 2014 Edward A. Maxedon, Sr. **
**  / __/ / /_/ / / / / /__     http://funl-lang.org/           **
** /_/    \__,_/_/ /_/____/                                     **
\*                                                              */

package funl.interp

import concurrent.SyncVar


abstract class Generator[T] extends Iterator[T]
{
	private val value = new SyncVar[Option[T]]
	
	new Thread(
		new Runnable
		{
			def run
			{
				compute
				value put None
			}
		} ).start
	
	def compute
	
	protected def yieldNext( v: T ) = value put Some( v )
	
	def hasNext = value.get != None
	
	def next =
		if (hasNext)
			value.take.get
		else
			throw new NoSuchElementException
}