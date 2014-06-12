/*     ______            __                                     *\
**    / ____/_  __ ___  / /     FunL Programming Language       **
**   / __/ / / / / __ \/ /      (c) 2014 Edward A. Maxedon, Sr. **
**  / /   / /_/ / / / / /__     http://funl-lang.org/           **
** /_/    \____/_/ /_/____/                                     **
\*                                                              */

package funl.interp

import collection.mutable.{AbstractBuffer}


class BitArray extends AbstractBuffer[Int]
{
	private var bits = BigInt( 1 )

	def apply( idx: Int ) =
	{
		if (0 <= idx && idx < length)
			if (bits testBit (bits.bitLength - 2 - idx)) 1 else 0
		else
			throw new IndexOutOfBoundsException
	}

	def update( n: Int, newelem: Int )
	{
		
	}
	
	def iterator =
		new Iterator[Int]
		{
			val _bits = bits
			var index = bits.bitLength - 2

			def hasNext = index >= 0

			def next =
				if (hasNext)
				{
				val res = if (_bits testBit index) 1 else 0

					index -= 1
					res
				}
				else
					sys.error( "no more bits" )
		}
		
	def length =  bits.bitLength - 1

	def clear
	{
		bits = BigInt( 1 )
	}
	
	def +=( elem: Int ) =
	{
		
		bits = (bits << 1) | (elem&1)
		this
	}

	def +=:( elem: Int ) =
	{
		bits setBit bits.bitLength
		
		if ((elem&1) == 0)
			bits = bits clearBit (bits.bitLength - 1)

		this
	}

	def insertAll( n: Int, iter: Traversable[Int] )
	{
		
	}

	def remove( n: Int ) =
	{
	val res = apply( n )

		res
	}

	override def toString = "BitArray(" + super.mkString(", ") + ")"
}