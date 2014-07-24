/*     ______            __                                     *\
**    / ____/_  __ ___  / /     FunL Programming Language       **
**   / __/ / / / / __ \/ /      (c) 2014 Edward A. Maxedon, Sr. **
**  / /   / /_/ / / / / /__     http://funl-lang.org/           **
** /_/    \____/_/ /_/____/                                     **
\*                                                              */

package funl.modules

import collection.mutable.{AbstractBuffer}


class BitArray( init: Array[Byte] ) extends AbstractBuffer[Int]
{
	def this() = this( Array[Byte]() )
	
	def this( s: List[Any] ) =
	this(
		{
		val array = new Array[Byte]( s.length )

			for ((b, i) <- (s map (_.asInstanceOf[Number].intValue)) zipWithIndex)
				array(i) = b.asInstanceOf[Byte]

			array
		} )

	private var _length =
		if (init isEmpty)
			0
		else
			init.length*8
	private var bits =
		if (init isEmpty)
			BigInt( 0 )
		else
			BigInt( init )
// 			BigInt( 1 )
// 		else
// 			BigInt( init ) | (BigInt( 1 ) << init.length*8)

	def apply( idx: Int ) =
		if (0 <= idx && idx < length)
			if (bits testBit (length - 1 - idx)) 1 else 0
		else
			throw new IndexOutOfBoundsException

	def update( n: Int, newelem: Int )
	{
		
	}
	
	def length =  _length
// 	def length =  bits.bitLength - 1

	def iterator =
		new Iterator[Int]
		{
			val _bits = bits
			var index = _length - 1

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

	def clear
	{
		bits = BigInt( 0 )
		_length = 0
// 		bits = BigInt( 1 )
	}
	
	def +=( elem: Int ) =
	{
		bits = (bits << 1) | (elem&1)
		_length += 1
		this
	}

	def +=:( elem: Int ) =
	{
// 		bits setBit bits.bitLength
// 		
// 		if ((elem&1) == 0)
// 			bits = bits clearBit (bits.bitLength - 1)
		if ((elem&1) == 1)
			bits setBit bits.bitLength

		_length += 1
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

	def appendInt( a: Int ) =
	{
		bits = (bits << 32) | a
		_length += 32
		this
	}

	def toIntVector =
	{
	val bytes = bits.toByteArray
	val bytes1 =
		if (bytes(0) == 0)
			bytes.tail
		else
			bytes
	val aligned =
		if (bytes1.length%4 == 0)
			bytes1
		else
		{
		val padding = 4 - bytes1.length%4
		val dst = Array.fill[Byte]( bytes1.length + padding )( 0 )

			Array.copy( bytes, 0, dst, padding, bytes1.length )
			dst
		}

		(for (i <- 0 until aligned.length by 4)
			yield (aligned(i) << 24) | ((aligned(i + 1)&0xff) << 16) | ((aligned(i + 2)&0xff) << 8) | (aligned(i + 3)&0xff)).toVector
	}

	override def toString = "BitArray(" + super.mkString(", ") + ")"
}
