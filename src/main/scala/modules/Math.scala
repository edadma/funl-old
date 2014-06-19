/*     ______            __                                      *\
**    / ____/_  __ ___  / /     FunL Programming Language        **
**   / __/ / / / / __ \/ /      (c) 2014, Edward A. Maxedon, Sr. **
**  / /   / /_/ / / / / /__     http://funl-lang.org/            **
** /_/    \____/_/ /_/____/                                      **
\*                                                               */

package funl.modules

import funl.lia.{Complex, Math => MATH}
import funl.interp.Interpreter._
import funl.interp.RuntimeException


object Math
{
// 	def sqrt( a: List[Any] ) =
// 		a match
// 		{
// 			case List( n: Number ) => MATH.sqrtFunction( n )
// 		}
// 		
// 	def abs( a: List[Any] ) =
// 		a match
// 		{
// 			case List( n: Number ) => MATH.absFunction( n )
// 		}

// 	def choose( a: Vector[Any] ): Any =
// 		a match
// 		{
// 			case Vector( n: Int, k: Int ) => choose( n, k )
// 			case Vector( n: BigInt, k: Int ) => Math.maybeDemote( choose(n, BigInt(k)) )
// 			case Vector( n: Int, k: BigInt ) => Math.maybeDemote( choose(BigInt(n), k) )
// 			case Vector( n: BigInt, k: BigInt ) => Math.maybeDemote( choose(n, k) )
// 		}
// 
// 	def choose( n: Int, k: Int ): Int =
// 	{
// 		if ((k == 0 || n == k) && n >= 0)
// 			1
// 		else if (1 <= k && k <= n - 1)
// 			choose( n - 1, k - 1 ) + choose( n - 1, k )
// 		else
// 			throw new RuntimeException( "choose: illegal arguments: " + (n, k) )
// 	}
// 
// 	def choose( n: BigInt, k: BigInt ): BigInt =
// 	{
// 		if ((k == 0 || n == k) && n >= 0)
// 			1
// 		else if (1 <= k && k <= n - 1)
// 			choose( n - 1, k - 1 ) + choose( n - 1, k )
// 		else
// 			throw new RuntimeException( "choose: illegal arguments: " + (n, k) )
// 	}
}