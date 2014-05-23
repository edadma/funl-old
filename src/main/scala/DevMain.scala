/*     ______            __                                      *\
**    / ____/_  __ ___  / /     FunL Programming Language        **
**   / __/ / / / / __ \/ /      (c) 2014, Edward A. Maxedon, Sr. **
**  / /   / /_/ / / / / /__     http://funl-lang.org/            **
** /_/    \____/_/ /_/____/                                      **
\*                                                               */

package funl

import interp.Interpreter._
import interp.Evaluator


object DevMain extends App
{
	val s =
"""
def
	choose( n, 0 ) | n >= 0 = 1
	choose( n, n ) | n >= 0 = 1
	choose( n, k ) | 1 <= k and k <= n - 1 = choose( n - 1, k - 1 ) + choose( n - 1, k )
	choose( n, k ) = error( "choose: illegal arguments: " + (n, k) )

println( choose(2, 2) )

//def bernoulli( n ) = 
// import rosetta.*
// 
// println( maximum([3, 6, 7, 2, 1]) )
// for m <- 0..3
// 	for n <- 0..4
//		println( m, n, ack(m, n) )
"""

//class javax.swing.JFrame
// 	f = JFrame()
// 	f.setSize( 200, 100 )
// 	f.setTitle( 'asdf' )
// 	f.setVisible( true )
// 	f.setDefaultCloseOperation( JFrame.EXIT_ON_CLOSE )

// 	cg
// 	start = time()
// 
// 	for i <- 1..10000000 do 0
// 
// 	println time() - start

	new Evaluator()( parse("main", s) )
}