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
import math.*

def bernoulli( n::Integer ) | n >= 0 = sigma( 0..n, k -> 1/(k + 1)*sigma(0..k, r -> (if even(r) then 1 else -1)*choose(k, r)*(r^n)) )

for i <- 0..60 if i == 1 or even( i )
	println( i, bernoulli(i) )

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