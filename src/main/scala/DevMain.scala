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
	def array( a: List[Any] ) = Array[Any](1, 2, 3)

	val s =
"""
// function funl.DevMain.array
// 
// a = array()
// println( a )
// 
// def f =
// 	println( 'here' )
// 	a(1)
// 
// f() = 123
// println( a(1) )
def f( a ) =
	def g = a + b

	g() + 2

val b = 1

println( f(3))
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