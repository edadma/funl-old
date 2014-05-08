/*     ______            __                                      *\
**    / ____/_  __ ___  / /     FunL Programming Language        **
**   / __/ / / / /  _ \/ /      (c) 2014, Edward A. Maxedon, Sr. **
**  / /   / /_/ / / / / /__     http://funl-lang.org/            **
** /_/    \____/_/ /_/____/                                      **
\*                                                               */

package funl.interp

import scala.util.parsing.input.CharSequenceReader

import Interpreter._


object ParserTestMain extends App
{
// 	def plugin( eval: Evaluator )
// 	{
// 		eval.function( 'f, a => a.head.asInstanceOf[Int] + 2 )
// 	}
// 
	val r = new CharSequenceReader(
"""
import io

main
	for line <- io.lines( 'io.funl' ) do
		println( line )
""" )
	
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

	new Evaluator()( parse('main, r) )
}