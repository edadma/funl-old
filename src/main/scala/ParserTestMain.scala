package funl.interp

import scala.util.parsing.input.CharSequenceReader


object ParserTestMain extends App
{
// 	def plugin( eval: Evaluator )
// 	{
// 		eval.function( 'f, a => a.head.asInstanceOf[Int] + 2 )
// 	}
// 
	val r = new CharSequenceReader(
"""
class java.lang.System
class scala.io.Source
class java.io.File

def lines( file ) = Source.fromFile( File(file), 'UTF-8', 1000 ).getLines()

def fib( n ) =
	a, b = 0, 1
	
	while a < n do
		print( a + (if b < n then ", " else "\n") )
		a, b = b, a+b
	
main
	fib(1000)
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

	val parser = new FunLParser( 'main )
	var s = parser.lexical.read(r)

// 	while (!s.atEnd)
// 	{
// 		println( s.first )
// 		s = s.rest
// 	}

	val p = parser.parseSource( r )

	p match
	{
		case parser.Success( l, _ ) =>
			val eval = new Evaluator()

//		println( l )
			Interpreter.markTailRecursion( l )
//		println( l )
			eval( l )
		case parser.Failure( m, r ) =>
			println( r.pos + ": " + m + '\n' + r.pos.longString )
		case parser.Error( m, r ) =>
			println( r.pos + ": " + m )
	}
}