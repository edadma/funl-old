package funl

import java.io.PrintWriter

import scala.tools.jline.console.ConsoleReader

import funl.interp.Evaluator
import funl.interp.Evaluator._


object REPL extends App
{
	val reader = new ConsoleReader
	val out = new PrintWriter( reader.getTerminal.wrapOutIfNeeded(System.out), true )
	var line: String = null
	val eval = new Evaluator
	var count = 1

	reader.setBellEnabled( false )
	reader.setPrompt( "FunL> " )

	out.println( "Welcome to FunL version 0.1" )
	out.println( "Type in expressions to have them evaluated." )
	out.println( "Type :help for more information." )
	out.println

	eval.enterEnvironment( null )

	while ({line = reader.readLine; line != null})
	{
		line match
		{
			case ":help" =>
				out.println( ":help                      print this summary" )
				out.println( ":quit                      exit the interpreter" )
				out.println
			case ":quit" => sys.exit
			case _ =>
				try
				{
				val res = statement( line, eval )
				val name = "res" + count

					out.println( name + ": " + res.getClass.getName + " = " + res )
					out.println
					eval.assign( Symbol(name) -> res )
					count += 1
				}
				catch
				{
					case e: Exception =>
						out.println( e )
						out.println
				}
		}
	}
}