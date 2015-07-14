/*     ______            __                                     *\
**    / ____/_  __ ___  / /     FunL Programming Language       **
**   / __/ / / / / __ \/ /      (c) 2014 Edward A. Maxedon, Sr. **
**  / /   / /_/ / / / / /__     http://funl-lang.org/           **
** /_/    \____/_/ /_/____/                                     **
\*                                                              */

package funl

import java.io.PrintWriter

import jline.console.ConsoleReader

import funl.interp.{Evaluator, Module, ConstantReference}
import funl.interp.Interpreter._


object REPL extends App
{
	val welcome =
		"""	|    ______            __
			|   / ____/_  __ ___  / /     FunL Programming Language
			|  / __/ / / / / __ \/ /      (c) 2014 Edward A. Maxedon, Sr.
			| / /   / /_/ / / / / /__     http://funl-lang.org/
			|/_/    \____/_/ /_/____/
			|                                                             """.stripMargin.lines ++
		Iterator (
			"Welcome to FunL version " + VERSION,
			"Type in expressions to have them evaluated.",
			"Type :help for more information."
		)

	start( welcome )
	
	def start( welcome: Iterator[String] ) {
		System.getProperties.setProperty( "jline.shutdownhook", "true" )

		val reader = new ConsoleReader
		val out = new PrintWriter( reader.getTerminal.wrapOutIfNeeded(System.out), true )
		var line: String = null
		val eval = new Evaluator
		var count = 1
		implicit val env = new eval.Environment

		for (l <- welcome)
			out.println( l )
			
		out.println
		reader.setBellEnabled( false )
		reader.setPrompt( "> " )
		eval.loadPredef( "REPL" )
		eval.enterActivation( null, null, eval.module("REPL") )
		
		while ({line = reader.readLine; line != null})
		{
			line.trim match
			{
				case ":help" =>
					out.println( ":help                      print this summary" )
					out.println( ":quit                      exit the REPL" )
					out.println
				case ":quit" =>
					sys.exit
				case "" =>
					out.println
				case _ =>
					try
					{
						statement( "REPL", line, eval ) match
						{
							case None =>
							case Some( res ) =>
							val name = "res" + count

								if (res == null)
									out.println( name + " = null" )
								else
									out.println( name + ": " + res.getClass.getName + " = " + display(res) )
									
								eval.assign( "REPL", name -> ConstantReference(name, res) )
								count += 1
						}
						
						out.println
					}
					catch
					{
						case e: Exception =>
	//					e.printStackTrace( out )
							out.println( e )
							out.println
					}
			}
		}
	}
}