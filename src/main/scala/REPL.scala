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
	start(
		"""	|    ______            __
			|   / ____/_  __ ___  / /     FunL Programming Language
			|  / __/ / / / / __ \/ /      (c) 2015 Edward A. Maxedon, Sr.
			| / /   / /_/ / / / / /__     http://funl-lang.org/
			|/_/    \____/_/ /_/____/
			|                                                             """.stripMargin.lines.toSeq ++
		Seq(
			"Welcome to FunL version " + VERSION,
			"Type in expressions to have them evaluated.",
			"Type :help for more information.",
			""
		) map ("$out.println('''" + _ + "''')") mkString ("\n"), true, true, true, false
	)
	
	def start( init: String, nulls: Boolean, types: Boolean, iterators: Boolean, trace: Boolean ) {
		System.getProperties.setProperty( "jline.shutdownhook", "true" )

		val reader = new ConsoleReader
		val out = new PrintWriter( reader.getTerminal.wrapOutIfNeeded(System.out), true )
		var line: String = null
		val eval = new Evaluator
		var count = 1
		implicit val env = new eval.Environment

		out.println
		reader.setBellEnabled( false )
		reader.setPrompt( "> " )
//		eval.loadPredef( "REPL" )
		eval.enterActivation( null, null, eval.module("REPL") )
		eval.assign( "REPL", "$out" -> out )
		snippet( "REPL", init, eval )
		
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
							case Some( _res ) =>
							val name = "res" + count
							val res =
								if (!iterators && _res.isInstanceOf[Iterator[_]])
									_res.asInstanceOf[Iterator[_]].toSeq
								else
									_res
									
								if (res == null) {
									if (nulls)
										out.println( name + " = null" )
								} else
									if (types)
										out.println( name + ": " + res.getClass.getName + " = " + display(res) )
									else
										out.println( name + " = " + display(res) )
										
								eval.assign( "REPL", name -> ConstantReference(name, res) )
								count += 1
						}
						
						out.println
					}
					catch
					{
						case e: Exception =>
							if (trace)
								e.printStackTrace( out )
							else
								out.println( e )
								
							out.println
					}
			}
		}
	}
}