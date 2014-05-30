/*     ______            __                                                  *\
**    / ____/_  __ ___  / /     FunL Programming Language                    **
**   / __/ / / / / __ \/ /      Copyright (c) 2014 by Edward A. Maxedon, Sr. **
**  / /   / /_/ / / / / /__     http://funl-lang.org/                        **
** /_/    \____/_/ /_/____/                                                  **
\*                                                                           */

package funl

import java.io.PrintWriter

import jline.console.ConsoleReader

import funl.interp.{Evaluator, Module}
import funl.interp.Interpreter._


object REPL extends App
{
	System.getProperties.setProperty( "jline.shutdownhook", "true" )

	val reader = new ConsoleReader
	val out = new PrintWriter( reader.getTerminal.wrapOutIfNeeded(System.out), true )
	var line: String = null
	val eval = new Evaluator
	var count = 1
	implicit val env = new eval.Environment

	reader.setBellEnabled( false )
	reader.setPrompt( "> " )

	for {line <- """	|    ______            __
									|   / ____/_  __ ___  / /     FunL Programming Language
									|  / __/ / / / / __ \/ /      Copyright (c) 2014 by Edward A. Maxedon, Sr.
									| / /   / /_/ / / / / /__     http://funl-lang.org/
									|/_/    \____/_/ /_/____/                                     
									|                                                                          """.stripMargin.lines}
		out.println( line )

	out.println( "Welcome to FunL version " + VERSION )
	out.println( "Type in expressions to have them evaluated." )
	out.println( "Type :help for more information." )
	out.println

	eval.loadPredef( "REPL" )
	eval.enterActivation( null, eval.module("REPL") )
	
	while ({line = reader.readLine; line != null})
	{
		line match
		{
			case ":help" =>
				out.println( ":help                      print this summary" )
				out.println( ":quit                      exit the interpreter" )
				out.println
			case ":quit" =>
				sys.exit
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
								
							eval.assign( "REPL", name -> res )
							count += 1
					}
					
					out.println
				}
				catch
				{
					case e: Exception =>
					e.printStackTrace( out )
						out.println( e )
						out.println
				}
		}
	}
}