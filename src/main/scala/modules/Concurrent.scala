/*     ______            __                                                  *\
**    / ____/_  __ ___  / /     FunL Programming Language                    **
**   / __/ / / / / __ \/ /      Copyright (c) 2014 by Edward A. Maxedon, Sr. **
**  / /   / /_/ / / / / /__     http://funl-lang.org/                        **
** /_/    \____/_/ /_/____/                                                  **
\*                                                                           */

package funl.modules

import java.util.concurrent.{Executors, ScheduledExecutorService, TimeUnit}

import funl.interp.Evaluator


object Concurrent
{
	val scheduler = Executors.newScheduledThreadPool( 8 )
	
	def scheduleAtFixedRate( c: Evaluator#Closure, initialDelay: Int, period: Int ) =
		scheduler.scheduleAtFixedRate( c.runnable, initialDelay, period, TimeUnit.MILLISECONDS )

	def schedule( c: Evaluator#Closure, delay: Int ) = scheduler.schedule( c.runnable, delay, TimeUnit.MILLISECONDS )
}