/*     ______            __                                      *\
**    / ____/_  __ ___  / /     FunL Programming Language        **
**   / __/ / / / / __ \/ /      (c) 2014, Edward A. Maxedon, Sr. **
**  / /   / /_/ / / / / /__     http://funl-lang.org/            **
** /_/    \____/_/ /_/____/                                      **
\*                                                               */

package funl


object Options
{
// 	def apply( args: Array[String] )( options: PartialFunction[List[String], List[String]] )
// 	{
// 		def nextOption( list: List[String] ): Unit =
// 			if (list != Nil)
// 				nextOption( options(list) )
// 
// 		nextOption( args.toList )
// 	}
	
	type OptionMap = Map[Symbol, String]
	type OptionValue = (Symbol, String)

	def apply( args: Array[String], defaults: OptionValue* )( options: PartialFunction[List[String], (OptionValue, List[String])] ): OptionMap =
	{
		def nextOption( map: OptionMap, list: List[String], defaults: List[OptionValue] ): OptionMap =
		{
			if (list == Nil)
				map ++ defaults.filterNot( d => map.contains(d._1) )
			else
			{
			val (kv, t) = options( list )

				nextOption( map ++ Map(kv), t, defaults )
			}
		}
		
		nextOption( Map(), args.toList, defaults.toList )
	}
}