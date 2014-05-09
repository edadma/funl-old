/*     ______            __                                      *\
**    / ____/_  __ ___  / /     FunL Programming Language        **
**   / __/ / / / / __ \/ /      (c) 2014, Edward A. Maxedon, Sr. **
**  / /   / /_/ / / / / /__     http://funl-lang.org/            **
** /_/    \____/_/ /_/____/                                      **
\*                                                               */

package funl


object Options
{
	type OptionMap = Map[Symbol, String]
	type OptionValue = (Symbol, String)
	type OptionFunction = PartialFunction[List[String], (OptionValue, List[String])]

	def apply( args: Array[String], defaults: OptionValue* )( options: OptionFunction ): OptionMap =
		nextOption( Map(), args.toList, options, defaults.toList )

	private def nextOption( map: OptionMap, list: List[String], options: OptionFunction, defaults: List[OptionValue] ): OptionMap =
	{
		if (list == Nil)
			map ++ defaults.filterNot( d => map.contains(d._1) )
		else
		{
		val (kv, t) = options( list )

			nextOption( map ++ Map(kv), t, options, defaults )
		}
	}
}