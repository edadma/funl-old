package funl.interp

import collection.mutable.HashMap


class Module( val name: Symbol )
{
	val symbols = new HashMap[Symbol, Any]
}
