package funl.interp


object TailRecursion
{
	def apply( s: ModuleAST )
	{
		for (c <- s.components)
			c match
			{
				case DefAST( _, name, func ) =>
					for (p <- func.parts)
						expr( name, p.body )
				case _ =>
			}
	}
	
	def expr( n: Symbol, e: ExprAST )
	{
		e match
		{
			case BlockExprAST( l ) =>
				l.last match
				{
					case ExpressionStatementAST( e ) => expr( n, e )
					case _ =>
				}
			case a@ApplyExprAST( f, _, _ ) =>
				f match
				{
					case VariableExprAST( _, v ) =>
						if (v.name == n)
							a.tailrecursive = true
					case CaseFunctionExprAST( _, cases ) =>
						for (c <- cases)
							expr( n, c.parts.head.body )
					case FunctionExprAST( _, _, parts ) =>
						expr( n, parts.head.body )
					case _ =>
				}
			case ConditionalExprAST( cond, no ) =>
				for ((_, thenpart) <- cond)
					expr( n, thenpart )

				if (no != None)
					expr( n, no.get )
			case BooleanConnectiveExprAST( _, _, right ) =>
				expr( n, right )
			case _ =>
		}
	}
}