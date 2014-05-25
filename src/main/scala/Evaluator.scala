/*     ______            __                                      *\
**    / ____/_  __ ___  / /     FunL Programming Language        **
**   / __/ / / / / __ \/ /      (c) 2014, Edward A. Maxedon, Sr. **
**  / /   / /_/ / / / / /__     http://funl-lang.org/            **
** /_/    \____/_/ /_/____/                                      **
\*                                                               */

package funl.interp

import java.lang.reflect.{Method, Modifier}

import collection.mutable.{ArrayBuffer, ArrayStack, ListBuffer, HashMap, HashSet, Seq => MutableSeq, Map => MutableMap}
import collection.immutable.{ListMap, Seq => ImmutableSeq, Map => ImmutableMap}
import util.parsing.input.{Reader, CharSequenceReader}
import math._
import compat.Platform._

import funl.lia.{Complex, Math}
import Interpreter._


trait Types
{
	type SymbolMap = HashMap[String, Any]
	type Function = Vector[Any] => Any
}

class Evaluator extends Types
{
	class Datatype( name: String )
	
	case class Constructor( datatype: String, name: String, fields: List[String] )

	case class NativeMethod( o: Any, m: List[Method] )
	
	class Record( val datatype: String, val name: String, val fields: List[String], val args: List[Any] )
	{
		private val map = ListMap[String, Any]( (fields zip args): _* )

		def get( key: String ) = map.get( key )

		override def hashCode = datatype.hashCode ^ name.hashCode ^ fields.hashCode ^ args.hashCode

		override def equals( o: Any ) =
			o match
			{
				case r: Record => name == r && args == r.args
				case _ => false
			}
			
		override def toString = name + (if (args isEmpty) "" else args.mkString("( ", ", ", " )"))
	}

	class BreakThrowable extends Throwable

	class ContinueThrowable extends Throwable

	case class Environment( stack: Int, activations: Int, scope: Int )
		
	val symbols = new SymbolMap
	val sysvars = new SymbolMap
	val datatypes = new HashSet[String]
	val stack = new StackArray[Any]
	val activations = new StackArray[Activation]
	
	var last: Option[Any] = None

	def sysvar( k: String )( v: => Any )
	{
		sysvars(k) = new SystemReference( k )( v )
	}

	sysvar( "time" ) {compat.Platform.currentTime}
	sysvar( "timeZone" ) {java.util.TimeZone.getDefault}
	sysvar( "timeZoneOffset" ) {
		val tz = java.util.TimeZone.getDefault

		tz.getRawOffset + tz.getDSTSavings
		}
	sysvar( "date" ) {new java.util.Date}
	
	def module( m: String ) =
		symbols.get(m) match
		{
			case None =>
				val res = new Module( m )
			
				symbols(m) = res
				res
			case Some( res ) => res.asInstanceOf[Module]
		}

	def loaded( m: String ) = symbols.contains( m ) && symbols(m).isInstanceOf[Module]

	def load( m: String ) =
		if (!loaded( m ))
		{
		val ast = parse( m )

			apply( ast )
      exitEnvironment
		}
	
	def symbol( m: String, key: String ) = module( m ).symbols(key)

	def symbolExists( m: String, key: String ) = module( m ).symbols contains key

	def declarationMap =
		if (topLevel)
			activations.top.module.symbols
		else
			localScope

	def declare( key: String, value: Any ) =
	{
	val syms = declarationMap

		if (syms contains key)
			RuntimeException( "already declared: " + key )
		else
			syms(key) = value

		export( key )
	}

	def export( sym: String ) =
		if (topLevel)
			activations.top.module.exports.add( sym )

	def assign( m: String, key: String, value: Any )
	{
	val syms = module(m).symbols
	
		if (syms contains key)
			RuntimeException( "already declared: " + key )
		else
			syms(key) = value
	}
	
	def assign( module: String, vs: (String, Any)* ): Evaluator =
	{
		for ((k, v) <- vs)
			assign( module, k, v )

		this
	}
	
	def function( m: String, n: String, f: Function ) = assign( m, n, f )

	def eval( t: AST ) =
	{
		apply( t )
		pop
	}

	def exec( t: AST )
	{
		apply( t )
		stack.pop
	}
	
	def neval( t: AST ) = eval( t ).asInstanceOf[Number]
	
	def ieval( t: AST ) = eval( t ).asInstanceOf[Int]
	
	def deval( t: AST ) = eval( t ).asInstanceOf[Number].doubleValue
	
	def beval( t: AST ) = eval( t ).asInstanceOf[Boolean]
	
	def reval( t: AST ) =
	{
		apply( t, creatvars = true )
		rpop
	}

	def teval( t: AST ) =
		eval( t ) match
		{
			case tr: TraversableOnce[Any] => tr
			case s: String => s.map( _.toString ).iterator
			case a: Array[Any] => a.iterator
			case o => 	RuntimeException( "non traversable object: " + o )
		}
	
	def pop = deref( stack.pop )

	def long2bigint( v: Any ) = if (v.isInstanceOf[Long]) BigInt(v.asInstanceOf[Long]) else v
	
	def push( v: Any ) = stack.push( long2bigint(v) )
	
	def rpop = stack.pop.asInstanceOf[Reference]
	
	def dpop = pop.asInstanceOf[Double]
	
	def bpop = pop.asInstanceOf[Boolean]
	
	def void = stack push ()
	
	def list( len: Int ) =
	{
	val buf = new ListBuffer[Any]
	
		for (i <- 1 to len)
			buf prepend pop
			
		buf.toList
	}
	
	def enterEnvironment( closure: Closure, module: Module )
	{
		activations push new Activation( closure, module )
		enterScope
	}
	
	def exitEnvironment = activations.pop

	def getEnvironment = Environment( stack.size, activations.size, activations.top.scope.size )

	def restoreEnvironment( env: Environment )
	{
		stack reduceToSize env.stack
		activations reduceToSize env.activations
		activations.top.scope reduceToSize env.scope
	}
	
	def enterScope = activations.top.scope push new SymbolMap

	def localScope = activations.top.scope.top
	
	def exitScope = activations.top.scope pop

	def topLevel = activations.top.scope.length ==1 && activations.top.closure == null
	
	def assignOperation( r: Reference, op: Symbol, n: Number ) =
	{
	val v = Math( op, r.value, n )
		
		r.assign( v )
		v
	}

	def loadPredef( m: String )
	{
		load( PREDEF )

		for ((k, v) <- module( PREDEF ).symbols)
			assign( m, k -> v )
	}
	
	def apply( l: List[AST] ): Any =
		for (s <- l)
			apply( s )

	def apply( t: AST, creatvars: Boolean = false ): Any =
	{
// 		def vars( m: String, name: String ) =
// 		{
// 			def vars( a: Activation ): Any =
// 			{
// 				a.scope find (_ contains name) match
// 				{
// 					case None =>
// 						if (a.closure != null && a.closure.referencing != null)
// 							vars( a.closure.referencing )
// 						else if (symbolExists( m, name ))
// 							symbol( m, name )
// 						else if (creatvars)
// 							newVar( name )
// 						else
// 							RuntimeException( "unknown variable: " + name )
// 					case Some( map ) => map(name)
// 				}
// 			}
// 			
// 			vars( activations.top )
// 		}
		def vars( name: String ) =
		{
			def vars( a: Activation ): Option[Any] =
			{
				a.scope.reverseIterator find (_ contains name) match
				{
					case None =>
						if (a.closure != null && a.closure.referencing != null)
							vars( a.closure.referencing )
						else
							activations.top.module.symbols.get( name ) match
							{
								case None =>
									if (creatvars)
										Some( newVar(name) )
									else
										RuntimeException( "unknown variable: " + name )
								case v => v
							}
					case Some( map ) => Some( map(name) )
				}
			}

			vars( activations.top )
		}
		
		def newVar( name: String ) =
		{
		val ref = new VariableReference
			
			localScope(name) = ref
			
			ref
		}

		t match
		{
			case ModuleAST( m, s ) =>
				if (m != PREDEF)
					loadPredef( m )

// 				assign( m,
// 					'i -> Complex( 0, 1 ),
// 					'sys -> this
// 					)
				enterEnvironment( null, module(m) )
				apply( s )
			case DeclStatementAST( s ) =>
				apply( s )
				last = None
			case ImportAST( qual, names ) =>
				if (qual == "")
				{
					load( names.head._1 )
					declare( names.head._2.getOrElse(names.head._1), module(names.head._1) )
				}
				else
				{
					load( qual )

					if (names eq null)
						for ((k, v) <- module(qual).symbols if module(qual).exports contains k)
							declare( k, v )
					else
						for ((s, a) <- names)
							declare( a.getOrElse(s), symbol(qual, s) )
				}
			case NativeAST( pkg, names ) =>
				(try
				{
					Some( Class.forName(pkg) )
				}
				catch
				{
					case _: Exception => None
				}) match
				{
					case None =>
						for ((n, a) <- names)
							declare( a.getOrElse(n), Class.forName(pkg + '.' + n) )
					case Some( cls ) =>
						for ((n, a) <- names)
						{
						val methods = cls.getMethods.toList.filter( m => m.getName == n && (m.getModifiers&Modifier.STATIC) == Modifier.STATIC )

							if (methods != Nil)
								declare( a.getOrElse(n), NativeMethod(null, methods) )
							else
							{
								cls.getFields.find( f => f.getName == n && (f.getModifiers&Modifier.STATIC) == Modifier.STATIC ) match
								{
									case None => RuntimeException( "no method or field called '" + n + "' for class '" + pkg + "'" )
									case Some( f ) => declare( a.getOrElse(n), new ConstantReference("field '" + n + "'", f.get(null)) )
								}
							}
						}
				}
// 			case ClassAST( pkg, names ) =>
// 				for ((n, a) <- names)
// 					declare( a.getOrElse(n), Class.forName(pkg + '.' + n) )
// 			case MethodAST( cls, names ) =>
// 				for ((n, a) <- names)
// 				{
// 				val methods = Class.forName( cls ).getMethods.toList.filter( m => m.getName == n && (m.getModifiers&Modifier.STATIC) == Modifier.STATIC )
// 
// 					declare( a.getOrElse(n), NativeMethod(null, methods) )
// 				}
			case FunctionAST( cls, names ) =>
				for ((n, a) <- names)
				{
				val method = Class.forName( cls ).getMethod( n, classOf[Vector[Any]] )

					if ((method.getModifiers&Modifier.STATIC) != Modifier.STATIC) RuntimeException( "function method must be static" )

					declare( a.getOrElse(n), (a => method.invoke(null, a)): Function )
				}
// 			case ConstAST( m, name, expr ) =>
// 				assign( m, name, eval(expr) )
			case VarAST( n, v ) =>
				declare( n, new VariableReference(if (v == None) null else eval(v.get)) )
			case DataAST( n, cs ) =>
				if (datatypes contains n) RuntimeException( "already declared: " + n )

				datatypes.add( n )

				for ((name, fields) <- cs)
					if (fields isEmpty)
						declare( name, new Record(n, name, Nil, Nil) )
					else
						declare( name, Constructor(n, name, fields) )
			case DefAST( name, func ) =>
				declarationMap.get(name) match
				{
					case None => declarationMap(name) = new Closure( if (topLevel) null else activations.top, module(func.module), List(func) )
					case Some( c: Closure ) => declarationMap(name) = new Closure( if (topLevel) null else activations.top, module(func.module), c.funcs :+ func )
					case _ => RuntimeException( "already declared: " + name )
				}

				export( name )
			case ExpressionStatementAST( e ) =>
				last = Some( eval(e) )
			case ValAST( p, e ) =>
				clear( declarationMap, p ) foreach export

				if (!unify( declarationMap, eval(e), p ))
					RuntimeException( "unification failure" )
			case SysvarExprAST( s ) =>
				sysvars.get( s ) match
				{
					case None => RuntimeException( s + " not a system variable" )
					case Some( v ) => push( v )
					case _ => RuntimeException( "problem" )
				}
			case TestExprAST( name ) => push( vars( name ) != None )
			case BreakExprAST =>
				throw new BreakThrowable
			case ContinueExprAST =>
				throw new ContinueThrowable
			case LiteralExprAST( v ) => push( v )
			case StringLiteralExprAST( s ) =>
				val buf = new StringBuilder
				
				def chr( r: Reader[Char] )
				{
						if (!r.atEnd)
						{
							if (r.first == '\\')
							{
								if (r.rest.atEnd)
									RuntimeException( "unexpected end of string" )

								if (r.rest.first == 'u')
								{
								var u = r.rest.rest
								
									def nextc =
										if (u.atEnd)
											RuntimeException( "unexpected end of string inside unicode sequence" )
										else
										{
										val res = u.first

											u = u.rest
											res
										}

									buf append Integer.valueOf( new String(Array(nextc, nextc, nextc, nextc)), 16 ).toChar
									chr( u )
								}
								else
								{
									buf.append(
										Map (
											'\\' -> '\\', '\'' -> '\'', '/' -> '/', 'b' -> '\b', 'f' -> '\f', 'n' -> '\n', 'r' -> '\r', 't' -> '\t'
										).get(r.rest.first) match
										{
											case Some( c ) => c
											case _ => RuntimeException( "illegal escape character " + r.rest.first )
										} )

									chr( r.rest.rest )
								}
							}
							else
							{
								buf append r.first	
								chr( r.rest )
							}
						}
				}

				chr( new CharSequenceReader(s) )
				push( buf.toString )
			case BinaryExprAST( left, op, right ) =>
				val l = eval( left )

				op match
				{
					case 'in | 'notin =>
						val r = teval( right )

						val res = r exists (_ == l)
// 						{
// 							case seq: collection.Seq[Any] => seq contains l
// 							case set: collection.Set[Any] => set contains l
// 							case map: collection.Map[Any, Any] => map contains l
// 							case _ => RuntimeException( "illegal use of 'in' predicate" )
// 						}

						push( (op == 'notin)^res )
					case '+ =>
						val r = eval( right )
						
						if (l.isInstanceOf[String] || r.isInstanceOf[String])
							push( l.toString + r.toString )
						else if (l.isInstanceOf[Iterable[_]] && r.isInstanceOf[Iterable[_]])
							push( l.asInstanceOf[Iterable[_]] ++ r.asInstanceOf[Iterable[_]] )
						else if (l.isInstanceOf[Number] && r.isInstanceOf[Number])
							push( Math(op, l, r) )
						else
							RuntimeException( "operation '" + op.name + "' not applicable to values: '" + l + "', '" + r + "'" )
					case '< | '> | '<= | '>= =>
						val r = eval( right )

						if (l.isInstanceOf[String] || r.isInstanceOf[String])
						{
						val c = l.toString.compare( r.toString )
						val res =
							op match
							{
								case '< => c < 0
								case '> => c > 0
								case '<= => c <= 0
								case '>= => c >= 0
							}
							
							push( res )
						}
						else
							push( Math(op, l, r) )
					case '== | '/= =>
						val r = eval( right )

						if (l.isInstanceOf[Number] && r.isInstanceOf[Number])
							push( Math(op, l, r) )
						else
							push( if (op == '==) l == r else l != r )
					case _ =>
						push( Math(op, l, eval(right)) )
				}
			case BooleanConnectiveExprAST( left, op, right ) =>
				op match
				{
					case 'or =>
						if (beval( left ))
							push( true )
						else
							push( beval(right) )
					case 'xor =>
						push( beval(left) ^ beval(right) )
					case 'and =>
						if (!beval( left ))
							push( false )
						else
							push( beval(right) )
				}
			case NotExprAST( e ) => push( !beval(e) )
			case VariableExprAST( v ) => push( vars(v).get )
			case CaseFunctionExprAST( m, cases ) => push( new Closure(activations.top, module(m), cases) )
			case f@FunctionExprAST( m, _, _ ) => push( new Closure(activations.top, module(m), List(f)) )
			case ApplyExprAST( f, args, tailrecursive ) =>
				apply( f )
				apply( args )

			val argList = list( args.length )

				pop match
				{
					case ms: MutableSeq[Any] => push( new MutableSeqReference(ms, argList.head.asInstanceOf[Int]) )
					case a: Array[Any] => push( new MutableSeqReference(a, argList.head.asInstanceOf[Int]) )
					case m: Map[Any, Any] => push( new ImmutableMapReference(m, argList.head) )
					case mm: MutableMap[Any, Any] => push( new MutableMapReference(mm, argList.head) )
					case s: ImmutableSeq[_] => push( new ImmutableSeqReference(s, argList.head.asInstanceOf[Int]) )
					case s: collection.Set[Any] => push( s(argList.head) )
					case c: Closure =>
						def occur( argList: List[Any] )
						{
							def findPart: Option[FunctionPartExprAST] =
							{
								for (alt <- c.funcs)
									if (pattern( localScope, argList, alt.parms ))
									{
										for (part <- alt.parts)
											part.cond match
											{
												case None => return Some( part )
												case Some( cond ) =>
													if (beval( cond ))
														return Some( part )
											}

										localScope.clear
									}

								None
							}

							findPart match
							{
								case None => RuntimeException( "function application failure: " + c.funcs + " applied to " + argList )
								case Some( part ) =>
// 									part.locals match
// 									{
// 										case None =>
// 										case Some( l ) =>
// 											for (k <- l)
// 												localScope(k) = new VariableReference
// 									}

									apply( part.body ) match
									{
										case a: List[Any] =>
											localScope.clear
											occur( a )
										case _ =>
									}
							}
						}

 						if (tailrecursive)
							argList
						else
						{
							enterEnvironment( c, c.module )
							occur( argList )
							exitEnvironment
						}
					case b: Function =>
						push( b(argList.toVector) )
					case Constructor( t, n, fields ) =>
						if (fields.length != argList.length) RuntimeException( "argument list length does not match data declaration" )

						push( new Record(t, n, fields, argList) )
					case NativeMethod( o, m ) =>
						m.filter( _.getParameterTypes.length == argList.length ).
							find( cm =>
								(argList zip cm.getParameterTypes).forall(
									{case (a, t) =>
										val cls = a.getClass

										t.getName == "int" && cls.getName == "java.lang.Integer" ||
											t.getName == "double" && cls.getName == "java.lang.Double" ||
											t.isAssignableFrom( cls )
									})
							) match
							{
								case None =>
									m.find( cm =>
										{
										val types = cm.getParameterTypes

												types.length == 1 && types(0).getName == "scala.collection.Seq"
										}) match
										{
											case None => RuntimeException( "no class methods with matching signatures for: " + argList.mkString(", ") )
											case Some( cm ) => push( cm.invoke(o, argList) )
										}
								case Some( cm ) => push( cm.invoke(o, argList.asInstanceOf[List[Object]]: _*) )
							}
					case c: Class[Any] =>
						c.getConstructors.toList.filter( _.getParameterTypes.length == argList.length ).
							find( cm => (argList zip cm.getParameterTypes).forall(
								{case (a, t) =>
									val cls = a.getClass

									t.getName == "int" && cls.getName == "java.lang.Integer" ||
										t.getName == "double" && cls.getName == "java.lang.Double" ||
										t.isAssignableFrom( cls )
								}) ) match
							{
								case None => RuntimeException( "no constructor with matching signatures for: " + argList )
								case Some( cm ) => push( cm.newInstance( argList.asInstanceOf[List[Object]]: _* ) )
							}
					case p: Product =>
						push( p.productElement(argList.head.asInstanceOf[Int]) )
					case o => RuntimeException( "not a function: " + o )
				}
			case UnaryExprAST( op, exp ) =>
				apply( exp )
				
				op match
				{
					case '- => push( Math(op, pop) )
					case Symbol( "pre--" ) =>
						push( assignOperation(rpop, '-, 1) )
					case Symbol( "pre++" ) =>
						push( assignOperation(rpop, '+, 1) )
					case Symbol( "post--" ) =>
						val h = rpop
						val v = h.value
						
						h.assign( Math('-, h.value, 1) )
						push( v )
					case Symbol( "post++" ) =>
						val h = rpop
						val v = h.value
						
						h.assign( Math('+, h.value, 1) )
						push( v )
				}
			case AssignmentExprAST( lhs, op, rhs ) =>
				if (lhs.length != rhs.length)
					RuntimeException( "left hand side must have the same number of elements as the right hand side" )

				var result: Any = null
				
				for ((l, r) <- (lhs map (reval)) zip (rhs map (eval)))
				{
					if (op == '=)
					{
						l.assign( r )
						result = r
					}
					else
						result = assignOperation( l, Symbol(op.toString.charAt(1).toString), r.asInstanceOf[Number] )
				}
				
				push( result )
			case VectorExprAST( l ) =>
				apply( l )
				push( list(l.length).toVector )
			case TupleExprAST( l, r ) =>
				push( (eval(l), eval(r)) )
			case ListComprehensionExprAST( e, p, t, f ) =>
				val o = teval( t )
				val buf = new ListBuffer[Any]
				
				enterScope

				o.foreach
				{ elem =>
					clear( localScope, p )

					if (!unify( localScope, deref(elem), p ))
						RuntimeException( "unification error in list comprehension" )

					if (f == None || beval(f.get))
						buf += eval( e )
				}

				push( buf.toList )
				exitScope
			case ListExprAST( l ) =>
				apply( l )
				push( list(l.length) )
			case ConsExprAST( head, tail ) =>
				val hd = eval( head )
				val tl = eval( tail )
				
				push( tl match
				{
					case t: List[Any] => hd :: t
					case end: Int if hd.isInstanceOf[Int] => hd.asInstanceOf[Int] until end
					case r: Range if hd.isInstanceOf[Int] && tail.isInstanceOf[ConsExprAST] && !tail.asInstanceOf[ConsExprAST].tail.isInstanceOf[ConsExprAST] =>
						hd.asInstanceOf[Int] until r.start by r.end
					case _ => RuntimeException( "not a valid slice or list: " + hd + ":" + tl )
				} )
			case SetExprAST( l ) =>
				apply( l )
				push( list(l.length).toSet )
			case MapExprAST( l ) =>
				apply( l )
				push( list(l.length).asInstanceOf[List[(_, _)]].toMap )
			case UnitExprAST => push( () )
			case NullExprAST => push( null )
			case BlockExprAST( Nil ) => push( () )
			case BlockExprAST( l ) =>
				val it = l.iterator
				var res: Any = ()
				
				enterScope
				
				while (it hasNext)
				{
				val s = it next
				
					if (it.hasNext || !s.isInstanceOf[ExpressionStatementAST])
						apply( s )
					else
						res = apply( s.asInstanceOf[ExpressionStatementAST].e )
				}

				exitScope
				res
			case ConditionalExprAST( cond, no ) =>
				cond find (i => beval( i._1 )) match
				{
					case Some( (_, t) ) => apply( t )
					case None =>
						if (no == None)
							void
						else
							apply( no.get )
				}
			case ForExprAST( p, r, filter, body, e ) =>
				val o = teval( r )

				enterScope

				val env = getEnvironment

				try
				{
					o.foreach
					{ elem =>
						clear( localScope, p )

						if (!unify( localScope, deref(elem), p ))
							RuntimeException( "unification error in for loop" )

						if (filter == None || beval(filter.get))
						{
							try
							{
								exec( body )
							}
							catch
							{
								case _: ContinueThrowable =>
									restoreEnvironment( env )
							}
						}
					}

					if (e != None)
						exec( e.get )
				}
				catch
				{
					case _: BreakThrowable =>
						restoreEnvironment( env )
				}

				exitScope
				void
			case WhileExprAST( cond, body, e ) =>
				void

				val stacksize = stack.size

					try
					{
						while (beval( cond ))
						{
							pop

							try
							{
								apply( body )
							}
							catch
							{
								case _: ContinueThrowable =>
									stack reduceToSize stacksize - 1		// because of the pop inside the while loop
									void
							}
						}

						if (e != None)
						{
							pop
							apply( e.get )
						}
					}
					catch
					{
						case _: BreakThrowable =>
							stack reduceToSize stacksize - 1		// because of the pop inside the while loop
							void
					}
			case DoWhileExprAST( body, cond, e ) =>
				void

				val stacksize = stack.size

				try
				{
					do
					{
						pop

						try
						{
							apply( body )
						}
						catch
						{
							case _: ContinueThrowable =>
								stack reduceToSize stacksize - 1		// because of the pop inside the while loop
								void
						}
					}
					while (beval( cond ))

					if (e != None)
					{
						pop
						apply( e.get )
					}
				}
				catch
				{
					case _: BreakThrowable =>
						stack reduceToSize stacksize - 1		// because of the pop inside the while loop
						void
				}
			case RepeatExprAST( body, cond, e ) =>
				void

				val stacksize = stack.size

					try
					{
						do
						{
							pop
							
							try
							{
								apply( body )
							}
							catch
							{
								case _: ContinueThrowable =>
									stack reduceToSize stacksize - 1		// because of the pop inside the while loop
									void
							}
						}
						while (!beval( cond ))

						if (e != None)
						{
							pop
							apply( e.get )
						}
					}
					catch
					{
						case _: BreakThrowable =>
							stack reduceToSize stacksize - 1		// because of the pop inside the while loop
							void
					}
			case RangeExprAST( f, t, b, inclusive ) =>
				eval( f ) match
				{
					case i: Int if inclusive => push( i to ieval(t) by (if (b == None) 1 else ieval(b.get)) )
					case d: Double if inclusive => push( d to deval(t) by (if (b == None) 1 else deval(b.get)) )
					case i: Int => push( i until ieval(t) by (if (b == None) 1 else ieval(b.get)) )
					case d: Double => push( d until deval(t) by (if (b == None) 1 else deval(b.get)) )
					case _ => RuntimeException( "expected a number as the initial value of a range" )
				}
			case DotExprAST( e, f ) =>
				eval( e ) match
				{
					case r: Record =>
						r.get( f ) match
						{
							case None => RuntimeException( "unknown field: " + f )
							case Some( v ) => push( new ConstantReference("field '" + f + "'", v) )
						}
					case m: Map[Any, Any] => push( new ImmutableMapReference(m, f) )
					case mm: MutableMap[Any, Any] => push( new MutableMapReference(mm, f) )
					case c: Class[Any] =>
						val methods = c.getMethods.toList.filter( m => m.getName == f && (m.getModifiers&Modifier.STATIC) == Modifier.STATIC )

						if (methods isEmpty)
						{
							c.getFields.find( cf => cf.getName == f && (cf.getModifiers&Modifier.STATIC) == Modifier.STATIC ) match
							{
								case None => RuntimeException( "static method or field not found: " + f )
								case Some( field ) => push( field.get(null) )
							}
						}
						else
							push( NativeMethod(null, methods) )
					case m: Module =>
						m.symbols.get( f ) match
						{
							case None => RuntimeException( "'" + f + "' not found in module '" + m.name + "'" )
							case Some( elem ) => push( new ConstantReference("module symbol '" + f + "'", elem) )
						}
					case o =>
						val c = o.getClass
						val methods = c.getMethods.toList.filter( m => m.getName == f && (m.getModifiers&Modifier.STATIC) != Modifier.STATIC )
						
						if (methods isEmpty)
							RuntimeException( "object method '" + f + "' not found in:" + o )

						push( NativeMethod(o, methods) )
				}
			case TypeExprAST( e, t ) =>
				push( typecheck(eval(e), t) )
		}
	}
	
	def clear( map: SymbolMap, p: PatternAST ) =
	{
	var vars: List[String] = Nil
	
		def clear( p: PatternAST )
		{
			p match
			{
				case AliasPatternAST( alias, pat ) =>
					map remove alias
					vars = alias :: vars
					clear( pat )
				case VariablePatternAST( n, _ ) =>
					vars = n :: vars
					map remove n
				case TuplePatternAST( t ) =>
					for (e <- t)
						clear( e )
				case RecordPatternAST( n, l ) =>
					for (e <- l)
						clear( e )
				case ListPatternAST( f ) =>
					for (e <- f)
						clear( e )
				case ConsPatternAST( head, tail ) =>
					clear( head )
					clear( tail )
				case _ =>
			}
		}

		clear( p )
		vars
	}

	def typecheck( a: Any, t: String ) =
		t match
		{
			case "String" => a.isInstanceOf[String]
			case "Integer" => a.isInstanceOf[Int] || a.isInstanceOf[BigInt]
			case "Float" => a.isInstanceOf[Double]
			case "Seq" => a.isInstanceOf[Seq[_]]
			case "List" => a.isInstanceOf[List[_]]
			case "Array" => a.isInstanceOf[Array[_]] || a.isInstanceOf[ArrayBuffer[_]]
			case _ if datatypes.contains( t ) => a.isInstanceOf[Record] && a.asInstanceOf[Record].datatype == t
			case _ => RuntimeException( "unknown type: " + t )
		}
	
	def unify( map: SymbolMap, a: Any, p: PatternAST ): Boolean =
		p match
		{
			case LiteralPatternAST( v ) => a == v
			case UnitPatternAST => a == ()
			case NullPatternAST => a == null
			case AliasPatternAST( alias, pat ) =>
				if (map contains alias)
					a == map(alias) && unify( map, a, pat )
				else
				{
					if (unify( map, a, pat ))
					{
						map(alias) = new ConstantReference( alias, a )
						true
					}
					else
						false
				}
			case VariablePatternAST( "_", t ) => t == None || typecheck( a, t.get )
			case VariablePatternAST( n, t ) =>
				if (map contains n)
					a == deref( map(n) )
				else
				{
					if (t == None || typecheck( a, t.get ))
					{
						map(n) = new ConstantReference( n, a )
						true
					}
					else
						false
				}
			case TuplePatternAST( t ) =>
				a match
				{
					case v: Vector[Any] => v.length == t.length && (v zip t).forall( {case (ve, te) => unify(map, ve, te)} )
					case p: Product => p.productArity == t.length && t.zipWithIndex.forall {case (te, i) => unify( map, p.productElement(i), te )}
					case _ => false
				}
			case RecordPatternAST( n, l ) =>
				a match
				{
					case r: Record => r.name == n && r.args.length == l.length && (r.args zip l).forall( pair => unify(map, pair._1, pair._2) )
					case _ => false
				}
			case ListPatternAST( f ) =>
				a match
				{
					case Nil => f == Nil
					case l: List[Any] if l.length == f.length =>
						(l take f.length zip f).forall( pair => unify(map, pair._1, pair._2) )
					case _ => false
				}
			case ConsPatternAST( head, tail ) =>
				a match
				{
					case Nil => false
					case l: List[Any] => unify( map, l.head, head ) && unify( map, l.tail, tail )
					case _ => false
				}
			case AltPatternAST( alts ) =>
				alts exists (unify( map, a, _ ))
		}

	def pattern( map: SymbolMap, args: List[Any], parms: List[PatternAST] ) =
	{
		def pattern( ah: Any, at: List[Any], ph: PatternAST, pt: List[PatternAST] ): Boolean =
		{
			if (unify( map, ah, ph ))
			{
				if (at == Nil && pt == Nil)
					true
				else if (at != Nil && pt != Nil)
					pattern( at.head, at.tail, pt.head, pt.tail )
				else
				{
					map.clear
					false
				}
			}
			else
			{
				map.clear
				false
			}
		}

		if (args == Nil && parms == Nil)
			true
		else if (args == Nil && parms != Nil)
			pattern( (), Nil, parms.head, parms.tail )
		else if (args != Nil && parms == Nil)
			false
		else
			pattern( args.head, args.tail, parms.head, parms.tail )
	}

	def deref( v: Any ) =
		if (v.isInstanceOf[Reference])
			long2bigint( v.asInstanceOf[Reference].value )
		else
			v
}