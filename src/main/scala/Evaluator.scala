/*     ______            __                                                *\
**    / ____/_  __ ___  / /     FunL Programming Language                  **
**   / __/ / / / / __ \/ /      Copyright (c) 2014, Edward A. Maxedon, Sr. **
**  / /   / /_/ / / / / /__     http://funl-lang.org/                      **
** /_/    \____/_/ /_/____/                                                **
\*                                                                         */

package funl.interp

import java.lang.reflect.{Method, Modifier}

import collection.mutable.{ArrayBuffer, ArrayStack, ListBuffer, HashMap, HashSet, Seq => MutableSeq, Map => MutableMap}
import collection.immutable.{ListMap, Seq => ImmutableSeq, Map => ImmutableMap}
import util.parsing.input.{Reader, CharSequenceReader}
import math._
import compat.Platform._

import funl.lia.{Complex, Math}
import Interpreter._


class Evaluator
{
	class Closure( _referencing: =>Activation, val module: Module, val funcs: List[FunctionExprAST] )
	{
		lazy val referencing = _referencing
		
		def runnable = _runnable( Nil )
		
		def runnable( arg: Any ) =
			arg match
			{
				case arg: List[Any] => _runnable( arg )
				case _ => _runnable( List(arg) )
			}

		private def _runnable( args: List[Any] ) =
			new Runnable
			{
				def run
				{
					invoke( Closure.this, args )( new Environment )
				}
			}

		def callable = _callable( Nil )

		def callable( arg: Any ) =
			arg match
			{
				case arg: List[Any] => _callable( arg )
				case _ => _callable( List(arg) )
			}

		private def _callable( args: List[Any] ) =
			new java.util.concurrent.Callable[Any]
			{
			implicit val env = new Environment
			
				def call =
				{
					invoke( Closure.this, args )
					pop
				}
			}

		override def toString = "<closure>"
	}

	class Activation( val closure: Closure, val referencing: Activation, val module: Module, val scope: ListStack[SymbolMap] = new ListStack ) extends SymbolMapContainer
	{
		// think about copying referencing too
		def copy = new Activation( closure, referencing, module, scope.copy )
		
		def apply( key: String ) = synchronized (scope.top( key ))

		def get( key: String ) = synchronized (scope.top.get( key ))

		def update( key: String, value: Any ) = synchronized (scope.top( scope.top + (key -> value) ))

		def contains( key: String ) = synchronized (scope.top contains key)

		def remove( key: String ) = synchronized (scope.top( scope.top - key ))

		def clear = synchronized (scope.top( symbolMap ))
		
		override def toString = "Activation( " + scope + ", " + referencing + " )"
	}

	class Environment( private[interp] var stack: ListStack[Any] = new ListStack, private[interp] var activations: ListStack[Activation] = new ListStack )
	{
		def copy =
		{
		val a = activations.copy

//			a.top( a.top.copy )
			new Environment( stack.copy, a )
		}
	}

	class Datatype( name: String )
	
	case class Constructor( module: Module, datatype: String, name: String, fields: List[String] )

	case class NativeMethod( o: Any, m: List[Method] )
	
	class Record( val module: Module, val datatype: String, val name: String, val fields: List[String], val args: List[Any] )
	{
		private val map = ListMap[String, Any]( (fields zip args): _* )

		def get( key: String ) = map.get( key )

		override def hashCode = datatype.hashCode ^ name.hashCode ^ fields.hashCode ^ args.hashCode

		override def equals( o: Any ) =
			o match
			{
				case r: Record => module == r.module && name == r.name && args == r.args
				case _ => false
			}
			
		override def toString = name + (if (args isEmpty) "" else args.mkString("( ", ", ", " )"))
	}

	class BreakThrowable extends Throwable

	class ContinueThrowable extends Throwable

	class State( val stack: Int, val activations: Int, val scope: Int )

	private var symbols = symbolMap
	private var sysvars = symbolMap
	
	var last: Option[Any] = None

	def sysvar( k: String )( v: => Any )
	{
		sysvars += k -> new SystemReference( k )( v )
	}

	sysvar( "time" ) {compat.Platform.currentTime}
	sysvar( "timeZone" ) {java.util.TimeZone.getDefault}
	sysvar( "timeZoneOffset" )
		{
		val tz = java.util.TimeZone.getDefault

			tz.getRawOffset + tz.getDSTSavings
		}
	sysvar( "date" ) {new java.util.Date}
	sysvar( "os" ) {System.getProperty( "os.name" )}
	
	def module( m: String ) = synchronized
	{
		symbols.get(m) match
		{
			case None =>
				val res = new Module( m )
			
				symbols += m -> res
				res
			case Some( res ) => res.asInstanceOf[Module]
		}
	}

	def loaded( m: String ) = synchronized {symbols.contains( m ) && symbols(m).isInstanceOf[Module]}
	
	def symbol( m: String, key: String ) = module( m )( key )

	def symbolExists( m: String, key: String ) = module( m ) contains key

	def assign( m: String, key: String, value: Any ) = synchronized
	{
	val syms = module(m)
	
		if (syms contains key)
			RuntimeException( "already declared: " + key )
		else
			syms(key) = value
	}
	
	def assign( module: String, vs: (String, Any)* )
	{
		for ((k, v) <- vs)
			assign( module, k, v )
	}
	
	def function( m: String, n: String, f: Function ) = assign( m, n, f )

	def eval( t: AST )( implicit env: Environment ) =
	{
		apply( t )
		pop
	}

	def pop( implicit env: Environment ) = deref( env.stack.pop )

	def exec( t: AST )( implicit env: Environment )
	{
		apply( t )
		env.stack.pop
	}
	
	def neval( t: AST )( implicit env: Environment ) = eval( t ).asInstanceOf[Number]
	
	def ieval( t: AST )( implicit env: Environment ) = eval( t ).asInstanceOf[Int]
	
	def deval( t: AST )( implicit env: Environment ) = eval( t ).asInstanceOf[Number].doubleValue
	
	def beval( t: AST )( implicit env: Environment ) = eval( t ).asInstanceOf[Boolean]
	
	def reval( t: AST )( implicit env: Environment ) =
	{
		apply( t, createvars = true )
		rpop
	}

	def rpop( implicit env: Environment ) = env.stack.pop.asInstanceOf[Reference]

	def teval( t: AST )( implicit env: Environment ) = traversable( eval(t) )

	def traversable( x: Any ): TraversableOnce[Any] =
		x match
		{
			case tr: TraversableOnce[Any] => tr
			case s: String => s.map( _.toString ).iterator
			case a: Array[Any] => a.iterator
			case o => 	RuntimeException( "non traversable object: " + o )
		}
	
	def assignOperation( r: Reference, op: Symbol, n: Number ) =
	{
	val v = Math( op, r.value, n )
		
		r.assign( v )
		v
	}

	def long2bigint( v: Any ) = if (v.isInstanceOf[Long]) BigInt(v.asInstanceOf[Long]) else v

	def enterActivation( closure: Closure, referencing: Activation, module: Module )( implicit env: Environment )
	{
		env.activations push new Activation( closure, referencing, module )
		enterScope
	}

	def exitActivation( implicit env: Environment ) = env.activations.pop

	def enterScope( implicit env: Environment ) = currentActivation.scope push symbolMap

	def load( m: String )( implicit env: Environment ) =
		if (!loaded( m ))
		{
		val ast = parse( m )

			apply( ast )
			exitActivation
		}

	def loadPredef( m: String )( implicit env: Environment )
	{
		load( PREDEF )

		for ((k, v) <- module( PREDEF ).iterator)
			assign( m, k -> v )
	}

	def currentActivation( implicit env: Environment ) = env.activations.top

	def currentModule( implicit env: Environment ) = env.activations.top.module

	def invoke( c: Closure, argList: List[Any] )( implicit env: Environment )
	{
		def occur( argList: List[Any] )
		{
			def findPart: Option[FunctionPartExprAST] =
			{
				for (alt <- c.funcs)
					if (pattern( currentActivation, argList, alt.parms ))
					{
						for (part <- alt.parts)
							part.cond match
							{
								case None => return Some( part )
								case Some( cond ) =>
									if (beval( cond ))
										return Some( part )
							}

						currentActivation.clear
					}

				None
			}

			findPart match
			{
				case None => RuntimeException( "function application failure: " + c.funcs + " applied to " + argList )
				case Some( part ) =>
					apply( part.body ) match
					{
						case a: List[Any] =>
							currentActivation.clear
							occur( a )
						case _ =>
					}
			}
		}

		enterActivation( c, c.referencing, c.module )
		occur( argList )
		exitActivation
	}
	
	def apply( l: List[AST] )( implicit env: Environment ): Any =
		for (s <- l)
			apply( s )

	def apply( t: AST, createvars: Boolean = false )( implicit env: Environment ): Any =
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
// 						else if (createvars)
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
				a.scope find (_ contains name) match
				{
					case None =>
						if (a.referencing != null)
							vars( a.referencing )
						else
							currentModule.get( name ) match
							{
								case None =>
									if (createvars)
										Some( newVar(name) )
									else
										RuntimeException( "unknown variable: " + name )
								case v => v
							}
					case Some( map ) => Some( map(name) )
				}
			}

			vars( env.activations.top )
		}
		
		def newVar( name: String ) =
		{
		val ref = new VariableReference
			
			currentActivation(name) = ref
			
			ref
		}

		def declare( key: String, value: Any ) =
		{
		val syms = declarationSymbolMapContainer

			if (syms contains key)
				RuntimeException( "already declared: " + key )
			else
				syms(key) = value

			export( key )
		}

		def export( sym: String ) =
			if (topLevel)
				env.activations.top.module.exports.add( sym )
		
		def declarationSymbolMapContainer =
			if (topLevel)
				currentModule
			else
				currentActivation

		def push( v: Any ) = env.stack.push( long2bigint(v) )

		def dpop = pop.asInstanceOf[Double]

		def bpop = pop.asInstanceOf[Boolean]

		def void = env.stack push ()

		def list( len: Int ) =
		{
		val buf = new ListBuffer[Any]

			for (i <- 1 to len)
				buf prepend pop

			buf.toList
		}

		def restoreEnvironment( st: Environment )
		{
			env.stack = st.stack
			env.activations = st.activations
		}

		def exitScope = env.activations.top.scope pop

		def topLevel = env.activations.top.scope.size == 1 && env.activations.top.closure == null

		def forLoop( gen: List[GeneratorAST], body: =>Unit, elseClause: Option[ExprAST] )
		{
			enterScope

		val st = env.copy

			try
			{
				def loop( g: List[GeneratorAST] )
				{
					teval( g.head.traversable ).foreach
					{ elem =>
						if (g eq gen)
							currentActivation.clear
						else
							clear( currentActivation, g.head.pattern )

						if (!unify( currentActivation, deref(elem), g.head.pattern ))
							RuntimeException( "unification error in for loop" )

						if (g.head.filter == None || beval(g.head.filter.get))
							try
							{
								if (g.tail != Nil)
									loop( g.tail )
								else
									body
							}
							catch
							{
								case _: ContinueThrowable =>
									restoreEnvironment( st )
							}
					}
				}
				
				loop( gen )

				if (elseClause != None)
					exec( elseClause.get )
			}
			catch
			{
				case _: BreakThrowable =>
					restoreEnvironment( st )
			}

			exitScope
		}

		def thunk( t: ExprAST ) = new Closure( currentActivation.copy, currentModule, List(FunctionExprAST(Nil, List(FunctionPartExprAST(None, t)))) )

		def iterator( e: ExprAST, gs: List[GeneratorAST] ) =
			new Iterator[Any]
			{
				val ps = gs map (_.pattern) toVector
				val ts = gs map (g => teval(g.traversable)) toVector
				val fs = gs map (_.filter) toVector
				val is = ts map (_.toIterator) toArray
				val len = is.length
				val itenv = new Environment
			// 						var hasNext_ = true
			// 						var next_: Option[Any] = None

				enterActivation( null, currentActivation, currentModule )( itenv )

				def hasNext =
				{
					is.head.hasNext
				}
			// 							for (i <- len - 1 to 0 by -1)
			// 							{
			// 								if (is(i).hasNext)
			// 								{
			// 									it(i).next
			// 								}
			// 								else
			// 							}
			//						}

				def next =
				{
					def _next: Any =
						if (hasNext)
						{
							clear( currentActivation(itenv), ps.head )

							if (!unify( currentActivation(itenv), deref(is.head.next), ps.head ))
								RuntimeException( "unification error in iterator" )

							if (fs.head == None || beval( fs.head.get )( itenv ))
								eval( e )( itenv )
							else
								_next
						}
						else
							RuntimeException( "iterator empty" )

					_next
				}
			}

		t match
		{
			case ModuleAST( m, s ) =>
				if (m != PREDEF)
					loadPredef( m )

				enterActivation( null, null, module(m) )
        currentModule( "_name_" ) = m
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

				val m = module( qual )
				
					if (names eq null)
						for ((k, v) <- m.iterator if synchronized (m.exports contains k))
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
				val method = Class.forName( cls ).getMethod( n, classOf[List[Any]] )

					if ((method.getModifiers&Modifier.STATIC) != Modifier.STATIC) RuntimeException( "function method must be static" )

					declare( a.getOrElse(n), (a => method.invoke(null, a)): Function )
				}
// 			case ConstAST( m, name, expr ) =>
// 				assign( m, name, eval(expr) )
			case VarAST( n, v ) =>
				declare( n, new VariableReference(if (v == None) null else eval(v.get)) )
			case DataAST( n, cs ) =>
				if (!topLevel) RuntimeException( "data declarations are only allowed as module level declarations" )
				
				if (currentModule.datatypes contains n) RuntimeException( "already declared: " + n )

				currentModule.datatypes.add( n )

				for ((name, fields) <- cs)
					if (fields isEmpty)
						declare( name, new Record(currentModule, n, name, Nil, Nil) )
					else
						declare( name, Constructor(currentModule, n, name, fields) )
			case DefAST( name, func ) =>
				val c = declarationSymbolMapContainer.get(name) match
				{
					case None => new Closure( if (topLevel) null else env.activations.top.copy, currentModule, List(func) )
					case Some( c: Closure ) => new Closure( if (topLevel) null else env.activations.top.copy, currentModule, c.funcs :+ func )
					case _ => RuntimeException( "already declared: " + name )
				}

				declarationSymbolMapContainer(name) = c
				c.referencing		// this line should not be removed. it is causing the lazy val to be evaluated
				export( name )
			case ExpressionStatementAST( e ) =>
				last = Some( eval(e) )
			case ValAST( p, e ) =>
				clear( declarationSymbolMapContainer, p ) foreach export

				if (!unify( declarationSymbolMapContainer, eval(e), p ))
					RuntimeException( "unification failure" )
			case SysvarExprAST( s ) =>
				synchronized (sysvars.get( s )) match
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
						val r = eval( right )
						val res =
							if (r.isInstanceOf[collection.Map[Any, Any]])
								r.asInstanceOf[collection.Map[Any, Any]].contains( l )
							else
								traversable( r ) exists (_ == l)

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
					case '== | '!= =>
						val r = eval( right )

						if (l.isInstanceOf[Number] && r.isInstanceOf[Number])
							push( Math(op, l, r) )
						else
							push( if (op == '==) l == r else l != r )
					case _ =>
						push( Math(op, l, eval(right)) )
				}
			case BooleanConnectiveExprAST( left, op, right ) =>
				val l = eval( left )
				
				op match
				{
					case 'or =>
						if (l.isInstanceOf[Boolean])
							if (l.asInstanceOf[Boolean])
								push( true )
							else
								push( beval(right) )
						else
							push( Math(op, l, eval(right)) )
					case 'xor =>
						if (l.isInstanceOf[Boolean])
							push( l.asInstanceOf[Boolean] ^ beval(right) )
						else
							push( Math(op, l, eval(right)) )
					case 'and =>
						if (l.isInstanceOf[Boolean])
							if (!l.asInstanceOf[Boolean])
								push( false )
							else
								push( beval(right) )
						else
							push( Math(op, l, eval(right)) )
				}
			case NotExprAST( e ) => push( !beval(e) )
			case VariableExprAST( v ) => push( vars(v).get )
			case CaseFunctionExprAST( cases ) => push( new Closure(env.activations.top.copy, currentModule, cases) )
			case f@FunctionExprAST( _, _ ) => push( new Closure(env.activations.top.copy, currentModule, List(f)) )
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
						if (tailrecursive)
							argList
						else
							invoke( c, argList )
					case b: Function =>
						push( b(argList) )
					case Constructor( m, t, n, fields ) =>
						if (fields.length != argList.length) RuntimeException( "argument list length does not match data declaration" )

						push( new Record(m, t, n, fields, argList) )
					case NativeMethod( o, m ) =>
						m.filter( _.getParameterTypes.length == argList.length ).
							find( cm =>
								(argList zip cm.getParameterTypes).forall(
									{case (a, t) =>
										val cls = a.getClass

										t.getName == "int" && cls.getName == "java.lang.Integer" ||
											t.getName == "double" && cls.getName == "java.lang.Double" ||
											t.getName == "boolean" && cls.getName == "java.lang.Boolean" ||
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
								case Some( cm ) =>
									push( cm.invoke(o, argList.asInstanceOf[List[Object]]: _*) )
							}
					case c: Class[Any] =>
						c.getConstructors.toList.filter( _.getParameterTypes.length == argList.length ).
							find( cm => (argList zip cm.getParameterTypes).forall(
								{case (a, t) =>
									val cls = a.getClass

									t.getName == "int" && cls.getName == "java.lang.Integer" ||
										t.getName == "double" && cls.getName == "java.lang.Double" ||
										t.getName == "boolean" && cls.getName == "java.lang.Boolean" ||
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
			case IteratorExprAST( e, gs ) =>
				push( iterator(e, gs) )
			case ListComprehensionExprAST( IteratorExprAST(e, gs) ) =>
				push( iterator(e, gs).toStream )
// 				val buf = new ListBuffer[Any]
// 
// 				forLoop( gs, buf += eval(e), None )
// 				push( buf.toList )
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
			case StreamExprAST( head, tail ) =>
				def stream( a: Any ): Stream[Any] =
					a match
					{
						case l: List[Any] => l.toStream
						case s: Stream[Any] => s
						case _ => RuntimeException( "not a valid stream: " + tail )
					}

				val callable = thunk( tail ).callable
				
				push( eval(head) #:: stream(callable.call) )
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
			case ForExprAST( gen, body, e ) =>
				forLoop( gen, exec(body), e )
				void
			case ForeverExprAST( body ) =>
				enterScope
				
				val st = env.copy

				void

				try
				{
					while (true)
					{
						currentActivation.clear
						pop

						try
						{
							apply( body )
						}
						catch
						{
							case _: ContinueThrowable =>
								restoreEnvironment( st )
								void
						}
					}
				}
				catch
				{
					case _: BreakThrowable =>
						restoreEnvironment( st )
						void
				}

				exitScope
			case WhileExprAST( cond, body, e ) =>
				enterScope
				
				val st = env.copy

				void

				try
				{
					while ({	currentActivation.clear; beval( cond )})
					{
						pop

						try
						{
							apply( body )
						}
						catch
						{
							case _: ContinueThrowable =>
								restoreEnvironment( st )
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
						restoreEnvironment( st )
						void
				}

				exitScope
			case DoWhileExprAST( body, cond, e ) =>
				enterScope

				val st = env.copy

				void

				try
				{
					do
					{
						currentActivation.clear
						pop

						try
						{
							apply( body )
						}
						catch
						{
							case _: ContinueThrowable =>
								restoreEnvironment( st )
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
						restoreEnvironment( st )
						void
				}

				exitScope
			case RepeatExprAST( body, cond, e ) =>
				enterScope

				val st = env.copy

				void

				try
				{
					do
					{
						currentActivation.clear
						pop

						try
						{
							apply( body )
						}
						catch
						{
							case _: ContinueThrowable =>
								restoreEnvironment( st )
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
						restoreEnvironment( st )
						void
				}

				exitScope
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
						m.get( f ) match
						{
							case None => RuntimeException( "'" + f + "' not found in module '" + m.name + "'" )
							case Some( elem ) => push( new ConstantReference("module symbol '" + f + "'", elem) )
						}
					case o =>
						if (o == null) RuntimeException( "'" + f + "' not a field of null" )
						
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
	
	def clear( map: SymbolMapContainer, p: PatternAST ) =
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
				case TypePatternAST( pat, _ ) =>
					clear( pat )
				case VariablePatternAST( n ) =>
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
			case "Stream" => a.isInstanceOf[Stream[_]]
			case "Iterator" => a.isInstanceOf[Iterator[_]]
			case "Array" => a.isInstanceOf[Array[_]] || a.isInstanceOf[ArrayBuffer[_]]
			case _ /*if datatypes.contains( t )*/ => a.isInstanceOf[Record] && a.asInstanceOf[Record].datatype == t
			case _ => RuntimeException( "unknown type: " + t )
		}
	
	def unify( map: SymbolMapContainer, a: Any, p: PatternAST ): Boolean =
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
			case TypePatternAST( pat, typename ) =>
				unify( map, a, pat ) && typecheck( a, typename )
			case VariablePatternAST( "_" ) => true
			case VariablePatternAST( n ) =>
				if (map contains n)
					a == deref( map(n) )
				else
				{
					map(n) = new ConstantReference( n, a )
					true
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
			case ListPatternAST( ps ) =>
				a match
				{
					case Nil => ps == Nil
					case l: LSeq if !l.isDefinedAt(ps.length) && l.length == ps.length =>
						(l zip ps).forall( pair => unify(map, pair._1, pair._2) )
					case _ => false
				}
			case ConsPatternAST( head, tail ) =>
				a match
				{
					case Nil => false
					case l: LSeq => unify( map, l.head, head ) && unify( map, l.tail, tail )
					case _ => false
				}
			case AltPatternAST( alts ) =>
				alts exists (unify( map, a, _ ))
		}

	def pattern( map: SymbolMapContainer, args: List[Any], parms: List[PatternAST] ) =
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