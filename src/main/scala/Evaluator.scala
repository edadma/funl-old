/*     ______            __                                               *\
**    / ____/_  __ ___  / /     FunL Programming Language                 **
**   / __/ / / / / __ \/ /      Copyright (c) 2014 Edward A. Maxedon, Sr. **
**  / /   / /_/ / / / / /__     http://funl-lang.org/                     **
** /_/    \____/_/ /_/____/                                               **
\*                                                                        */

package funl.interp

import java.io.File
import java.lang.reflect.{Method, Modifier}
import java.util.concurrent.Callable

import collection.mutable.{ArrayBuffer, ArrayStack, ListBuffer, HashMap, HashSet, Seq => MutableSeq, Map => MutableMap}
import collection.immutable.{LinearSeq, Seq => ImmutableSeq, Map => ImmutableMap}
import util.parsing.input.{Reader, CharSequenceReader}
import math._
import compat.Platform._
import util.matching.Regex

import funl.lia.{Complex, Math}
import Interpreter._


class Evaluator
{
	abstract class Closure
	{
		val module: Module
		val funcs: List[FunctionExprAST]
		def invoke( argList: List[Any] )( implicit env: Environment ): Unit

		def curry( args: Any* ) =
			new Closure
			{
				val c = Closure.this
				val module = c.module
				val funcs = c.funcs

				def invoke( argList: List[Any] )( implicit env: Environment ) = c.invoke( (args ++ argList).toList )
			}

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
					invoke( args )( new Environment )
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
			new Callable[Any]
			{
			implicit val env = new Environment

				def call =
				{
					invoke( args )
					pop
				}
			}

		def function0 =
			new scala.runtime.AbstractFunction0[Any]
			{
			implicit val env = new Environment

				def apply =
				{
					invoke( Nil )
					pop
				}
			}

		def function1 =
			new scala.runtime.AbstractFunction1[Any, Any]
			{
			implicit val env = new Environment

				def apply( arg: Any ) =
				{
					invoke( List(arg) )
					pop
				}
			}

		def function2 =
			new scala.runtime.AbstractFunction2[Any, Any, Any]
			{
			implicit val env = new Environment

				def apply( arg1: Any, arg2: Any ) =
				{
					invoke( List(arg1, arg2) )
					pop
				}
			}

		override def toString = "<closure>"
	}

	class BasicClosure( _referencing: =>Activation, val module: Module, val funcs: List[FunctionExprAST] ) extends Closure
	{
		lazy val referencing = _referencing

		def computeReferencing =
		{
			referencing	// this line should not be removed; it forces 'referencing' to be computed
			this
		}

		def invoke( argList: List[Any] )( implicit env: Environment )
		{
			def occur( argList: List[Any] )
			{
				def findPart: Option[FunctionPartExprAST] =
				{
					for (alt <- funcs)
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
					case None => RuntimeException( "function application failure: " + funcs + " applied to " + argList )
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

			enterActivation( BasicClosure.this, referencing, module )

		val st = env.copy

			try
			{
				occur( argList )
			}
			catch
			{
				case ReturnThrowable( ret ) =>
					restoreEnvironment( st )
					push( ret )
			}

			exitActivation
		}
	}

	class Activation( val closure: Closure, val referencing: Activation, val module: Module, val scope: ListStack[SymbolMap] = new ListStack ) extends SymbolMapContainer
	{
		// think about copying referencing too
		def copy: Activation = new Activation( closure, if (referencing ne null) referencing.copy else null, module, scope.copy )

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

	class Record( val module: Module, val datatype: String, val name: String, val fields: List[String], val args: Vector[Any] ) extends Seq[Any]
	{
		private val map = Map[String, Any]( (fields zip args): _* )

		def apply( idx: Int ) = args( idx )

		def iterator = args.iterator

		def get( key: String ) = map.get( key )

		def length = args.length

		override def hashCode = datatype.hashCode ^ name.hashCode ^ fields.hashCode ^ args.hashCode

		override def equals( o: Any ) =
			o match
			{
				case r: Record => module == r.module && name == r.name && args == r.args
				case _ => false
			}

		override def toString = name + (if (args isEmpty) "" else args.map( display(_) ).mkString("(", ", ", ")"))
	}

	class BreakThrowable extends Throwable

	class ContinueThrowable extends Throwable

	case class ReturnThrowable( ret: Any ) extends Throwable

	object NEW

	private var symbols = symbolMap
	private var sysvars = symbolMap

	var last: Option[Any] = None

	def rosysvar( k: String )( v: => Any )
	{
		sysvars += k -> new ReadOnlySystemReference( k )( v )
	}

	def wosysvar( k: String, output: Any => Unit )
	{
		sysvars += k -> new WriteOnlySystemReference( k, output )
	}

	def rwsysvar( k: String, output: Any => Unit )( v: => Any )
	{
		sysvars += k -> new ReadWriteSystemReference( k, output )( v )
	}

	rwsysvar( "path", p => modulePath = p.asInstanceOf[String].split(";").toList )( modulePath map (new File(_).getCanonicalPath) mkString ";" )
	rosysvar( "time" ) {compat.Platform.currentTime}
	rosysvar( "timeZone" ) {java.util.TimeZone.getDefault}
	rosysvar( "timeZoneOffset" )
		{
		val tz = java.util.TimeZone.getDefault

			tz.getRawOffset + tz.getDSTSavings
		}
	rosysvar( "date" ) {new java.util.Date}
	rosysvar( "os" ) {System.getProperty( "os.name" )}
	rosysvar( "user" ) {System.getProperty( "user.name" )}
	rosysvar( "home" ) {System.getProperty( "user.home" )}
	rosysvar( "fs" ) {System.getProperty( "file.separator" )}
	rosysvar( "ls" ) {System.getProperty( "line.separator" )}
	rosysvar( "stdin" ) {scala.io.StdIn.readLine}
	wosysvar( "stdout", println )

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

	def restoreEnvironment( st: Environment )( implicit env: Environment )
	{
		env.stack = st.stack
		env.activations = st.activations
	}

	def push( v: Any )( implicit env: Environment ) = env.stack.push( long2bigint(v) )

	def pop( implicit env: Environment ) = deref( env.stack.pop )

	def exec( t: AST )( implicit env: Environment )
	{
		apply( t )
		env.stack.pop
	}

	def neval( t: AST )( implicit env: Environment ) = eval( t ).asInstanceOf[Number]

	def ieval( t: AST )( implicit env: Environment ) = eval( t ).asInstanceOf[Int]

  def bieval( t: AST )( implicit env: Environment ) = Math.toBigInt( eval(t).asInstanceOf[Number] )

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
			case p: Product => p.productIterator
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

	def clear( map: SymbolMapContainer, p: PatternAST )( implicit env: Environment ) =
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

	def unify( map: SymbolMapContainer, a: =>Any, p: PatternAST )( implicit env: Environment ): Boolean =
		p match
		{
			case LiteralPatternAST( v ) =>
				if (v.isInstanceOf[String] && v.asInstanceOf[String].charAt(0) >= '\ue000')
					RuntimeException( "string interpolation not allowed in patterns" )

				a == v
			case EmptySetPatternAST => a == Set.empty
			case VoidPatternAST => a == ()
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
				if (map.isInstanceOf[Activation])
					varSearch( n, map.asInstanceOf[Activation], false ) match
					{
						case Some( rpat: Record ) if rpat.args.length == 0 =>
							return a match
							{
								case r: Record => r.name == rpat.name && r.args.length == 0
								case _ => false
							}
						case Some( ConstantReference(_, ppat: Product) ) if ppat.productArity == 0 =>
							return a match
							{
								case p: Product => p.productPrefix == ppat.productPrefix && p.productArity == 0
								case _ => false
							}
						case _ =>
					}

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
					case v: Array[Any] => v.length == t.length && (v zip t).forall( {case (ve, te) => unify(map, ve, te)} )
					case p: Product => p.productArity == t.length && t.zipWithIndex.forall {case (te, i) => unify( map, p.productElement(i), te )}
					case _ => false
				}
			case RecordPatternAST( n, l ) =>
				a match
				{
					case r: Record => r.name == n && r.args.length == l.length && (r.args zip l).forall( pair => unify(map, pair._1, pair._2) )
					case p: Product => p.productPrefix == n && p.productArity == l.length && l.zipWithIndex.forall {case (te, i) => unify( map, p.productElement(i), te )}
					case s: String if map.isInstanceOf[Activation] =>
						varSearch( n, map.asInstanceOf[Activation], false ) match
						{
							case Some( r: Reference ) if r.value.isInstanceOf[Regex] => r.value.asInstanceOf[Regex].unapplySeq( s ) match
							{
								case Some( groups ) => (groups zip l).forall( pair => unify(map, pair._1, pair._2) )
								case None => false
							}
							case None => false
						}
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

	def pattern( map: SymbolMapContainer, args: List[Any], parms: List[PatternAST] )( implicit env: Environment ) =
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
/*
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

	val st = env.copy

		try
		{
			occur( argList )
		}
		catch
		{
			case ReturnThrowable( ret ) =>
				restoreEnvironment( st )
				push( ret )
		}

		exitActivation
	}*/

	def varSearch( name: String, a: Activation, createvars: Boolean )( implicit env: Environment ): Option[Any] =
		a.scope find (_ contains name) match
		{
			case None =>
				if (a.referencing != null)
					varSearch( name, a.referencing, createvars )
				else
					currentModule.get( name ) match
					{
						case None =>
							if (createvars)
							{
							val ref = new VariableReference( NEW )

								ref.assign( ref )
								currentActivation(name) = ref
								Some( ref )
							}
							else
								None
						case v => v
					}
			case Some( map ) => Some( map(name) )
		}

	def apply( l: List[AST] )( implicit env: Environment ): Any =
		for (s <- l)
			apply( s )

	def apply( t: AST, createvars: Boolean = false )( implicit env: Environment ): Any =
	{
		def vars( name: String ) = varSearch( name, env.activations.top, createvars )

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

		def exitScope = env.activations.top.scope pop

		def topLevel = env.activations.top.scope.size == 1 && env.activations.top.closure == null

		def forLoop( gen: List[GeneratorAST], body: =>Unit, els: Option[ExprAST] )
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

				if (els != None)
					exec( els.get )
			}
			catch
			{
				case _: BreakThrowable =>
					restoreEnvironment( st )
			}

			exitScope
		}

		def thunk( t: ExprAST ) = (new BasicClosure( currentActivation.copy, currentModule, List(FunctionExprAST(Nil, List(FunctionPartExprAST(None, t)))) )).computeReferencing

		def iterator( e: ExprAST, gs: List[GeneratorAST] ) =
			new Iterator[Any]
			{
				private val ps = gs map (_.pattern) toArray
				private val ts = gs map (_.traversable) toArray
				private val fs = gs map (_.filter) toArray
				private val len = fs.length
				private val is = new Array[Iterator[Any]]( len )
				private val itenv = new Environment
				private var avail = false

				enterActivation( null, currentActivation, currentModule )( itenv )

				def hasNext =
				{
					if (avail)
						true
					else
					{
						def _hasNext( i: Int ): Boolean =
						{
							if (is(i) != null && is(i).hasNext)
							{
								if (i == 0)
									currentActivation(itenv).clear
								else
									clear( currentActivation(itenv), ps(i) )

							// force the iterator's next() to be called, since unit()'s second parameter is call-by-name
							val next = deref( is(i).next )

								if (!unify( currentActivation(itenv), next, ps(i) ))
									RuntimeException( "unification error in iterator" )

								if (fs(i) == None || beval( fs(i).get )( itenv ))
									true
								else
									_hasNext( i )
							}
							else
							{
								if (i == 0 && is(0) != null)
									false
								else if (i == 0 || _hasNext( i - 1 ))
								{
									is(i) = teval( ts(i) )( itenv ).toIterator
									_hasNext( i )
								}
								else
									false
							}
						}

						avail = _hasNext( len - 1 )
						avail
					}
				}

				def next =
					if (hasNext)
					{
						avail = false
						eval( e )( itenv )
					}
					else
						RuntimeException( "iterator empty" )
			}

		def section( l: ExprAST, op: Symbol, r: ExprAST ) =
			op match
			{
				case ': => ConsExprAST( l, r )
				case '# => StreamExprAST( l, r )
				case _ => BinaryExprAST( l, op, r )
			}

		def rewrap( objs: List[Any], types: Array[Class[_]] ) =
		{
		val varargs = types.length > 0 && types(types.length - 1).getName == "scala.collection.Seq"

			def wrap( l: List[Any], idx: Int ): List[Any] =
				if (varargs && idx == types.length - 1)
					List( l )
				else if (!varargs && idx == types.length)
					Nil
				else
					((l.head, types(idx).getName) match
						{
							case (x: Number, "java.lang.Number") => x
							case (x: BigInt, _) if x.isValidLong => x.longValue.asInstanceOf[AnyRef]
							case (x: ScalaNumber, _) => x.underlying
							case (x: Closure, "funl.interp.Evaluator$Closure") => x
							case (x: Closure, "scala.Function0") => x.function0
							case (x: Closure, "scala.Function1") => x.function1
							case (x: Closure, "scala.Function2") => x.function2
							case (x, _) => x.asInstanceOf[AnyRef]
						}) :: wrap( l.tail, idx + 1 )

			wrap( objs, 0 ).asInstanceOf[Seq[Object]]
		}

		def assignable( objs: List[Any], types: Array[Class[_]] ) =
		{
		val len = objs.length
		val varargs = types.length > 0 && types(types.length - 1).getName == "scala.collection.Seq"

			if (varargs && len < types.length - 1 || !varargs && len != types.length)
				false
			else
			{
				(objs zip types zipWithIndex).forall(
					{case ((a, t), idx) =>
						val cls = a.getClass

						t.getName == "int" && cls.getName == "java.lang.Integer" ||
							t.getName == "double" && cls.getName == "java.lang.Double" ||
							t.getName == "boolean" && cls.getName == "java.lang.Boolean" ||
							t.getName == "long" && (cls.getName == "java.lang.Integer" || cls.getName == "scala.math.BigInt") ||
							(t.getName == "scala.Function0" || t.getName == "scala.Function1" || t.getName == "scala.Function2") && a.isInstanceOf[Closure] ||
							t.getName == "scala.collection.Seq" && idx == types.length - 1 ||
							t.isAssignableFrom( cls )
					} )
			}
		}

		def compound( l: List[StatementAST] ): Any =
			if (l.tail == Nil)
				apply( l.head.asInstanceOf[ExpressionStatementAST].e )
			else
			{
				apply( l.head )
				compound( l.tail )
			}

		t match
		{
			case ModuleAST( m, s ) =>
				if (m != PREDEF)
					loadPredef( m )

				enterActivation( null, null, module(m) )
        currentModule( "_name_" ) = m
				apply( s )
			case DeclarationBlockAST( s ) =>
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
				val method = Class.forName( cls ).getMethod( n, classOf[Any] )

					if ((method.getModifiers&Modifier.STATIC) != Modifier.STATIC) RuntimeException( "function method must be static" )

					declare( a.getOrElse(n), (a => method.invoke(null, a.asInstanceOf[Object])): Function )
				}
			case VarAST( n, v ) =>
				declare( n, new VariableReference(if (v == None) null else eval(v.get)) )
			case DataAST( n, cs ) =>
				if (!topLevel) RuntimeException( "data declarations are only allowed as module level declarations" )

				if (currentModule.datatypes contains n) RuntimeException( "already declared: " + n )

				currentModule.datatypes.add( n )

				for ((name, fields) <- cs)
					if (fields isEmpty)
						declare( name, new Record(currentModule, n, name, Nil, Vector()) )
					else
						declare( name, Constructor(currentModule, n, name, fields) )
			case DefAST( name, func ) =>
				val c = declarationSymbolMapContainer.get(name) match
				{
					case None => new BasicClosure( if (topLevel) null else env.activations.top.copy, currentModule, List(func) )
					case Some( c: Closure ) => new BasicClosure( if (topLevel) null else env.activations.top.copy, currentModule, c.funcs :+ func )
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
			case ReturnExprAST( ret ) =>
				throw new ReturnThrowable( eval(ret) )
			case SectionExprAST( op ) =>
				push( new BasicClosure(null, currentModule,
					List(FunctionExprAST(List(VariablePatternAST("$a"), VariablePatternAST("$b")),
						List(FunctionPartExprAST(None, section(VariableExprAST("$a"), op, VariableExprAST("$b"))))))) )
			case LeftSectionExprAST( e, op ) =>
				push( (new BasicClosure(currentActivation.copy, currentModule,
					List(FunctionExprAST(List(VariablePatternAST("$a")),
						List(FunctionPartExprAST(None, section(e, op, VariableExprAST("$a")))))))).computeReferencing )
			case RightSectionExprAST( op, e ) =>
				push( (new BasicClosure(currentActivation.copy, currentModule,
					List(FunctionExprAST(List(VariablePatternAST("$a")),
						List(FunctionPartExprAST(None, section(VariableExprAST("$a"), op, e))))))).computeReferencing )
			case LiteralExprAST( v ) => push( v )
			case StringLiteralExprAST( s ) => push( s )
			case InterpolationExprAST( l ) =>
				val buf = new StringBuilder

				for (e <- l)
					buf append (display( eval(e) ))

				push( buf.toString )
			case BinaryExprAST( left, op, right ) =>
				val l = eval( left )

				op match
				{
					case 'rotateright | 'rotateleft | 'shiftright | 'shiftleft =>
						val bits = l.asInstanceOf[Number].intValue
						val k = ieval( right )

						push( op match
						{
							case 'rotateright => Integer.rotateRight( bits, k )
							case 'rotateleft => Integer.rotateRight( bits, k )
							case 'shiftright => bits >>> k
							case 'shiftleft => bits << k
						} )
					case 'in | 'notin =>
						val r = eval( right )
						val res =
							if (r.isInstanceOf[collection.Map[Any, Any]])
								r.asInstanceOf[collection.Map[Any, Any]].contains( l )
							else if (r.isInstanceOf[String])
								r.asInstanceOf[String].indexOf( l.toString ) > -1
							else
								traversable( r ) exists (_ == l)

						push( (op == 'notin)^res )
					case '+ =>
						val r = eval( right )

						if (l.isInstanceOf[String] || r.isInstanceOf[String])
							push( display(l) + display(r) )
						else if (l.isInstanceOf[collection.Map[_, _]] && r.isInstanceOf[collection.Map[_, _]])
							push( l.asInstanceOf[collection.Map[_, _]] ++ r.asInstanceOf[collection.Map[_, _]] )
						else if (l.isInstanceOf[collection.Map[_, _]] && r.isInstanceOf[Iterable[_]])
							push( l.asInstanceOf[collection.Map[Any, Any]] ++ r.asInstanceOf[Iterable[Vector[Any]]].map(v => (v(0), v(1))) )
						else if (l.isInstanceOf[Iterable[_]] && r.isInstanceOf[Iterable[_]])
							push( l.asInstanceOf[Iterable[_]] ++ r.asInstanceOf[Iterable[_]] )
						else
							push( Math(op, l, r) )
					case '* =>
						val r = eval( right )

						if (l.isInstanceOf[String])
							push( l.asInstanceOf[String]*r.asInstanceOf[Int] )
						else if (r.isInstanceOf[String])
							push( r.asInstanceOf[String]*l.asInstanceOf[Int] )
						else
							push( Math(op, l, r) )
					case '< | '> | '<= | '>= =>
						def comp( c: Int ) =
							op match
							{
								case '< => c < 0
								case '> => c > 0
								case '<= => c <= 0
								case '>= => c >= 0
							}

						val r = eval( right )

						if (l.isInstanceOf[String] || r.isInstanceOf[String])
							push( comp(l.toString.compare(r.toString)) )
						else if (l.isInstanceOf[Seq[Any]] && r.isInstanceOf[Seq[Any]])
							push( comp(lexicographicalCompare(l.asInstanceOf[Seq[Any]], r.asInstanceOf[Seq[Any]])) )
						else if (l.isInstanceOf[Product] && r.isInstanceOf[Product])
							push( comp(lexicographicalCompare(l.asInstanceOf[Product].productIterator.toSeq, r.asInstanceOf[Product].productIterator.toSeq)) )
						else
							push( Math(op, l, r) )
					case '== | '!= =>
						val r = eval( right )

						if (l.isInstanceOf[Number] && r.isInstanceOf[Number])
							push( Math(op, l, r) )
						else
							push( if (op == '==) l == r else l != r )
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
					case _ =>
						push( Math(op, l, eval(right)) )
				}
			case NotExprAST( e ) =>
				val o = eval( e )

				if (o.isInstanceOf[Boolean])
					push( !o.asInstanceOf[Boolean] )
				else
					push( Math('not, o) )
			case VariableExprAST( v ) =>
				push( vars(v).getOrElse(RuntimeException( "unknown variable: " + v )) )
			case CaseFunctionExprAST( cases ) => push( (new BasicClosure(currentActivation.copy, currentModule, cases)).computeReferencing )
			case f@FunctionExprAST( _, _ ) => push( (new BasicClosure(currentActivation.copy, currentModule, List(f))).computeReferencing )
			case ApplyExprAST( f, args, tailrecursive ) =>
				apply( f, createvars )
				apply( args )

			val argListLength = args.length
			val argList = list( argListLength )

				pop match
				{
					case ref: Reference =>
						val seq = new ArrayBuffer[Any]
						ref.assign( seq )

						argList match
						{
							case List( index: Int ) =>
								push( new MutableSeqReference(seq, index) )
							case List( range: Range ) =>
								push( new MutableSeqRangeReference(seq, range) )
						}
					case ms: MutableSeq[MutableSeq[Any]] if argList.length == 2 =>
						argList match
						{
							case List( r: Int, c: Int ) => push( new Mutable2DSeqReference(ms, r, c) )
							case List( r: Range, c: Int ) => push( new MutableSeqRowRangeReference(ms, r, c) )
							case List( r: Int, c: Range ) => push( new MutableSeqColumnRangeReference(ms, r, c) )
							case List( r: Range, c: Range ) => push( new MutableSeq2DRangeReference(ms, r, c) )
						}
					case ms: MutableSeq[Any] if argList.head.isInstanceOf[Int] => push( new MutableSeqReference(ms, argList.head.asInstanceOf[Int]) )
          case ms: MutableSeq[Any] => push( new MutableSeqRangeReference(ms, argList.head.asInstanceOf[Range]) )
					case a: Array[Any] => push( new MutableSeqReference(a, argList.head.asInstanceOf[Int]) )
					case m: Map[Any, Any] => push( new ImmutableMapReference(m, argList.head) )
					case mm: MutableMap[Any, Any] => push( new MutableMapReference(mm, argList.head) )
					case ms: ImmutableSeq[ImmutableSeq[_]] if argList.length == 2 =>
						argList match
						{
							case List( r: Int, c: Int ) => push( new Immutable2DSeqReference(ms, r, c) )
							case List( r: Range, c: Int ) => push( new ImmutableSeqRowRangeReference(ms, r, c) )
							case List( r: Int, c: Range ) => push( new ImmutableSeqColumnRangeReference(ms, r, c) )
							case List( r: Range, c: Range ) => push( new ImmutableSeq2DRangeReference(ms, r, c) )
						}
					case s: ImmutableSeq[_] if argList.head.isInstanceOf[Int] => push( new ImmutableSeqReference(s, argList.head.asInstanceOf[Int]) )
          case s: ImmutableSeq[_] => push( new ImmutableSeqRangeReference(s, argList.head.asInstanceOf[Range]) )
					case s: collection.Set[Any] => push( s(argList.head) )
					case c: Closure =>
						if (tailrecursive)
							argList
						else
							c.invoke( argList )
					case b: Function =>
						def conv( l: List[Any] ): List[Any] =
							l match
							{
								case Nil => Nil
								case (c: Closure) :: tail =>
									c.funcs.head.parms.length match
									{
										case 0 => c.function0 :: conv( tail )
										case 1 => c.function1 :: conv( tail )
										case 2 => c.function2 :: conv( tail )
									}
								case head :: tail => head :: conv( tail )
							}

						if (argListLength == 1)
							push( b(conv(argList).head) )
						else
							push( b(ArgList(conv(argList))) )
					case Constructor( m, t, n, fields ) =>
						if (fields.length != argList.length) RuntimeException( "argument list length does not match data declaration" )

						push( new Record(m, t, n, fields, argList.toVector) )
					case r :Record =>
            argList match
            {
              case List( idx: Int ) => push( r(idx) )
              case List( field: String ) => push( r.get(field).get )
            }
					case NativeMethod( o, m ) =>
						m.find( cm => assignable(argList, cm.getParameterTypes) ) match
							{
								case None => RuntimeException( "no methods with matching signatures for: " + m.head.getName + ": " + argList.mkString(", ") )
								case Some( cm ) =>
									push( cm.invoke(o, rewrap(argList, cm.getParameterTypes): _*) )
							}
					case c: Class[Any] =>
						c.getConstructors.toList.find( cm => assignable(argList, cm.getParameterTypes) ) match
							{
								case None => RuntimeException( "no constructor with matching signatures for: " + argList )
								case Some( cm ) => push( cm.newInstance(rewrap(argList, cm.getParameterTypes): _*) )
							}
					case p: Product =>
						push( p.productElement(argList.head.asInstanceOf[Int]) )
					case s: String =>
						push( s.charAt(argList.head.asInstanceOf[Int]).toString )
					case o => RuntimeException( "not callable: " + o )
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
					op match
					{
						case "=" =>
							l.assign( r )
							result = r
						case "-=" if l.value.isInstanceOf[collection.generic.Shrinkable[Any]] =>
							l.value.asInstanceOf[collection.generic.Shrinkable[Any]] -= r
							result = l.value
            case "--=" if l.value.isInstanceOf[collection.generic.Shrinkable[Any]] =>
              l.value.asInstanceOf[collection.generic.Shrinkable[Any]] --= r.asInstanceOf[scala.collection.TraversableOnce[Any]]
              result = l.value
            case "+=" if l.value.isInstanceOf[collection.generic.Growable[Any]] =>
              l.value.asInstanceOf[collection.generic.Growable[Any]] += r
              result = l.value
            case "++=" if l.value.isInstanceOf[collection.generic.Growable[Any]] =>
              l.value.asInstanceOf[collection.generic.Growable[Any]] ++= r.asInstanceOf[scala.collection.TraversableOnce[Any]]
              result = l.value
						case _ =>
							result = assignOperation( l, Symbol(op.substring(0, op.length - 1)), r.asInstanceOf[Number] )
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
      case SetComprehensionExprAST( IteratorExprAST(e, gs) ) =>
        push( iterator(e, gs).toSet )
			case ListExprAST( l ) =>
				apply( l )
				push( list(l.length) )
			case ConsExprAST( head, tail ) =>
				val hd = eval( head )
				val tl = eval( tail )

				push( tl match
				{
					case t: LinearSeq[Any] => hd :: t.toList
					case end: Int if hd.isInstanceOf[Int] => hd.asInstanceOf[Int] until end
					case r: Range if hd.isInstanceOf[Int] && tail.isInstanceOf[ConsExprAST] && !tail.asInstanceOf[ConsExprAST].tail.isInstanceOf[ConsExprAST] =>
						hd.asInstanceOf[Int] until r.start by r.end
					case _ => RuntimeException( "not a valid slice or list: " + hd + ":" + tl )
				} )
			case StreamExprAST( head, tail ) =>
				def stream( a: Any ): Stream[Any] =
				{
					a match
					{
						case s: Stream[Any] => s
						case l: Seq[Any] => l.toStream
						case _ => RuntimeException( "not a valid stream: " + tail )
					}
				}

				val callable = thunk( tail ).callable

				push( eval(head) #:: stream(callable.call) )
			case SetExprAST( l ) =>
				apply( l )
				push( list(l.length).toSet )
			case MapExprAST( l ) =>
				apply( l )
				push( list(l.length).asInstanceOf[List[(_, _)]].toMap )
			case VoidExprAST => push( () )
			case NullExprAST => push( null )
      case EmptyMapExprAST => push( Map() )
			case BlockExprAST( Nil ) => push( () )
			case BlockExprAST( l ) =>
				var res: Any = null

				enterScope
				res = compound( l )
				exitScope
				res
			case CompoundExprAST( l ) =>
				compound( l )
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
			case RepeatExprAST( count, body, e ) =>
				enterScope

				val st = env.copy

				void

				try
				{
					for (_ <- 1 to ieval(count))
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
			case DoUntilExprAST( body, cond, e ) =>
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
          case i: BigInt if inclusive => push( i to bieval(t) by (if (b == None) 1 else bieval(b.get)) )
          case i: BigInt => push( i until bieval(t) by (if (b == None) 1 else bieval(b.get)) )
					case _ => RuntimeException( "expected a number as the initial value of a range" )
				}
			case UnboundedStreamExprAST( f, b ) =>
				val by: Number = b match
					{
						case None => 1
						case Some( e ) => neval( e )
					}

				push( Stream.iterate(eval(f))
					{	v =>
						Math( '+, v, by )
					} )
			case DotExprAST( e, f, lookup ) =>
				eval( e ) match
				{
					case r: Record if lookup =>
						r.get( f ) match
						{
							case None => RuntimeException( "unknown field: " + f )
							case Some( v ) => push( new ConstantReference("field '" + f + "'", v) )
						}
					case m: Map[Any, Any] if lookup => push( new ImmutableMapReference(m, f) )
					case mm: MutableMap[Any, Any] if lookup => push( new MutableMapReference(mm, f) )
					case c: Class[Any] if lookup =>
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

	def typecheck( a: Any, t: String ) =
		t match
		{
			case "String" => a.isInstanceOf[String]
			case "Integer" => a.isInstanceOf[Int] || a.isInstanceOf[BigInt]
      case "Rational" => a.isInstanceOf[funl.lia.Rational]
			case "Float" => a.isInstanceOf[Double] ||  a.isInstanceOf[BigDecimal]
			case "Seq" => a.isInstanceOf[Seq[_]]
			case "List" => a.isInstanceOf[List[_]]
			case "Stream" => a.isInstanceOf[Stream[_]]
			case "Iterable" => a.isInstanceOf[Iterable[_]]
			case "Iterator" => a.isInstanceOf[Iterator[_]]
			case "Product" => a.isInstanceOf[Product]
			case "Tuple"|"Vector" => a.isInstanceOf[Vector[_]]
			case "Array" => a.isInstanceOf[Array[_]] || a.isInstanceOf[ArrayBuffer[_]]
			case _ /*if datatypes.contains( t )*/ => a.isInstanceOf[Record] && a.asInstanceOf[Record].datatype == t
			case _ => RuntimeException( "unknown type: " + t )
		}

	def deref( v: Any ) =
		if (v.isInstanceOf[Reference])
			long2bigint( v.asInstanceOf[Reference].value )
		else
			v
}
