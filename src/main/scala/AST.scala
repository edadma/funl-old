/*     ______            __                                     *\
**    / ____/_  __ ___  / /     FunL Programming Language       **
**   / __/ / / / / __ \/ /      (c) 2014 Edward A. Maxedon, Sr. **
**  / /   / /_/ / / / / /__     http://funl-lang.org/           **
** /_/    \____/_/ /_/____/                                     **
\*                                                              */

package funl.interp


trait AST

case class ModuleAST( module: String, statements: List[StatementAST] ) extends AST

trait StatementAST extends AST

case class DeclarationBlockAST( decls: List[DeclarationStatementAST] ) extends StatementAST

trait DeclarationStatementAST extends StatementAST
case class ImportAST( qual: String, names: List[(String, Option[String])] ) extends DeclarationStatementAST
case class NativeAST( pkg: String, name: List[(String, Option[String])] ) extends DeclarationStatementAST
case class FunctionAST( cls: String, name: List[(String, Option[String])] ) extends DeclarationStatementAST
case class ValAST( pat: PatternAST, exp: ExprAST ) extends DeclarationStatementAST
case class VarAST( name: String, init: Option[ExprAST] ) extends DeclarationStatementAST
case class DataAST( name: String, constructors: List[(String, List[String])] ) extends DeclarationStatementAST
case class DefAST( name: String, func: FunctionExprAST ) extends DeclarationStatementAST

case class ExpressionStatementAST( e: ExprAST ) extends StatementAST

case class GeneratorAST( pattern: PatternAST, traversable: ExprAST, filter: Option[ExprAST] ) extends AST

trait ExprAST extends AST
case class SectionExprAST( op: Symbol ) extends ExprAST
case class LeftSectionExprAST( e: ExprAST, op: Symbol ) extends ExprAST
case class RightSectionExprAST( op: Symbol, e: ExprAST ) extends ExprAST
case class IteratorExprAST( e: ExprAST, gen: List[GeneratorAST] ) extends ExprAST
case class ListComprehensionExprAST( iterator: IteratorExprAST ) extends ExprAST
case class TypeExprAST( e: ExprAST, t: String ) extends ExprAST
case class SysvarExprAST( name: String ) extends ExprAST
case class TestExprAST( name: String ) extends ExprAST
case object BreakExprAST extends ExprAST
case object ContinueExprAST extends ExprAST
case class ReturnExprAST( ret: ExprAST ) extends ExprAST
case class LiteralExprAST( v: Any ) extends ExprAST
case class StringLiteralExprAST( s: String ) extends ExprAST
case class VariableExprAST( name: String ) extends ExprAST
case class ApplyExprAST( f: ExprAST, args: List[ExprAST], var tailrecursive: Boolean ) extends ExprAST
case class BinaryExprAST( left: ExprAST, op: Symbol, right: ExprAST ) extends ExprAST
case class UnaryExprAST( op: Symbol, exp: ExprAST ) extends ExprAST
case class AssignmentExprAST( lhs: List[ExprAST], op: Symbol, rhs: List[ExprAST] ) extends ExprAST
case class VectorExprAST( l: List[ExprAST] ) extends ExprAST
case class TupleExprAST( l: ExprAST, r: ExprAST ) extends ExprAST
case class ListExprAST( l: List[ExprAST] ) extends ExprAST
case class ConsExprAST( head: ExprAST, tail: ExprAST ) extends ExprAST
case class StreamExprAST( head: ExprAST, tail: ExprAST ) extends ExprAST
case class SetExprAST( l: List[ExprAST] ) extends ExprAST
case class MapExprAST( l: List[TupleExprAST] ) extends ExprAST
case object VoidExprAST extends ExprAST
case object NullExprAST extends ExprAST
case class BlockExprAST( l: List[StatementAST] ) extends ExprAST
case class ConditionalExprAST( cond: List[(ExprAST, ExprAST)], no: Option[ExprAST] ) extends ExprAST
case class ForeverExprAST( body: ExprAST ) extends ExprAST
case class ForExprAST( gen: List[GeneratorAST], body: ExprAST, e: Option[ExprAST] ) extends ExprAST
case class WhileExprAST( cond: ExprAST, body: ExprAST, e: Option[ExprAST] ) extends ExprAST
case class DoWhileExprAST( body: ExprAST, cond: ExprAST, e: Option[ExprAST] ) extends ExprAST
case class RepeatExprAST( body: ExprAST, cond: ExprAST, e: Option[ExprAST] ) extends ExprAST
case class RangeExprAST( f: ExprAST, t: ExprAST, b: Option[ExprAST], inclusize: Boolean ) extends ExprAST
case class UnboundedStreamExprAST( f: ExprAST, b: Option[ExprAST] ) extends ExprAST
case class CaseFunctionExprAST( cases: List[FunctionExprAST] ) extends ExprAST
case class FunctionExprAST( parms: List[PatternAST], parts: List[FunctionPartExprAST] ) extends ExprAST
case class FunctionPartExprAST( cond: Option[ExprAST], body: ExprAST ) extends ExprAST
case class DotExprAST( e: ExprAST, f: String ) extends ExprAST
case class NotExprAST( exp: ExprAST ) extends ExprAST

trait PatternAST extends AST
case class AliasPatternAST( alias: String, pat: PatternAST ) extends PatternAST
case class TypePatternAST( pat: PatternAST, typename: String ) extends PatternAST
case class LiteralPatternAST( v: Any ) extends PatternAST
case class VariablePatternAST( v: String ) extends PatternAST
case class TuplePatternAST( l: List[PatternAST] ) extends PatternAST
case class AltPatternAST( l: List[PatternAST] ) extends PatternAST
case class RecordPatternAST( n: String, l: List[PatternAST] ) extends PatternAST
case class ListPatternAST( l: List[PatternAST] ) extends PatternAST
case class ConsPatternAST( head: PatternAST, tail: PatternAST ) extends PatternAST
case object EmptySetPatternAST extends PatternAST
case object VoidPatternAST extends PatternAST
case object NullPatternAST extends PatternAST