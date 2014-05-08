package funl.interp


trait AST

case class ModuleAST( module: Symbol, components: List[ComponentAST] ) extends AST

trait ComponentAST extends AST
case class ImportAST( module: Symbol, name: Symbol ) extends ComponentAST
case class NativeAST( module: Symbol, pkg: String, name: List[(String, Option[Symbol])] ) extends ComponentAST
case class ConstAST( module: Symbol, name: Symbol, const: ExprAST ) extends ComponentAST
case class VarAST( module: Symbol, name: Symbol, init: Option[ExprAST] ) extends ComponentAST
case class DataAST( module: Symbol, name: Symbol, constructors: List[(Symbol, List[Symbol])] ) extends ComponentAST
case class DefAST( module: Symbol, name: Symbol, func: FunctionExprAST ) extends ComponentAST
case class MainAST( module: Symbol, s: StatementAST ) extends ComponentAST

trait StatementAST extends AST
case class ExpressionStatementAST( e: ExprAST ) extends StatementAST
case class ValStatementAST( pat: PatternAST, exp: ExprAST ) extends StatementAST

trait ExprAST extends AST
case object BreakExprAST extends ExprAST
case object ContinueExprAST extends ExprAST
case class IntegerLiteralExprAST( i: Int ) extends ExprAST
case class DoubleLiteralExprAST( d: Double ) extends ExprAST
case class BooleanLiteralExprAST( b: Boolean ) extends ExprAST
case class StringLiteralExprAST( s: String ) extends ExprAST
case class VariableExprAST( module: Symbol, v: Symbol ) extends ExprAST
case class ApplyExprAST( f: ExprAST, args: List[ExprAST], var tailrecursive: Boolean ) extends ExprAST
case class BinaryExprAST( left: ExprAST, op: Symbol, right: ExprAST ) extends ExprAST
case class UnaryExprAST( op: Symbol, exp: ExprAST ) extends ExprAST
case class AssignmentExprAST( lhs: List[ExprAST], op: Symbol, rhs: List[ExprAST] ) extends ExprAST
case class VectorExprAST( l: List[ExprAST] ) extends ExprAST
case class TupleExprAST( l: ExprAST, r: ExprAST ) extends ExprAST
case class ListExprAST( l: List[ExprAST] ) extends ExprAST
case class ConsExprAST( head: ExprAST, tail: ExprAST ) extends ExprAST
case class SetExprAST( l: List[ExprAST] ) extends ExprAST
case class MapExprAST( l: List[TupleExprAST] ) extends ExprAST
case object UnitExprAST extends ExprAST
case object NullExprAST extends ExprAST
case class BlockExprAST( l: List[StatementAST] ) extends ExprAST
case class ConditionalExprAST( cond: List[(ExprAST, ExprAST)], no: Option[ExprAST] ) extends ExprAST
case class ForExprAST( p: PatternAST, r: ExprAST, body: ExprAST, e: Option[ExprAST] ) extends ExprAST
case class WhileExprAST( cond: ExprAST, body: ExprAST, e: Option[ExprAST] ) extends ExprAST
case class DoWhileExprAST( body: ExprAST, cond: ExprAST, e: Option[ExprAST] ) extends ExprAST
case class RepeatExprAST( body: ExprAST, cond: ExprAST, e: Option[ExprAST] ) extends ExprAST
case class RangeExprAST( f: ExprAST, t: ExprAST, b: Option[ExprAST], inclusize: Boolean ) extends ExprAST
case class CaseFunctionExprAST( module: Symbol, cases: List[FunctionExprAST] ) extends ExprAST
case class FunctionExprAST( module: Symbol, parms: List[PatternAST], parts: List[FunctionPartExprAST] ) extends ExprAST
case class FunctionPartExprAST( cond: Option[ExprAST], locals: Option[List[Symbol]], body: ExprAST ) extends ExprAST
case class DotExprAST( e: ExprAST, f: Symbol ) extends ExprAST
case class BooleanConnectiveExprAST( left: ExprAST, op: Symbol, right: ExprAST ) extends ExprAST
case class NotExprAST( exp: ExprAST ) extends ExprAST

trait PatternAST extends AST
case class AliasPatternAST( alias: Symbol, pat: PatternAST ) extends PatternAST
case class IntegerLiteralPatternAST( i: Int ) extends PatternAST
case class DoubleLiteralPatternAST( d: Double ) extends PatternAST
case class BooleanLiteralPatternAST( b: Boolean ) extends PatternAST
case class StringLiteralPatternAST( s: String ) extends PatternAST
case class VariablePatternAST( v: Symbol, t: Option[Symbol] ) extends PatternAST
case class TuplePatternAST( l: List[PatternAST] ) extends PatternAST
case class AltPatternAST( l: List[PatternAST] ) extends PatternAST
case class RecordPatternAST( n: String, l: List[PatternAST] ) extends PatternAST
case class ListPatternAST( l: List[PatternAST] ) extends PatternAST
case class ConsPatternAST( head: PatternAST, tail: PatternAST ) extends PatternAST
case object UnitPatternAST extends PatternAST
case object NullPatternAST extends PatternAST