package slox

import slox.Expr.Ternary
import slox.Expr.Grouping
import slox.Expr.Literal
import slox.Expr.Unary
import slox.Expr.Binary

//TODO: Is Matchable the right type or do we need to be more specific?
class Interpreter extends Expr.Visitor[Matchable]:

  override def visitTernaryExpr(expr: Ternary): Matchable = ???

  override def visitBinaryExpr(expr: Binary): Matchable =
    val left = evaluate(expr.left)
    val right = evaluate(expr.right)
    (expr.operator.`type`: @unchecked) match
      case TokenType.MINUS =>
        val (l, r) = checkNumberOperands(expr.operator, left, right)
        l - r
      case TokenType.SLASH =>
        val (l, r) = checkNumberOperands(expr.operator, left, right)
        l / r
      case TokenType.STAR =>
        val (l, r) = checkNumberOperands(expr.operator, left, right)
        l * r
      case TokenType.PLUS =>
        (left, right) match
          case (l: Double, r: Double) => l + r
          case (l: String, r: String) => l + r
          case _ =>
            throw new RuntimeException(
              "Operands must be two numbers or two strings."
            )
      case TokenType.GREATER =>
        val (l, r) = checkNumberOperands(expr.operator, left, right)
        l > r
      case TokenType.GREATER_EQUAL =>
        val (l, r) = checkNumberOperands(expr.operator, left, right)
        l >= r
      case TokenType.LESS =>
        val (l, r) = checkNumberOperands(expr.operator, left, right)
        l < r
      case TokenType.LESS_EQUAL =>
        val (l, r) = checkNumberOperands(expr.operator, left, right)
        l <= r
      case TokenType.BANG_EQUAL  => !isEqual(left, right)
      case TokenType.EQUAL_EQUAL => isEqual(left, right)

  override def visitGroupingExpr(expr: Grouping): Matchable = evaluate(
    expr.expression
  )

  override def visitLiteralExpr(expr: Literal): Matchable = expr.value

  override def visitUnaryExpr(expr: Unary): Matchable =
    val right = evaluate(expr.right)
    (expr.operator.`type`: @unchecked) match
      case TokenType.MINUS =>
        val r = checkNumberOperand(expr.operator, right)
        -r
      case TokenType.BANG => !isTruthy(right)

  def checkNumberOperand(op: Token, left: Matchable): Double =
    left match
      case l: Double => l
      case _      => throw new RuntimeError(op, "Operand must be a number")

  def checkNumberOperands(
      op: Token,
      left: Matchable,
      right: Matchable
  ): (Double, Double) =
    (left, right) match
      case (l: Double, r: Double) => (l, r)
      case _ => throw new RuntimeError(op, "Operands must be numbers.")

  def isTruthy(value: Matchable): Boolean = value match
    case null         => false
    case (b: Boolean) => b
    case _            => true

  def isEqual(a: Matchable, b: Matchable): Boolean =
    (a, b) match
      case (null, null) => true
      case (null, _)    => false
      case (x, y)       => x == y

  def stringify(m: Matchable): String = 
    m match
      case null => "nil"
      case d: Double =>
        val text = d.toString()
        if text.endsWith(".0") then text.substring(0, text.length() - 2) else text
      case _ => m.toString()

  def evaluate(expr: Expr): Matchable = expr.accept(this)

  def interpret(expr: Expr) = 
    try
      val value = evaluate(expr)
      println(stringify(value))
    catch 
      case (error: RuntimeError) => runtimeError(error)
end Interpreter
