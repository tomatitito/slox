package slox

import slox.Expr.Binary
import slox.Expr.Grouping
import slox.Expr.Unary
import slox.Expr.Literal

class AstPrinter extends Expr.Visitor[String]:
  def print(expr: Expr): String = expr.accept(this)
  def visitBinaryExpr(expr: Binary): String = parenthesize(expr.operator.lexeme, expr.left, expr.right)
  def visitGroupingExpr(expr: Grouping): String = parenthesize("group", expr.expression)
  def visitLiteralExpr(expr: Literal): String = if expr.value == null then "nil" else parenthesize(expr.value.toString)
  def visitUnaryExpr(expr: Unary): String = parenthesize(expr.operator.lexeme, expr.right)

  private def parenthesize(name: String, exprs: Expr*): String = {
    val builder = new StringBuilder()
    builder.append("(").append(name)
    for (expr <- exprs) {
      builder.append(" ").append(expr.accept(this))
    }
    builder.append(")")
    builder.toString()
  }

object AstPrinter:
  def main(args: Array[String]): Unit =
    // Build test expression from the book: -123 * (45.67)
    val expression = Binary(
      Unary(
        Token(TokenType.MINUS, "-", null, 1),
        Literal(123)
      ),
      Token(TokenType.STAR, "*", null, 1),
      Grouping(Literal(45.67))
    )

    val printer = new AstPrinter()
    println("Expression: -123 * (45.67)")
    println("AST:        " + printer.print(expression))
    println()

    // Additional test cases
    println("More examples:")
    println("--------------")

    // Simple binary: 1 + 2
    val simple = Binary(
      Literal(1),
      Token(TokenType.PLUS, "+", null, 1),
      Literal(2)
    )
    println("1 + 2       => " + printer.print(simple))

    // Grouping: (1 + 2)
    val grouped = Grouping(
      Binary(
        Literal(1),
        Token(TokenType.PLUS, "+", null, 1),
        Literal(2)
      )
    )
    println("(1 + 2)     => " + printer.print(grouped))

    // Nested: (1 + 2) * (4 - 3)
    val nested = Binary(
      Grouping(
        Binary(
          Literal(1),
          Token(TokenType.PLUS, "+", null, 1),
          Literal(2)
        )
      ),
      Token(TokenType.STAR, "*", null, 1),
      Grouping(
        Binary(
          Literal(4),
          Token(TokenType.MINUS, "-", null, 1),
          Literal(3)
        )
      )
    )
    println("(1 + 2) * (4 - 3) => " + printer.print(nested))
