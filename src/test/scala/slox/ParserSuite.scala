//> using dep org.scalameta::munit::1.3.0
package slox

import slox.Expr.Literal

// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class ParserSuite extends munit.FunSuite:
  val eof = new Token(TokenType.EOF, null, null, 1)
  
  test("parses a single digit") {
    val digitLiteral = Literal(42)
    val digitToken = new Token(TokenType.NUMBER, "42", 42, 1)

    val underTest = new Parser(List(digitToken, eof))
    val actual = underTest.parse()

    assertEquals(actual, digitLiteral)
  }

  test("parses a comma expresion") {
    val falseLiteral = Literal("false")
    val trueLiteral = Literal("true")
    val left = new Token(
      TokenType.FALSE,
      lexeme = "false",
      literal = falseLiteral,
      line = 1
    )
    val comma: Token = new Token(
      `type` = TokenType.COMMA,
      lexeme = ",",
      literal = ",",
      line = 1
    )
    val right = new Token(
      TokenType.TRUE,
      lexeme = "true",
      literal = trueLiteral,
      line = 1
    )

    val underTest = new Parser(List(left, comma, right, eof))
    val expectedExpression: Expr = Expr.Literal(true)
    val actualExpression = underTest.parse()
    print(actualExpression)

    assertEquals(actualExpression, expectedExpression)
  }
