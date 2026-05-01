//> using dep org.scalameta::munit::1.3.0
package slox

import slox.Expr.{Binary, Literal, Ternary}

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

  test("parses a ternary expression when the condition is true") {
    val two = new Token(TokenType.NUMBER, "2", 2, 1)
    val less = new Token(TokenType.LESS, "<", null, 1)
    val three = new Token(TokenType.NUMBER, "3", 3, 1)
    val ternaryIf = new Token(TokenType.TERNARY_IF, "?", null, 1)
    val ternaryThen = new Token(TokenType.NUMBER, "2", 2, 1)
    val ternaryElse = new Token(TokenType.TERNARY_ELSE, ":", null, 1)
    val ternaryOtherwise = new Token(TokenType.NUMBER, "3", 3, 1)

    val parserInput: List[Token] = List(
      two,
      less,
      three,
      ternaryIf,
      ternaryThen,
      ternaryElse,
      ternaryOtherwise,
      eof
    )
    val underTest = new Parser(parserInput)

    val expected: Expr = Ternary(
      Binary(Literal(2), less, Literal(3)),
      ternaryIf,
      Literal(2),
      ternaryElse,
      Literal(3)
    )
    val actual = underTest.parse()

    assertEquals(actual, expected)
  }

  test("parses a ternary expression when the condition is false") {
    val three = new Token(TokenType.NUMBER, "3", 3, 1)
    val less = new Token(TokenType.LESS, "<", null, 1)
    val two = new Token(TokenType.NUMBER, "2", 2, 1)
    val ternaryIf = new Token(TokenType.TERNARY_IF, "?", null, 1)
    val ternaryThen = new Token(TokenType.NUMBER, "2", 2, 1)
    val ternaryElse = new Token(TokenType.TERNARY_ELSE, ":", null, 1)
    val ternaryOtherwise = new Token(TokenType.NUMBER, "3", 3, 1)

    val parserInput: List[Token] = List(
      three,
      less,
      two,
      ternaryIf,
      ternaryThen,
      ternaryElse,
      ternaryOtherwise,
      eof
    )
    val underTest = new Parser(parserInput)

    val expected: Expr = Ternary(
      Binary(Literal(3), less, Literal(2)),
      ternaryIf,
      Literal(2),
      ternaryElse,
      Literal(3)
    )
    val actual = underTest.parse()

    assertEquals(actual, expected)
  }

  test("parses a ternary expression with FALSE as condition") {
    val condition = new Token(TokenType.FALSE, "false", null, 1)
    val ternaryIf = new Token(TokenType.TERNARY_IF, "?", null, 1)
    val ternaryThen = new Token(TokenType.NUMBER, "2", 2, 1)
    val ternaryElse = new Token(TokenType.TERNARY_ELSE, ":", null, 1)
    val ternaryOtherwise = new Token(TokenType.NUMBER, "3", 3, 1)

    val parserInput: List[Token] = List(
      condition,
      ternaryIf,
      ternaryThen,
      ternaryElse,
      ternaryOtherwise,
      eof
    )
    val underTest = new Parser(parserInput)

    val expected: Expr = Ternary(
      Literal(false),
      ternaryIf,
      Literal(2),
      ternaryElse,
      Literal(3)
    )
    val actual = underTest.parse()

    assertEquals(actual, expected)
  }

  test("parses a ternary expression with TRUE as condition") {
    val condition = new Token(TokenType.TRUE, "true", null, 1)
    val ternaryIf = new Token(TokenType.TERNARY_IF, "?", null, 1)
    val ternaryThen = new Token(TokenType.NUMBER, "2", 2, 1)
    val ternaryElse = new Token(TokenType.TERNARY_ELSE, ":", null, 1)
    val ternaryOtherwise = new Token(TokenType.NUMBER, "3", 3, 1)

    val parserInput: List[Token] = List(
      condition,
      ternaryIf,
      ternaryThen,
      ternaryElse,
      ternaryOtherwise,
      eof
    )
    val underTest = new Parser(parserInput)

    val expected: Expr = Ternary(
      Literal(true),
      ternaryIf,
      Literal(2),
      ternaryElse,
      Literal(3)
    )
    val actual = underTest.parse()

    assertEquals(actual, expected)
  }
end ParserSuite
