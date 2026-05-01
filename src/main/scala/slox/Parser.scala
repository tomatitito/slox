package slox

import slox.Expr.Unary
import scala.caps.consume
import slox.{error as LoxError}
import scala.compiletime.ops.double

class Parser(tokens: List[Token]):
  private var current = 0
  private var ternaryCondition: Expr | Null = null
  
  def parse() = 
    try
      expression()
    catch
      case _: ParseError => null

  private def expression(): Expr =
    val expr = comma()
    if !isAtEnd() & matchToken(TokenType.TERNARY_IF) then
      val condition = expr
      val ifOperator = previous()
      val thenExpr = expression()
      val elseOperator = consume(TokenType.TERNARY_ELSE, "Expect ':' after then branch of ternary expression.")
      val elseExpr = expression()
      Expr.Ternary(condition, ifOperator, thenExpr, elseOperator, elseExpr)
    else expr
    
  private def comma(): Expr = 
    var expr = equality()
    
    while matchToken(TokenType.COMMA) do 
      val op = previous()
      val right = equality()
      expr = right 
    expr

  private def equality(): Expr =
    var expr = comparison()

    while matchToken(TokenType.BANG_EQUAL, TokenType.EQUAL_EQUAL) do
      val op = previous()
      val right = comparison()
      expr = Expr.Binary(expr, op, right)
    expr

  private def comparison(): Expr =
    var expr = term()

    while matchToken(
        TokenType.GREATER,
        TokenType.GREATER_EQUAL,
        TokenType.LESS,
        TokenType.LESS_EQUAL
      )
    do
      val op = previous()
      val right = term()
      expr = Expr.Binary(expr, op, right)
    expr

  private def term(): Expr =
    var expr = factor()

    while matchToken(TokenType.MINUS, TokenType.PLUS) do
      val op = previous()
      val right = factor()
      expr = Expr.Binary(expr, op, right)
    expr

  private def factor(): Expr =
    var expr = unary()

    while matchToken(TokenType.SLASH, TokenType.STAR) do
      val op = previous()
      val right = unary()
      expr = Expr.Binary(expr, op, right)
    expr

  private def unary(): Expr =
    if matchToken(TokenType.BANG, TokenType.MINUS) then
      val op = previous()
      Unary(op, unary())
    else primary()

  private def primary(): Expr =
    val currentToken = advance()
    currentToken.`type` match
      case TokenType.IDENTIFIER => Expr.Literal(currentToken.literal)
      case TokenType.STRING     => Expr.Literal(currentToken.literal)
      case TokenType.NUMBER     => Expr.Literal(currentToken.literal)
      case TokenType.TRUE       => Expr.Literal(true)
      case TokenType.FALSE      => Expr.Literal(false)
      case TokenType.NIL        => Expr.Literal(null)
      case TokenType.LEFT_PAREN =>
        val expr = expression()
        consume(TokenType.RIGHT_PAREN, "Expect ')' after expression.")
        Expr.Grouping(expr)
      case _ => throw error(currentToken, "Expect expression.")
      
  private def matchToken(ts: TokenType*): Boolean =
    val currentToken = peek()
    if ts.contains(currentToken.`type`) then
      val _ = advance()
      true
    else false

  private def consume(token: TokenType, message: String): Token =
    if check(token) then advance() else throw error(peek(), message)
    
  private def check(t: TokenType): Boolean = tokens(current).`type` == t

  private def advance(): Token =
    val token = tokens(current)
    current += 1
    token

  private def peek(): Token = tokens(current)

  private def previous(): Token = tokens(current - 1)

  private def isAtEnd(): Boolean = tokens(current).`type` == TokenType.EOF
  
  private def error(token: Token, message: String): ParseError =
    LoxError(token, message)
    return new ParseError()
  
  private class ParseError() extends RuntimeException
  
end Parser
