package slox

import TokenType.*
import java.io.StreamTokenizer

class Scanner(private val source: String):
  private var tokens: List[Token] = List.empty
  private var start: Int = 0
  private var current: Int = 0
  private var line: Int = 1

  def scanTokens(): List[Token] =
    while (!isAtEnd()) {
      start = current
      scanToken()
    }
    tokens ::: List(new Token(EOF, "", null, line))

  private def scanToken() =
    val c = advance()
    c match
      case '(' => addToken(LEFT_PAREN)
      case ')' => addToken(RIGHT_PAREN)
      case '{' => addToken(LEFT_BRACE)
      case '}' => addToken(RIGHT_BRACE)
      case ',' => addToken(COMMA)
      case '.' => addToken(DOT)
      case '-' => addToken(MINUS)
      case '+' => addToken(PLUS)
      case ';' => addToken(SEMICOLON)
      case '*' => addToken(STAR)
      case '!' =>
        val newToken = if matchChar('=') then BANG_EQUAL else BANG
        addToken(newToken)
      case '=' =>
        val newToken = if matchChar('=') then EQUAL_EQUAL else EQUAL
        addToken(newToken)
      case '<' =>
        val newToken = if matchChar('=') then LESS_EQUAL else LESS
        addToken(newToken)
      case '>' =>
        val newToken = if matchChar('=') then GREATER_EQUAL else GREATER
        addToken(newToken)
      case '/' =>
        if matchChar('/') then
          while peek() != '\n' && !isAtEnd() do advance()
          ()
        else addToken(SLASH)
      case ' '  => ()
      case '\t' => ()
      case '\r' => ()
      case '\n' => line += 1
      case '"'  => string()
      case _ =>
        if (isDigit(peek())) then number()
        else if (isAlpha(c)) then identifier()
        else error(line, "unexpected character.")

  private def string(): Unit =
    while peek() != '"' && !isAtEnd()
    do
      if peek() == '\n' then line += 1 else ()
      advance()
    if (isAtEnd())
      error(line, "Unterminated string.")
      return
    advance()
    val value = source.substring(start + 1, current - 1)
    addToken(STRING, value)

  private def number(): Unit =
    while isDigit(peek()) do advance()

    if peek() == '.' && isDigit(peekNext()) then
      advance()
      while isDigit(peek()) do advance()

    val value = source.substring(start, current)
    addToken(NUMBER, value.toDouble)
    
  private def identifier(): Unit =
    while isAlphaNumeric(peek()) do advance()
    val text = source.substring(start, current)
    val tokenType = Scanner.keywords.get(text) match {
      case Some(t) => t
      case None => IDENTIFIER
    }
    addToken(tokenType)

  private def isDigit(c: Char): Boolean = c >= '0' && c <= '9'
  
  private def isAlpha(c: Char) = c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c == '_'
  
  private def isAlphaNumeric(c: Char) = isAlpha(c) || isDigit(c)

  private def advance(): Char =
    current += 1 // next time arouund current will point to the next character
    source.charAt(current - 1)

  private def addToken(`type`: TokenType): Unit = addToken(`type`, null)

  private def addToken(`type`: TokenType, literal: Any): Unit =
    val text = source.substring(start, current)
    tokens = new Token(`type`, text, literal, line) :: tokens

  private def matchChar(expected: Char): Boolean =
    if isAtEnd()
    then return false

    if source.charAt(current) != expected
    then return false

    current += 1
    return true

  private def peek(): Char =
    if isAtEnd()
    then '\u0000'
    else source.charAt(current)

  private def peekNext(): Char =
    if current + 1 >= source.length
    then '\u0000'
    else source.charAt(current + 1)

  private def isAtEnd(): Boolean =
    current >= source.length

end Scanner

object Scanner:
  private val keywords = Map(
    "and" -> AND,
    "class" -> CLASS,
    "else" -> ELSE,
    "false" -> FALSE,
    "for" -> FOR,
    "fun" -> FUN,
    "if" -> IF,
    "nil" -> NIL,
    "or" -> OR,
    "print" -> PRINT,
    "return" -> RETURN,
    "super" -> SUPER,
    "this" -> THIS,
    "true" -> TRUE,
    "var" -> VAR,
    "while" -> WHILE
  )

  def apply(source: String): Scanner =
    new Scanner(source)
