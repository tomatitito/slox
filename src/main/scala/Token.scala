class Token(
    val `type`: TokenType,
    val lexeme: String,
    val literal: Any,
    val line: Int
):
  override def toString(): String = s"${`type`} $lexeme $literal"
end Token
