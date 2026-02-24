//> using scala 3.7.3
package slox

import java.io.IOException
import java.nio.file.Files
import java.nio.file.Paths
import java.nio.charset.StandardCharsets
import java.nio.charset.Charset
import java.io.InputStreamReader
import java.io.BufferedReader

@main def Lox(args: String*): Unit =
  println("Welcome to Slox!")
  if (args.length > 1) {
    println("Usage: slox [script]")
    System.exit(64)
  } else if (args.length == 1) {
    runFile(args.head)
  } else
    runPrompt()

  println(msg)
end Lox

def msg = "I was compiled by Scala 3. :)"

var hadError = false

@throws[IOException]
private def runFile(path: String): Unit =
  if (hadError) then System.exit(65)
  val bytes = Files.readAllBytes(Paths.get(path))
  run(new String(bytes, Charset.defaultCharset()))
end runFile

@throws[IOException]
private def runPrompt(args: String*): Unit =
  val input = new InputStreamReader(System.in)
  val reader = new BufferedReader(input)

  while (true) {
    print("> ")
    val line = reader.readLine()
    if (line == null) {
      return
    }
    run(line)
    hadError = false
  }
end runPrompt

private def run(source: String): Unit =
  val scanner = Scanner(source) // to be implemented
  val tokens = scanner.scanTokens()
  tokens.foreach(t => print(s"$t "))
end run

private def error(line: Int, message: String): Unit =
  report(line, "", message)

private def report(line: Int, where: String, message: String): Unit =
  System.err.println(s"[line $line] Error$where: $message")
  hadError = true
