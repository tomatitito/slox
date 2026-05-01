package slox

import java.nio.charset.StandardCharsets
import java.nio.file.Files

class E2ESuite extends munit.FunSuite:
  private var testPath: List[String] = Nil

  private def describe(name: String)(body: => Unit): Unit =
    val previousPath = testPath
    testPath = testPath :+ name
    try body
    finally testPath = previousPath

  private def it(name: String)(body: => Any)(using loc: munit.Location): Unit =
    test((testPath :+ name).mkString(" / "))(body)

  private def runSlox(source: String): (Int, String, String) =
    val script = Files.createTempFile("slox-e2e-", ".lox")
    Files.writeString(script, source, StandardCharsets.UTF_8)

    val projectRoot = java.io.File(".").getCanonicalFile
    val process = new ProcessBuilder(
      "scala-cli",
      "run",
      ".",
      "--main-class",
      "slox.Lox",
      "--",
      script.toString
    )
      .directory(projectRoot)
      .start()

    val stdout = scala.io.Source.fromInputStream(process.getInputStream).mkString
    val stderr = scala.io.Source.fromInputStream(process.getErrorStream).mkString
    val exitCode = process.waitFor()

    Files.deleteIfExists(script)
    (exitCode, stdout, stderr)

  describe("ternary expression parse errors"):
    it("reports an error for '?a:'"):
      val (exitCode, _, stderr) = runSlox("?a:")

      assert(clue(stderr).contains("Error"))
      assertNotEquals(exitCode, 0)

    it("reports an error for 'true ? = : 42'"):
      val (exitCode, _, stderr) = runSlox("true ? = : 42")

      assert(clue(stderr).contains("Error"))
      assertNotEquals(exitCode, 0)
