//> using scala "3.2.1"

package boilerplate

import scala.io.Source

trait Solve(val day: String):

  private def lines(fileName: String): List[String] = Source
    .fromFile(fileName)
    .mkString
    .linesIterator
    .toList
    .filterNot(_.isBlank)
    .map(_.trim)

  def input: List[String] = lines(s"input/$day")
  def testInput: List[String] = lines(s"input/test/$day")

  def p1(input: List[String]): Int
  def p2(input: List[String]): Int
