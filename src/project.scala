//> using scala "3.2.1"

package boilerplate

import scala.io.Source

trait Solve[T](val day: String):

  private def lines(fileName: String): List[String] = Source
    .fromFile(fileName)
    .mkString
    .linesIterator
    .toList

  def input: List[String] = lines(s"input/$day")
  def testInput: List[String] = lines(s"input/test/$day")

  def p1(input: List[String]): T
  def p2(input: List[String]): T
