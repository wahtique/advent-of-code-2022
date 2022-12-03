package day03

import scala.io.Source
import java.security.spec.EncodedKeySpec

lazy val testInput = Source.fromFile("input/test/day03").mkString
lazy val input = Source.fromFile("input/day03").mkString

object Solution:

  type Item = Char
  extension (i: Item) 
    // 'a'.toInt = 97, 'A'.toInt = 65
    def priority: Int = if i.isLower then i.toInt - 96 else i.toInt - 64 + 26

  type Rucksacks = Seq[Seq[Item]]
  extension (r: Rucksacks)
    // safe-ish as we assume AoC's organizers are perfect
    def findFirstCommonItem: Item = r.map(_.toSet).reduceLeft(_ intersect _).head

  object Rucksacks: 
    def splitInTwain(items: String): Rucksacks = 
      val (left, right) = items.splitAt(items.length / 2)
      Seq(left, right)
    def apply(rs: IterableOnce[String]): Rucksacks = Seq.from(rs.map(_.toSeq))
  
  def lines(input: String): List[String] = input.linesIterator.toList
  
  def part1(input: String): Int = lines(input).map(Rucksacks.splitInTwain).map(_.findFirstCommonItem.priority).sum
  def part2(input: String): Int = lines(input).grouped(3).map(Rucksacks.apply).map(_.findFirstCommonItem.priority).sum


package test:
  @main def part1(): Unit = 
    val result = Solution.part1(testInput)
    val expected = 157
    assert(result == expected, s"Expected $expected, got $result")

  @main def part2(): Unit =
    val result = Solution.part2(testInput)
    val expected = 70
    assert(result == expected, s"Expected $expected, got $result")


package main: 
  @main def part1(): Unit = 
    val result = Solution.part1(input)
    println(s"Total priority of common items: $result.")

  @main def part2(): Unit = 
    val result = Solution.part2(input)
    println(s"Total priority of common items: $result.")