package day04

import scala.io.Source
import java.security.spec.EncodedKeySpec

lazy val testInput = Source.fromFile("input/test/day04").mkString
lazy val input = Source.fromFile("input/day04").mkString

object Solution:

  def areOverlapping(r1: Range, r2: Range): Boolean = r1.intersect(r2).nonEmpty

  def oneContainsTheOther(r1: Range, r2: Range): Boolean =
    r1.containsSlice(r2) || r2.containsSlice(r1)

  def pair[T](ts: List[T]): (T, T) = ts match {
    case a :: b :: _ => (a, b)
    case _ => throw new IllegalArgumentException("pairs: not enough elements")
  }

  def ranges(ranges: String): List[Range] = ranges
    .split(",")
    .map(_.split("-"))
    .map(x => x(0).toInt to x(1).toInt)
    .toList

  def lines(input: String): List[String] =
    input.linesIterator.toList.filterNot(_.isBlank).map(_.trim)

  def part1(input: String): Int =
    lines(input).map(ranges).map(pair).filter(oneContainsTheOther).size
  def part2(input: String): Int = lines(input).map(ranges).map(pair).filter(areOverlapping).size

package test:
  @main def part1(): Unit =
    val result = Solution.part1(testInput)
    val expected = 2
    assert(result == expected, s"Expected $expected, got $result")

  @main def part2(): Unit =
    val result = Solution.part2(testInput)
    val expected = 4
    assert(result == expected, s"Expected $expected, got $result")

package main:
  @main def part1(): Unit =
    val result = Solution.part1(input)
    println(s"Assignments: $result.")

  @main def part2(): Unit =
    val result = Solution.part2(input)
    println(s"Overlapping assignments: $result.")
