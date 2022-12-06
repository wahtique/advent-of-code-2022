package day04

import boilerplate.Solve

object Solution extends Solve[Int]("day04"):

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

  override def p1(input: List[String]): Int =
    input.map(ranges).map(pair).filter(oneContainsTheOther).size
  override def p2(input: List[String]): Int =
    input.map(ranges).map(pair).filter(areOverlapping).size

package test:
  import Solution.*
  @main def part1(): Unit =
    val result = p1(testInput)
    val expected = 2
    assert(result == expected, s"Expected $expected, got $result")

  @main def part2(): Unit =
    val result = p2(testInput)
    val expected = 4
    assert(result == expected, s"Expected $expected, got $result")

package main:
  import Solution.*
  @main def part1(): Unit =
    val result = p1(input)
    println(s"Assignments: $result.")

  @main def part2(): Unit =
    val result = p2(input)
    println(s"Overlapping assignments: $result.")
