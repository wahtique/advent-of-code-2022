
package day06

import boilerplate.Solve
import scala.collection.mutable.Stack
import scala.annotation.tailrec

object Solution extends Solve[Int]("day06"):

  def solve(windowSize: Int)(code: String): Int = code.sliding(windowSize).indexWhere(_.toSet.size == windowSize) + windowSize

  override def p1(input: List[String]): Int = solve(4)(input.head)
  override def p2(input: List[String]): Int = solve(14)(input.head)

package test:
  import Solution.*
  @main def part1(): Unit =
    val result = p1(testInput)
    val expected = 11
    assert(result == expected, s"Expected $expected, got $result")

  @main def part2(): Unit =
    val result = p2(testInput)
    val expected = 26
    assert(result == expected, s"Expected $expected, got $result")

package main:
  import Solution.*
  @main def part1(): Unit =
    val result = p1(input)
    println(s"Result: $result.")

  @main def part2(): Unit =
    val result = p2(input)
    println(s"Result: $result.")