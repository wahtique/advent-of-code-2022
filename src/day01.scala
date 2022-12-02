package day01

import scala.io.Source

lazy val testInput = Source.fromFile("input/test/day01").mkString
lazy val input = Source.fromFile("input/day01").mkString

object Solutions:

  private def totalCaloriesByElf(input: String): Array[Int] =
    val loadsByElf = input.split("(?:\\h*\\n){2,}")
    val sumsByElf = loadsByElf.map(loads => loads.split("\\h*\\n").map(_.toInt).sum) 
    sumsByElf

  def part1(input: String): Int = 
    totalCaloriesByElf(input).max

  def part2(input: String): Int = 
    totalCaloriesByElf(input).sorted.reverse.take(3).sum

package test:
  @main def part1(): Unit = 
    val result = Solutions.part1(testInput)
    assert(result == 24000, s"Expected 24000, got $result")

  @main def part2(): Unit =
    val result = Solutions.part2(testInput)
    assert(result == 45000, s"Expected 45000, got $result")


package main: 
  @main def part1(): Unit = 
    val result = Solutions.part1(input)
    println(s"The elf carrying the most calories carries $result cal.")

  @main def part2(): Unit =
    val result = Solutions.part2(input)
    println(s"The 3 elves carrying the most calories carry $result cal.")