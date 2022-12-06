package day03

import boilerplate.Solve

object Solution extends Solve[Int]("day03"):

  type Item = Char
  extension (i: Item)
    // 'a'.toInt = 97, 'A'.toInt = 65
    def priority: Int = if i.isLower then i.toInt - 96 else i.toInt - 64 + 26

  type Rucksacks = Seq[Seq[Item]]
  extension (r: Rucksacks)
    // safe-ish as we assume AoC's organizers are perfect
    def findFirstCommonItem: Item =
      r.map(_.toSet).reduceLeft(_ intersect _).head

  object Rucksacks:
    def splitInTwain(items: String): Rucksacks =
      val (left, right) = items.splitAt(items.length / 2)
      Seq(left, right)
    def apply(rs: IterableOnce[String]): Rucksacks = Seq.from(rs.map(_.toSeq))

  override def p1(input: List[String]): Int =
    input.map(Rucksacks.splitInTwain).map(_.findFirstCommonItem.priority).sum
  override def p2(input: List[String]): Int = input
    .grouped(3)
    .map(Rucksacks.apply)
    .map(_.findFirstCommonItem.priority)
    .sum

package test:
  import Solution.*
  @main def part1(): Unit =
    val result = p1(testInput)
    val expected = 157
    assert(result == expected, s"Expected $expected, got $result")

  @main def part2(): Unit =
    val result = p2(testInput)
    val expected = 70
    assert(result == expected, s"Expected $expected, got $result")

package main:
  import Solution.*
  @main def part1(): Unit =
    val result = p1(input)
    println(s"Total priority of common items: $result.")

  @main def part2(): Unit =
    val result = p2(input)
    println(s"Total priority of common items: $result.")
