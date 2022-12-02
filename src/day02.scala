package day02

import scala.io.Source

lazy val testInput = Source.fromFile("input/test/day02").mkString
lazy val input = Source.fromFile("input/day02").mkString

object Solution:

  type MyPlays = 'X' | 'Y' | 'Z'
  type OthersPlays = 'A' | 'B' | 'C'

  case class Round(myPlay: MyPlays, othersPlay: OthersPlays)

  enum Play(val value: Int): 
    case Rock extends Play(1)
    case Paper extends Play(2)
    case Scissors extends Play(3)

    def outcomeAgainst(that: Play): Int = (this, that) match 
      case (Rock, Paper) | (Scissors, Rock) | (Paper, Scissors) => 0
      case (Rock, Scissors) | (Scissors, Paper) | (Paper, Rock) => 6
      case _ => 3

    def score(that: Play): Int = 
      outcomeAgainst(that) + this.value

  object Play:
    def parseMine(myPlay: MyPlays) = myPlay match 
      case 'X' => Rock
      case 'Y' => Paper
      case 'Z' => Scissors
    
    def parseOthers(othersPlay: OthersPlays) = othersPlay match
      case 'A' => Rock
      case 'B' => Paper
      case 'C' => Scissors

  def parseRound(line: String): Round = line.toCharArray.toList match
    case (othersPlay: OthersPlays) :: _ :: (myPlay: MyPlays) :: Nil => Round(myPlay, othersPlay)
    case e => throw new Exception(s"Invalid input: $e")

  def parseInput(input: String): List[Round] = input.linesIterator.map(parseRound).toList

  def part1(input: String): Int = 
    parseInput(input).map { case Round(myPlay, othersPlay) => Play.parseMine(myPlay).score(Play.parseOthers(othersPlay)) }.sum

  def part2(input: String): Int = ???

package test:
  @main def part1(): Unit = 
    val result = Solution.part1(testInput)
    val expected = 15
    assert(result == expected, s"Expected $expected, got $result")

  // @main def part2(): Unit =
  //   val result = Solutions.part2(testInput)
  //   assert(result == 45000, s"Expected 45000, got $result")


package main: 
  @main def part1(): Unit = 
    val result = Solution.part1(input)
    println(s"Your total score is $result.")

  // @main def part2(): Unit =
  //   val result = Solution.part2(input)
  //   println(s"The 3 elves carrying the most calories carry $result cal.")