package day02

import boilerplate.Solve

object Solution extends Solve[Int]("day02"):

  // the game : very classic RockPaperScissors
  // with values for plays and outcomes to spice things up

  // something a player can do
  enum Play(val value: Int):
    case Rock extends Play(1)
    case Paper extends Play(2)
    case Scissors extends Play(3)

  // a round has only 3 outcomes
  enum Outcome(val value: Int):
    case Victory extends Outcome(6)
    case Defeat extends Outcome(0)
    case Draw extends Outcome(3)

  case class Round(myPlay: Play, theOthersPlay: Play):
    import Outcome.*
    import Play.*
    def outcome: Outcome = (myPlay, theOthersPlay) match
      case (Rock, Scissors) | (Paper, Rock) | (Scissors, Paper) => Victory
      case (Rock, Paper) | (Paper, Scissors) | (Scissors, Rock) => Defeat
      case _                                                    => Draw
    def score: Int = myPlay.value + outcome.value

  // now for the "impure" part !
  // ie. get the cheat sheet as input and decide how to play

  // whatever is in the cheat sheet
  type EncodedPlay = ABC | XYZ
  type ABC = 'A' | 'B' | 'C'
  type XYZ = 'X' | 'Y' | 'Z'

  // deducing the round is done by decoding the encoded plays
  // with typeclasses for fun and giggles
  // ... and also beacause this will let us do part 2 without changing anything :)
  trait Decode[EP <: EncodedPlay, T]:
    def decode(ep: EP): T

  // this does not change
  given Decode[ABC, Play] = ep =>
    ep match
      case 'A' => Play.Rock
      case 'B' => Play.Paper
      case 'C' => Play.Scissors

  object Decode:
    def apply[EP <: EncodedPlay, T](using d: Decode[EP, T]): Decode[EP, T] = d

  type RoundReader = (ABC, XYZ) => Round

  // read a line of the cheatsheet into a nice scala object
  def parseRound(read: RoundReader)(line: String): Round =
    line.toCharArray.toList match
      case (abc: ABC) :: _ :: (xyz: XYZ) :: Nil => read(abc, xyz)
      case e => throw new Exception(s"Invalid input: $e")

  // parse the cheatsheet
  def rounds(readRound: RoundReader)(input: List[String]): List[Round] =
    input.map(parseRound(readRound))

  override def p1(input: List[String]): Int =
    // this is true only for part 1
    given Decode[XYZ, Play] = ep =>
      ep match
        case 'X' => Play.Rock
        case 'Y' => Play.Paper
        case 'Z' => Play.Scissors

    val readRound: RoundReader = (abc, xyz) =>
      Round(
        myPlay = Decode[XYZ, Play].decode(xyz),
        theOthersPlay = Decode[ABC, Play].decode(abc)
      )
    rounds(readRound)(input).map(_.score).sum

  override def p2(input: List[String]): Int =
    // this is for part 2
    given Decode[XYZ, Outcome] = ep =>
      ep match
        case 'X' => Outcome.Defeat
        case 'Y' => Outcome.Draw
        case 'Z' => Outcome.Victory

    val readRound: RoundReader = (abc, xyz) => {
      val theirs = Decode[ABC, Play].decode(abc)
      val expectedOutcome = Decode[XYZ, Outcome].decode(xyz)
      val myPlay = (expectedOutcome, theirs) match
        case (Outcome.Victory, Play.Rock)     => Play.Paper
        case (Outcome.Victory, Play.Paper)    => Play.Scissors
        case (Outcome.Victory, Play.Scissors) => Play.Rock
        case (Outcome.Defeat, Play.Rock)      => Play.Scissors
        case (Outcome.Defeat, Play.Paper)     => Play.Rock
        case (Outcome.Defeat, Play.Scissors)  => Play.Paper
        case (Outcome.Draw, _)                => theirs
      Round(myPlay, theirs)
    }
    rounds(readRound)(input).map(_.score).sum

package test:
  import Solution.*
  @main def part1(): Unit =
    val result = p1(testInput)
    val expected = 15
    assert(result == expected, s"Expected $expected, got $result")

  @main def part2(): Unit =
    val result = p2(testInput)
    val expected = 12
    assert(result == expected, s"Expected $expected, got $result")

package main:
  import Solution.*
  @main def part1(): Unit =
    val result = p1(input)
    println(s"Your total score is $result.")

  @main def part2(): Unit =
    val result = p2(input)
    println(s"Your total score is $result.")
