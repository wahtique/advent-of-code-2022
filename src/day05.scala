package day05

import boilerplate.Solve
import scala.collection.mutable.Stack
import scala.annotation.tailrec

object Solution extends Solve[String]("day05"):

  // pure stuff

  type Stacks = Seq[Stack[Char]]

  sealed trait Move
  case class AtomicMove(from: Int, to: Int) extends Move
  case class SeqMove(n: Int, atomicMove: AtomicMove) extends Move
  case class ParMove(n: Int, from: Int, to: Int) extends Move

  trait Transition[T]: 
    def apply(transition: T)(stacks: Stacks): Stacks

  given Transition[AtomicMove] with
    def apply(atomicMove: AtomicMove)(stacks: Stacks): Stacks =
      // use clone avoid mutating the original stacks
      val source = stacks(atomicMove.from).clone()
      val poped = source.pop
      val target = stacks(atomicMove.to)
      stacks.updated(atomicMove.from, source).updated(atomicMove.to, target.push(poped))

  given Transition[SeqMove] with
    def apply(seqMove: SeqMove)(stacks: Stacks): Stacks =
      @tailrec
      def loop(n: Int, stacks: Stacks): Stacks =
        if n <= 0 then stacks
        else loop(n - 1, summon[Transition[AtomicMove]].apply(seqMove.atomicMove)(stacks))
      loop(seqMove.n, stacks)

  given Transition[ParMove] with
    def apply(parMove: ParMove)(stacks: Stacks): Stacks =
      val source = stacks(parMove.from).clone()
      val crates = (1 to parMove.n).foldLeft(Seq.empty[Char])((acc, _) => acc :+ source.pop)
      val target = stacks(parMove.to)
      val newTarget = crates.foldRight(target)((c, acc) => acc.push(c))
      stacks.updated(parMove.from, source).updated(parMove.to, newTarget)

  case class State[M <: Move: Transition](stacks: Stacks, moves: Seq[M]):
    def next(using t: Transition[M]): State[M] = State(t.apply(moves.head)(stacks), moves.tail)
    def endState: State[M] = 
      @tailrec
      def loop(state: State[M]): State[M] =
        if state.moves.isEmpty then state
        else loop(state.next)
      loop(this)
    def topMessage: String = stacks.map(_.head).mkString

  // impure stuff

  // parsing crates never changes
  def parseCrate(s: String): Option[Char] =
    Option.when(!s.isBlank)(s.trim.filterNot(c => c == '[' || c == ']').trim.head)

  def parseStacks(lines: Seq[String]): Stacks =
    val cratesByLine =
      lines.reverse // easier to build stacks from bottom to top
        .drop(1) // we don't really care about the first line of number
        .map(l => l.grouped(4).map(parseCrate).toSeq)
    val nCrates = cratesByLine.head.size
    val empty = Seq.fill(nCrates)(Stack[Char]())
    cratesByLine.foldLeft(empty)((stacks, crates) =>
      stacks.zip(crates).map { case (stack, crate) =>
        crate match
          case None        => stack
          case Some(value) => stack.push(value)
      }
    )

  private val moveRegex = """move (\d+) from (\d+) to (\d+)""".r

  // parsing moves actually change ! kinda
  trait Parse[M <: Move]:
    def apply(n: Int, from: Int, to: Int): M

  given Parse[AtomicMove] with
    def apply(n: Int, from: Int, to: Int): AtomicMove = AtomicMove(from - 1, to - 1 )

  given Parse[SeqMove] with
    def apply(n: Int, from: Int, to: Int): SeqMove = SeqMove(n, summon[Parse[AtomicMove]].apply(n, from, to))

  given Parse[ParMove] with
    def apply(n: Int, from: Int, to: Int): ParMove = ParMove(n, from - 1, to - 1)

  def parseMove[M <: Move : Parse : Transition](line: String): M = line match
    case moveRegex(n, from, to) => summon[Parse[M]].apply(n.toInt, from.toInt, to.toInt)
    // AoC organizers are perfect but meh I would feel bad not having an exhaustive match
    case _ => throw new IllegalArgumentException(s"Invalid move: $line")

  def parseMoves[M <: Move : Parse : Transition](lines: Seq[String]): Seq[M] = lines.map(parseMove[M])

  def parse[M <: Move : Parse : Transition](input: Seq[String]): State[M] =
    val sepLineIndex = input.indexWhere(_.startsWith("move")) - 1
    val (stacks, moves) = input.splitAt(sepLineIndex)
    val stacksParsed = parseStacks(stacks)
    val movesParsed = parseMoves[M](moves.drop(1)) // drop the separator line
    State[M](stacksParsed, movesParsed)

  override def p1(input: List[String]): String = parse[SeqMove](input.toSeq).endState.topMessage
  override def p2(input: List[String]): String = parse[ParMove](input.toSeq).endState.topMessage

package test:
  import Solution.*
  @main def part1(): Unit =
    val result = p1(testInput)
    val expected = "CMZ"
    assert(result == expected, s"Expected $expected, got $result")

  @main def part2(): Unit =
    val result = p2(testInput)
    val expected = "MCD"
    assert(result == expected, s"Expected $expected, got $result")

package main:
  import Solution.*
  @main def part1(): Unit =
    val result = p1(input)
    println(s"Result: $result.")

  @main def part2(): Unit =
    val result = p2(input)
    println(s"Result: $result.")
