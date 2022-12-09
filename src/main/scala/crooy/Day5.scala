package crooy

import zio.*
import zio.ZIO.*

object Day5 extends ZIOAppDefault {
  def run = (for {
    _       <- logInfo("day 5")
    answer1 <- part1
    _       <- logInfo(s"answer part 1 is $answer1")
    answer2 <- part2
    _       <- logInfo(s"answer part 2 is $answer2")

  } yield ()).provide(live)

  def part2: ZIO[FileListInput, Exception, String] = for {
    input  <- parseInput
    _      <- logInfo(s"input state is ${input._1}")
    output <- ZIO.succeed(moveCrane(input, false))
    _      <- logInfo(s"output state is ${output._1}")
    result = output._1.toList.sortBy(_._1).map(_._2.last).mkString("")
  } yield result

  def part1: ZIO[FileListInput, Exception, String] = for {
    input  <- parseInput
    _      <- logInfo(s"input state is ${input._1}")
    output <- ZIO.succeed(moveCrane(input, true))
    _      <- logInfo(s"output state is ${output._1}")
    result = output._1.toList.sortBy(_._1).map(_._2.last).mkString("")
  } yield result

  private def moveCrane(input: (CrateState, List[Move]), v1: Boolean): (CrateState, List[Move]) =
    input match {
      case (state, move :: rest) =>
        moveCrane((Move.applyTo(move, state, v1), rest), v1)

      case (state, Nil) => (state, Nil)
    }

  private val crate = "\\s?\\[([A-Z]+)\\]\\s?".r
  private def parseColumn(column: List[String]): CrateColumn = column match {
    case Nil                        => Nil
    case crate(letter) :: remainder => letter :: parseColumn(remainder)
    case _ :: remainder             => parseColumn(remainder)
  }
  private def parseHeader(input: List[String]) = ZIO.succeed {
    val listed = input
      .map(_.grouped(4).toList)
      .dropRight(1)
    val size = listed.last.length
    val rows = listed.map { row =>
      row ++ Range(0, size - row.length).toList.map(_ => "")
    }.reverse

    val columns = rows.transpose

    columns
      .map(parseColumn)
      .zipWithIndex
      .map { case (column, i) =>
        i + 1 -> column
      }
      .toMap
  }

  private def parseMoves(input: List[String]) = ZIO.succeed(input.map(Move.parse))

  def parseInput: ZIO[FileListInput, Exception, (CrateState, List[Move])] =
    for {
      input <- ZIO.service[FileListInput].map(identity)
      header = input.list.takeWhile(s => s.nonEmpty)
      body   = input.list.drop(header.length + 1)
      moves <- parseMoves(body)
      state <- parseHeader(header)
    } yield (state, moves)

  type CrateState  = Map[Int, CrateColumn]
  type CrateColumn = List[String]
  object CrateColumn {
    def take(column: CrateColumn, count: Int): (List[String], CrateColumn) =
      (column.takeRight(count), column.dropRight(count))
  }
  case class Move(count: Int, from: Int, to: Int)
  object Move {
    private val moveRegex = "move (\\d+) from (\\d+) to (\\d+)".r
    def parse(input: String): Move = input match {
      case moveRegex(count, from, to) => Move(count.toInt, from.toInt, to.toInt)
    }

    def applyTo(move: Move, state: CrateState, v1: Boolean): CrateState = {
      val (crates, oldColumn)    = CrateColumn.take(state(move.from), move.count)
      val correctCreates         = if (v1) crates.reverse else crates
      val newColumn: CrateColumn = state(move.to) ++ correctCreates
      val updatedColumns: CrateState = Map(
        move.from -> oldColumn,
        move.to   -> newColumn
      )
      state.filter { case (k, v) => k != move.from && k != move.to } ++ updatedColumns
    }
  }

  val live = FileListInput.layer("/day5.txt")
}
