package crooy

import zio.*
object Day4 extends ZIOAppDefault {

  def run = (for {
    _       <- ZIO.logInfo("day 4")
    answer1 <- part1
    _       <- ZIO.logInfo(s"answer part 1 is $answer1")
    answer2 <- part2
    _       <- ZIO.logInfo(s"answer part 2 is $answer2")

  } yield ()).provide(live)

  val live = FileListInput.layer("/day4.txt")

  private def parseInput: ZIO[FileListInput, Exception, List[Assignment]] =
    ZIO.service[FileListInput].map { input =>
      input.list.map(Assignment.parse)
    }

  def part1: ZIO[FileListInput, Exception, Int] = for {
    assignments            <- parseInput
    assignmentsWithOverlap <- ZIO.succeed(assignments.filter(_.hasFullOverlap))
  } yield assignmentsWithOverlap.length

  def part2: ZIO[FileListInput, Exception, Int] = for {
    assignments            <- parseInput
    assignmentsWithOverlap <- ZIO.succeed(assignments.filter(_.hasPartialOverlap))
  } yield assignmentsWithOverlap.length

  case class Assignment(elf1: Section, elf2: Section)
  object Assignment {
    def parse(input: String): Assignment = input.split(',').toList match {
      case left :: right :: Nil => Assignment(Section.parse(left), Section.parse(right))
      case _                    => throw RuntimeException("should not happen")
    }

    implicit class AssignmentOps(current: Assignment) {
      def hasFullOverlap: Boolean    = Section.overlapFully(current.elf1, current.elf2).isDefined
      def hasPartialOverlap: Boolean = Section.overlap(current.elf1, current.elf2).nonEmpty
    }
  }
  case class Section(start: Int, end: Int) {
    lazy val range = Range.inclusive(start, end).toSet
  }
  object Section {
    def parse(input: String): Section = input.split('-').toList match {
      case left :: right :: Nil => Section(left.toInt, right.toInt)
      case _                    => throw RuntimeException("should not happen")
    }

    def overlapFully(current: Section, other: Section): Option[Section] = {
      val overlap = current.range.intersect(other.range)
      (overlap == current.range, overlap == other.range) match {
        case (true, _)      => Some(current)
        case (false, true)  => Some(other)
        case (false, false) => None
      }
    }

    def overlap(current: Section, other: Section): Set[Int] = current.range.intersect(other.range)

  }

}
