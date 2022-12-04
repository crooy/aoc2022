package crooy

import zio.{ZIO, ZIOAppDefault, ZLayer}

object Day3 extends ZIOAppDefault {
  override def run = (for {
    _ <- ZIO.logInfo("Day 3")
    _ <- part1
    _ <- part2
  } yield ()).provide(live)

  private val live: ZLayer[Any, Exception, FileListInput] = FileListInput.layer("/day3.txt")

  def part1: ZIO[FileListInput, Exception, Int] = for {
    _ <- ZIO.logInfo("Day 3, part 1")
    _ <- ZIO.logDebug(s"valuemap is ${Rucksack.itemValues}")
    input <- parseInput
    _ <- ZIO.logDebug(s"input is $input")
    result <- sumPriorities(input)
    _ <- ZIO.logInfo(s"part1 is is $result")
  } yield result

  def part2: ZIO[FileListInput, Exception, Int] = for {
    _ <- ZIO.logInfo("Day 3, part 2")
    input <- parseInput
    groups <- groupElfs(input)
    _ <- ZIO.logDebug(s"groupElfs is is $groups")
    result <- sumPriorities2(groups)
    _ <- ZIO.logInfo(s"part2 is is $result")
  } yield result

  def parseInput: ZIO[FileListInput, Exception, List[Rucksack]] = ZIO.service[FileListInput].map { input =>
    input.list.map(Rucksack.create)
  }

  private def sumPriorities(input: List[Rucksack]): ZIO[Any, Exception, Int] = ZIO.succeed(
    input.map(_.priority).sum
  )

  private def sumPriorities2(input: List[ElfGroup]): ZIO[Any, Exception, Int] = ZIO.succeed(
    input.map(_.priority).sum
  )

  private def groupElfs(input: List[Rucksack]): ZIO[Any, Exception, List[ElfGroup]] =
    ZIO.succeed(input.grouped(3).toList.map(ElfGroup.identify))

  case class ElfGroup(list: List[Rucksack], badge: Char) {
    def priority: Int = Rucksack.itemValues.get(badge).get
  }

  object ElfGroup {
    def identify(list: List[Rucksack]) = {
      val badges = list.map(_.fullSet).reduce((a, b) => a.intersect(b))
      if (badges.isEmpty){
        throw RuntimeException(s"no common badges found in $list")
      }
      ElfGroup(list, badges.head)
    }
  }

  case class Rucksack(compartment1: String, compartment2: String) {

    lazy val itemsInBoth: Set[Char] = compartment1.toSet.intersect(compartment2.toSet)
    lazy val fullSet = compartment1.toSet ++ compartment2.toSet

    def priority: Int = itemsInBoth.map(Rucksack.itemValues).sum

  }

  object Rucksack {
    def create(input: String): Rucksack = input.splitAt(input.length / 2) match {
      case (a, b) => Rucksack(a, b)
    }

    lazy val itemValues: Map[Char, Int] = {
      val lowerCaseStart = 'a'.toInt
      val upperCaseStart = 'A'.toInt
      Range(lowerCaseStart, lowerCaseStart + 26)
        .map(charValue => charValue.toChar -> (1 + charValue - lowerCaseStart))
        .toMap ++
        Range(upperCaseStart, upperCaseStart + 26)
          .map(charValue => charValue.toChar -> (27 + charValue - upperCaseStart))
          .toMap
    }
  }
}
