package crooy

import zio.*

object Day1 extends ZIOAppDefault {
  override def run =
    day1.provide(FileListInput.layer("/day1.txt"))

  def day1: ZIO[FileListInput, Exception, Unit] =
    for {
      _           <- ZIO.logInfo("Running day 1")
      elves       <- splitElves
      sortedElves <- ZIO.succeed(elves.sortBy(_.calories).reverse)
      top1 = sortedElves.head
      top3 = sortedElves.take(3)
      _ <- ZIO.logInfo(s"answer top1 is ${top1.index} with ${top1.calories} calories")
      _ <- ZIO.logInfo(
        s"answer top3 is ${top3.map(_.index)} with ${top3.map(_.calories).sum} calories"
      )
    } yield ()

  def splitElves: ZIO[FileListInput, Exception, List[Elf]] =
    ZIO.service[FileListInput].flatMap { input =>
      for {
        _ <- ZIO.logDebug(s"last item on list is ${input.list.last}, first is ${input.list.head}")
        elves <- ZIO.succeed(recursiveSplitAt(input.list, Elf.zero))
      } yield elves
    }

  private def recursiveSplitAt(list: List[String], current: Elf): List[Elf] =
    list match {
      case "" :: Nil =>
        current :: Nil
      case "" :: rest =>
        current :: recursiveSplitAt(rest, Elf.next(current))
      case value :: rest =>
        recursiveSplitAt(rest, Elf.addTo(current, value))
      case Nil =>
        current :: Nil

    }

}
