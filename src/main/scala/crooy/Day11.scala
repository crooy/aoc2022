package crooy

object Day11 {

  def testInput = List(
    Monkey.create(
      0,
      List(79, 98),
      (x) => x * 19,
      (x) => if (x % 23 == 0) 2 else 3
    ),
    Monkey.create(
      1,
      List(54, 65, 75, 74),
      (x) => x + 6,
      (x) => if (x % 19 == 0) 2 else 0
    ),
    Monkey.create(
      2,
      List(79, 60, 97),
      (x) => x * x,
      (x) => if (x % 13 == 0) 1 else 3
    ),
    Monkey.create(
      3,
      List(74),
      (x) => x + 3,
      (x) => if (x % 17 == 0) 0 else 1
    )
  )

  def live = List(
    Monkey.create(
      0,
      List(92, 73, 86, 83, 65, 51, 55, 93),
      (x) => x * 5,
      (x) => if (x % 11 == 0) 3 else 4
    ),
    Monkey.create(
      1,
      List(99, 67, 62, 61, 59, 98),
      (x) => x * x,
      (x) => if (x % 2 == 0) 6 else 7
    ),
    Monkey.create(
      2,
      List(81, 89, 56, 61, 99),
      (x) => x * 7,
      (x) => if (x % 5 == 0) 1 else 5
    ),
    Monkey.create(
      3,
      List(97, 74, 68),
      (x) => x + 1,
      (x) => if (x % 17 == 0) 2 else 5
    ),
    Monkey.create(
      4,
      List(78, 73),
      (x) => x + 3,
      (x) => if (x % 19 == 0) 2 else 3
    ),
    Monkey.create(
      5,
      List(50),
      (x) => x + 5,
      (x) => if (x % 7 == 0) 1 else 6
    ),
    Monkey.create(
      6,
      List(95, 88, 53, 75),
      (x) => x + 8,
      (x) => if (x % 3 == 0) 0 else 7
    ),
    Monkey.create(
      7,
      List(50, 77, 98, 85, 94, 56, 89),
      (x) => x + 2,
      (x) => if (x % 13 == 0) 4 else 0
    )
  )
  def main(args: Array[String]): Unit = {
//    val testOutput = part1(testInput)
//    assert(testOutput == 10605, s"$testOutput is not 10605")
//    println(s"test 1 is good")
//    val answer = part1(live)
//    println(s"answer 1 is $answer")

    val testOutput2 = part2(testInput, 23 * 19 * 13 * 17L)
    val expected    = 2713310158L
    assert(testOutput2 == expected, s"part2 $testOutput2 is not 2713310158")
    val answer2 = part2(live, 11 * 2 * 5 * 17 * 19 * 7 * 3 * 13L)
    println(s"answer 2 is $answer2")
  }

  def part2(monkeys: List[Monkey], divisor: Long): BigInt = {
    val lowerStressLevels = false
    Range
      .inclusive(1, 10000)
      .foreach(i => {
        monkeys.foreach(m => m.processItems(monkeys, divisor))
        if (i % 1000 == 0 || i == 1 || i == 20) {

          println(s"[run $i]")
          val stats = monkeys.map(m => m.nr -> m.history.length).toMap
          println(s"stats $stats")
        }
      })
    val stats = monkeys.map(m => m.nr -> m.history.length.toLong).toMap
    println(s"stats $stats")
    val top2 = stats.values.toList.sorted.reverse.take(2).map(BigInt(_))
    println(s"top2 $top2")
    top2.head * top2.last
  }

  def part1(monkeys: List[Monkey]): Long = {
    val lowerStressLevels = true
    Range
      .inclusive(1, 20)
      .foreach(i => {
        println(s"[run $i]")
        monkeys.foreach(m => m.processItems(monkeys, 3))
      })
    val stats = monkeys.map(m => m.nr -> m.history.length.toLong).toMap
    println(s"stats $stats")
    val top2 = stats.values.toList.sorted.reverse.take(2)
    println(s"top2 $top2")
    top2.head * top2.last
  }

  type WorryLevel = BigInt
  type MonkeyNr   = Int
  type Item       = BigInt
  type State      = List[(MonkeyNr, Item)]
  case class Monkey(
      nr: MonkeyNr,
      items: scala.collection.mutable.ListBuffer[Item],
      history: scala.collection.mutable.ListBuffer[Item],
      operation: (WorryLevel) => WorryLevel,
      test: (WorryLevel) => MonkeyNr
  ) {
    def processItems(monkeys: List[Monkey], divisor: Long): Unit = {
      val itemsCopy = items.toList
      history.addAll(itemsCopy.map(_ => 1))
//      println(s"monkey $nr inspecint ${itemsCopy.length} items")
      items.clear()
      itemsCopy.foreach(monkeyDo(_, monkeys, divisor))
    }

    private def monkeyDo(
        item: Item,
        monkeys: List[Monkey],
        divisor: Long
    ): Unit = {
      val duringInspection = operation(item)

      val afterInspection =
        if (divisor == 3) duringInspection / 3
        else {
          if (duringInspection > divisor) {
            duringInspection % divisor
          } else duringInspection
        }
//      assert(
//        duringInspection / 3 == Math.floor(duringInspection / 3.0),
//        s"duringInspection $duringInspection, ${Math.floor(duringInspection / 3.0)} != ${duringInspection / 3}"
//      )
      assert(duringInspection > 0, s"$duringInspection")
      assert(afterInspection > 0, s"$afterInspection")
      val newMonkey         = test(afterInspection)
      val newMonkeyInstance = monkeys(newMonkey)
      assert(newMonkey == newMonkeyInstance.nr)
//      println(s"monkey $nr changed item $item to $afterInspection, throwing to $newMonkey")
      newMonkeyInstance.catchItem(afterInspection)
    }

    def catchItem(item: Item): Unit = {
//      println(s"monkey $nr catches $item")
      items.addOne(item)
    }
  }
  object Monkey {
    def create(
        nr: MonkeyNr,
        items: List[Item],
        operation: (WorryLevel) => WorryLevel,
        test: (WorryLevel) => MonkeyNr
    ): Monkey = Monkey(
      nr,
      scala.collection.mutable.ListBuffer().addAll(items),
      scala.collection.mutable.ListBuffer(),
      operation,
      test
    )
  }

}
