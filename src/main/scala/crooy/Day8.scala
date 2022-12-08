package crooy
import scala.collection.parallel.CollectionConverters._
object Day8 {

  def main(args: Array[String]): Unit = {
    println("day 8")
    val testForest = buildForest(testInput)
    assertSquare(testForest)
    val testPart1 = countVisibleTrees(testForest)
    testPart1.filter(t => t.x == 0 || t.y == 0 || t.x == 4 || t.y == 4).map(println)
    assert(
      testPart1.length == 21,
      s"expecting test part 1 to be 21, it is ${testPart1.length}"
    )

    val liveForest = buildForest(live.list)
    val answer1    = countVisibleTrees(liveForest)
    println(s"part 1 answer is ${answer1.length}")

    val testForestFlat = testForest.flatten

    val test1 = testForestFlat
      .find(t => t.x == 2 && t.y == 3)
      .map(TreeHouseLocation.create(_, testForestFlat))
    println(s"testing for 2,3 = ${test1}, with ${test1.map(_.scenicScore)}, expecting 8")
    assert(test1.get.scenicScore == 8)

    val test2 = testForestFlat
      .find(t => t.x == 2 && t.y == 1)
      .map(TreeHouseLocation.create(_, testForestFlat))
    println(s"testing for 2,1 = ${test2}, with ${test2.map(_.scenicScore)}, expecting 4")
    assert(test2.get.scenicScore == 4)

    val test3 = testForestFlat
      .find(t => t.x == 1 && t.y == 2)
      .map(TreeHouseLocation.create(_, testForestFlat))
    println(s"testing for 1,2 = ${test3}, with ${test3.map(_.scenicScore)}, expecting 6")
    assert(test3.get.scenicScore == 6)

    val testAnswer2 = part2(testForestFlat, testForest.length)
    println(s"test forest answer 2 is ${testAnswer2} with ${testAnswer2.scenicScore}")

    val answer2 = part2(liveForest.flatten, liveForest.length)
    println(s"part 2 answer is ${answer2.scenicScore}")

  }

  def part2(forest: List[Tree], size: Int) = {
    val houses = forest.par
      .map(TreeHouseLocation.create(_, forest))

    val scores = houses
      .map(_.scenicScore)
      .seq
      .toList
      .grouped(size)
      .map(_.map(s => f"$s%06.0f").mkString("."))
      .mkString("\n")
    println(s"scores is \n${scores}")

    houses.maxBy(_.scenicScore)

  }

  private def countVisibleTrees(forest: List[List[Tree]]): List[Tree] = (
    forest.map(row => findVisibleTreesInLine(row, -1) ++ findVisibleTreesInLine(row.reverse, -1))
      ++
        forest.transpose.map(row =>
          findVisibleTreesInLine(row, -1) ++ findVisibleTreesInLine(row.reverse, -1)
        )
  ).flatten.distinct

  def findVisibleTreesInLine(line: List[Tree], tallestSoFar: Int): List[Tree] = line match {
    case tree :: tail if tree.height > tallestSoFar =>
      tree :: findVisibleTreesInLine(tail, tree.height)
    case _ :: tail => findVisibleTreesInLine(tail, tallestSoFar)
    case Nil       => Nil
  }

  def buildForest(input: List[String]): List[List[Tree]] =
    input.map(_.toList.map(_.toString.toInt)).zipWithIndex.map { case (line, y) =>
      line.zipWithIndex.map { case (height, x) =>
        Tree(x, y, height)
      }
    }

  case class TreeHouseLocation(tree: Tree, inView: Set[Tree]) {
    lazy val scenicScore: Int = {
      val viewable = inView
        .groupBy(t => (t.x.compare(tree.x), t.y.compare(tree.y)))
        .view
        .mapValues(_.size)
        .toMap

      viewable.getOrElse((-1, 0), 0) *
        viewable.getOrElse((1, 0), 0) *
        viewable.getOrElse((0, 1), 0) *
        viewable.getOrElse((0, -1), 0)
    }
  }
  object TreeHouseLocation {
    def create(location: Tree, fullForest: List[Tree]) = {

      def visibleNeighbours(tree: Tree, forest: Set[Tree]): Set[Tree] = {
        val myNeighbours = forest
          .filter(_.isNeighbour(tree))
          .filter(_.isInLineWith(tree, location))

        val neighboursVisible = myNeighbours
          .filter(_.height < location.height)

        myNeighbours ++ neighboursVisible
          .flatMap(t =>
            visibleNeighbours(
              t,
              forest.diff(myNeighbours)
            )
          )
      }

      val notMe = fullForest.filter(t => t != location).toSet
      TreeHouseLocation(location, visibleNeighbours(location, notMe))
    }
  }
  case class Tree(x: Int, y: Int, height: Int) {

    def distance(other: Tree): Int = Math.abs(other.x - x) + (Math.abs(other.y - y))
    def isNeighbour(other: Tree): Boolean =
      distance(other) == 1 && isInLineWith(this, this)

    def isInLineWith(start: Tree, end: Tree): Boolean =
      (start.x == end.x && end.x == x) || (start.y == end.y && end.y == y)

    override def equals(obj: Any): Boolean =
      if (obj.isInstanceOf[Tree]) {
        obj.asInstanceOf[Tree].x == x && obj.asInstanceOf[Tree].y == y && obj
          .asInstanceOf[Tree]
          .height == height
      } else super.equals(obj)
  }

  def assertSquare(input: List[List[Tree]]) = {
    val expected = input.length
    input.map(line => assert(line.length == expected, "not square"))
  }

  val testInput = """30373
                    |25512
                    |65332
                    |33549
                    |35390""".stripMargin.split("\n").toList

  lazy val live = FileListInput.read("/day8.txt")
}
