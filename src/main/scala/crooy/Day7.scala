package crooy

import scala.collection.immutable.Nil
import scala.util.Try

object Day7 {

  def main(args: Array[String]): Unit = {
    println("day 7")
    val testTree = buildtree(testInput1)
    println(testTree.indentedString(""))

    assert(testTree.size == 48381165, s"expected total size ${testTree.size} to equal 48381165")
    assert(part1(testTree) == 95437, s"expected ${part1(testTree)} to equal 95437")

    val liveTree = buildtree(live.list)

    val result1 = part1(liveTree)
    println(s"part 1 is $result1")

    assert(part2(testTree).name == "d", s"expected ${part2(testTree)} to equal d")
    assert(part2(testTree).size == 24933642, s"expected ${part2(testTree)} to equal 24933642")

    val result2 = part2(liveTree).size
    println(s"part 2 is $result2")
  }

  def part1(tree: TreeNode): Int =
    tree match {
      case dir: TreeNode.Dir if dir.size <= 100000 =>
        dir.size + dir.children.map(part1).sum
      case dir: TreeNode.Dir =>
        dir.children.map(part1).sum
      case _: TreeNode.File =>
        0
    }

  def part2(tree: TreeNode): TreeNode = {

    val free   = 70000000 - tree.size
    val needed = 30000000

    def findOptions(tree: TreeNode): List[TreeNode] =
      tree match {
        case dir: TreeNode.Dir if (dir.size + free) >= needed =>
          dir :: dir.children.flatMap(findOptions)
        case dir: TreeNode.Dir =>
          dir.children.flatMap(findOptions)
        case _: TreeNode.File => Nil
      }

    findOptions(tree).minBy(_.size)
  }

  def buildtree(input: List[String]): TreeNode = {
    val (_, tree) = TreeNode.parse(input)
    tree.head
  }

  sealed trait TreeNode {
    val name: String
    def indentedString(indents: String): String
    def size: Int
  }
  object TreeNode {
    case class Dir(name: String, children: List[TreeNode]) extends TreeNode {
      def size: Int = children.map(_.size).sum
      def indentedString(indents: String): String =
        s"$indents- $name (dir)\n" + children
          .sortBy(_.name)
          .map(_.indentedString(indents + "  "))
          .mkString("")
    }

    case class File(name: String, size: Int) extends TreeNode {
      def indentedString(indents: String): String = s"$indents- $name (file, size=$size)\n"
    }

    private val fileRegex = "(\\d+) (.*)".r
    private val dirRegex  = "dir (.*)".r
    private val cdRegex   = "[$] cd (.*)".r
    def parse(input: List[String]): (List[String], List[TreeNode]) = input match {
      case fileRegex(size, name) :: tail =>
        val node                  = File(name, size.toInt)
        val (remainder, siblings) = parse(tail)
        (remainder, node :: siblings)
      case cdRegex(name) :: "$ ls" :: tail =>
        val (remainder, children)  = parse(tail)
        val node                   = Dir(name, children)
        val (remainder2, siblings) = parse(remainder)
        (remainder2, node :: siblings)
      case dirRegex(_) :: tail =>
        parse(tail)
      case "$ cd .." :: tail =>
        (tail, Nil)
      case Nil => (Nil, Nil)
      case other =>
        throw RuntimeException(s"unexpected input $other")
    }

  }

  lazy val live = FileListInput.read("/day7.txt")

  val testInput1 =
    """$ cd /
      |$ ls
      |dir a
      |14848514 b.txt
      |8504156 c.dat
      |dir d
      |$ cd a
      |$ ls
      |dir e
      |29116 f
      |2557 g
      |62596 h.lst
      |$ cd e
      |$ ls
      |584 i
      |$ cd ..
      |$ cd ..
      |$ cd d
      |$ ls
      |4060174 j
      |8033020 d.log
      |5626152 d.ext
      |7214296 k""".stripMargin.split("\n").toList

}
