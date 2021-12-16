import scala.io.Source
import scala.annotation.tailrec

object Day09 extends PuzzleSolution:
  def title = "Smoke Basin"

  def solve(input: Source) =
    val ventMap = readInput(input.getLines.toSeq)
    println("Part 1:")
    val lowPoints = findLowPoints(ventMap)
    println(lowPoints.values.map(_ + 1).sum)
    println("Part 2:")
    val basins = lowPoints.keys.map(findBasinSize(ventMap)).toList
    println(basins.sorted.reverse.take(3).product)

  def findLowPoints(ventMap: Map[Coord, Int]): Map[Coord, Int] =
    def isLowPoint(c: Coord, height: Int) =
      Coord.adjacent4(c).forall(height < ventMap.getOrElse(_, 10))
    ventMap.filter(isLowPoint)

  def findBasinSize(ventMap: Map[Coord, Int])(from: Coord): Int =
    @tailrec def findBasinSizeInner(toVisit: List[Coord], seen: Set[Coord], size: Int): Int =
      if toVisit.isEmpty then size
      else
        val next = toVisit.head
        val neighbors = Coord.adjacent4(next)
          .filter(!seen.contains(_))
          .filter(ventMap.getOrElse(_, 10) < 9)
        findBasinSizeInner(toVisit.tail ++ neighbors, seen ++ neighbors, size + neighbors.size)
    findBasinSizeInner(List(from), Set(from), 1)

  def readInput(lines: Seq[String]): Map[Coord, Int] =
    Input.readGrid(lines, _.toSeq, _ - '0')

