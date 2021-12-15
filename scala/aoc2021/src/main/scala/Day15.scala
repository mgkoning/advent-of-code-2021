import scala.collection.immutable.TreeSet

object Day15 extends PuzzleSolution:
  def title = "Chiton"

  case class Path(visited: List[Coord], risk: Int)
  object Path:
    def append(path: Path, next: (Coord, Int)): Path =
      val (coord, risk) = next
      Path(coord :: path.visited, path.risk + risk)
  implicit object PathOrdering extends Ordering[Path] {
    def compare(one: Path, other: Path) = one.risk compare other.risk
  }

  def solve(input: scala.io.Source) =
    val cave = readCave(input.getLines.toSeq)
    println("Part 1:")
    val pathToExit = leastDangerousPath(cave, Coord(0, 0), cave.keys.max)
    println(pathToExit.risk)
    println("Part 2:")
    val realCave = expandCave(cave)
    val pathToRealExit = leastDangerousPath(realCave, Coord(0, 0), realCave.keys.max)
    println(pathToRealExit.risk)

  def leastDangerousPath(cave: Map[Coord, Int], from: Coord, to: Coord): Path =
    // should probably use a more efficient data type than List for the priority queue
    def findPathInner(pathsToContinue: List[Path], visited: Set[Coord]): Path =
      val shortestPath @ Path(lastVisit :: _, _) = pathsToContinue.head
      val neighbors = Coord.adjacent4(lastVisit).filter(cave.contains).filter(!visited.contains(_))
      neighbors.find(_ == to) match
        case Some(destination) => Path.append(shortestPath, (destination, cave.getOrElse(destination, 10)))
        case _ =>
          val pathsToAdd = neighbors.map(n => Path.append(shortestPath, (n, cave.getOrElse(n, 10))))
          findPathInner((pathsToContinue.tail ++ pathsToAdd).sorted, visited ++ neighbors)
    findPathInner(List(Path(List(from), 0)), Set(from))

  def expandCave(cave: Map[Coord, Int]): Map[Coord, Int] =
    val max = cave.keys.max
    def shiftBy(dx: Int, dy: Int)(part: (Coord, Int)): (Coord, Int) =
      val (Coord(x, y), risk) = part
      ((Coord(x + dx * (max.x + 1), y + dy * (max.y + 1))), (risk + dx + dy - 1) % 9 + 1)
    val otherParts =
      for x <- 0 to 4
          y <- 0 to 4
          if x + y != 0
      yield cave.map(shiftBy(x, y))
    otherParts.foldLeft(cave)((m1, m2) => m1 ++ m2)

  def readCave(lines: Seq[String]): Map[Coord, Int] =
    Input.readGrid(lines, _.toSeq, _ - '0')
