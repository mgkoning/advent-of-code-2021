import scala.annotation.tailrec

object Day25 extends PuzzleSolution:
  def title = "Sea Cucumber"

  case class TrenchMap(width: Int, height: Int, eastFacing: Set[Coord], southFacing: Set[Coord])
  object TrenchMap:
    def occupied(m: TrenchMap, coord: Coord): Boolean =
      m.eastFacing.contains(coord) || m.southFacing.contains(coord)

  def solve(input: scala.io.Source) =
    println("Part 1:")
    val cucumbers = readCucumbers(input.getLines)
    println(findSafeTime(cucumbers))

  @tailrec def findSafeTime(map: TrenchMap, time: Long = 1L): Long =
    val next = step(map)
    if map == next then time else findSafeTime(next, time + 1)

  def step(trenchMap: TrenchMap): TrenchMap =
    def moveSouth(m: TrenchMap): TrenchMap =
      m.copy(southFacing = m.southFacing.map(c =>
        val next = c.copy(y = (c.y + 1) % trenchMap.height)
        if TrenchMap.occupied(m, next) then c else next))
    def moveEast(m: TrenchMap): TrenchMap =
      m.copy(eastFacing = m.eastFacing.map(c =>
        val next = c.copy(x = (c.x + 1) % trenchMap.width)
        if TrenchMap.occupied(m, next) then c else next))
    moveSouth(moveEast(trenchMap))

  def readCucumbers(lines: Iterator[String]): TrenchMap =
    val grid = Input.readGrid(lines.toSeq, _.toSeq, identity)
    val size = grid.keys.max
    def coordsWhere(ch: Char) = grid.filter(_._2 == ch).map(_._1).toSet
    TrenchMap(size.x + 1, size.y + 1, coordsWhere('>'), coordsWhere('v'))

  def showMap(m: TrenchMap): String =
    (0 to m.height - 1).map(y =>
      (0 to m.width - 1).map(x =>
        val c = Coord(x, y)
        if m.eastFacing.contains(c) then '>'
        else if m.southFacing.contains(c) then 'v'
        else '.').mkString
      ).mkString("\n")