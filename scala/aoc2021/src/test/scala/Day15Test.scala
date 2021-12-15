import org.junit.Test
import org.junit.Assert.*
import Day15.*

class Day15Test:
  val input = """1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581"""

  val expandedSample = """89123
91234
12345
23456
34567"""

  @Test def leastDangerousPathTest =
    val cave = readCave(input.linesIterator.toSeq)
    assertEquals(40, leastDangerousPath(cave, Coord(0, 0), cave.keys.max).risk)

  @Test def expandCaveTest =
    val cave = List((Coord(0, 0), 8)).toMap
    val expanded = expandCave(cave)
    assertEquals(readCave(expandedSample.linesIterator.toSeq), expanded)

  @Test def leastDangerousPathRealCaveTest =
    val realCave = expandCave(readCave(input.linesIterator.toSeq))
    assertEquals(315, leastDangerousPath(realCave, Coord(0, 0), realCave.keys.max).risk)
