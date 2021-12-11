import org.junit.Test
import org.junit.Assert.*
import Day11.*

class Day11Test {

  val input = """5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526"""

  val smallSample = """11111
19991
19191
19991
11111"""

  @Test
  def smallSampleTest =
    val s = readOctopodes(smallSample.linesIterator.toSeq)
    assertEquals(9, countFlashers(step(s)))

  @Test
  def part1 =
    val o = readOctopodes(input.linesIterator.toSeq)
    assertEquals(0, countFlashers(o))
    val step1 = step(o)
    assertEquals(0, countFlashers(step1))
    val step2 = step(step1)
    assertEquals(35, countFlashers(step2))

  def printOctopodes(o: Octopodes) =
    println(
      (0 to 9)
        .map(y => (0 to 9).flatMap(x => o.get(Coord(x, y)).toSeq).mkString)
        .mkString("\n"))
}
