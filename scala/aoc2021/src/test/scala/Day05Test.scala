import org.junit.Test
import org.junit.Assert.*
import Day05.*

class Day05Test {
  val input = """0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2"""

  @Test
  def part1 =
    val ventSpecs = parseInput(input)
    val allCoords = ventSpecs.flatMap(horVertCoords)
    assertEquals(5, countDuplicates(allCoords))

  @Test
  def part2 =
    val ventSpecs = parseInput(input)
    val horVert = ventSpecs.flatMap(horVertCoords)
    val diagonal = ventSpecs.flatMap(diagonalCoords)
    val allCoords = horVert.concat(diagonal)
    assertEquals(12, countDuplicates(allCoords))
}
