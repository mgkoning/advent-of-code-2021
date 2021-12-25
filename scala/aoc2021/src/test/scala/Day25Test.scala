import org.junit.{Test,Ignore}
import org.junit.Assert.*
import Day25.*

class Day25Test:
  val part1Example = """v...>>.vv>
.vv>>.vv..
>>.>v>...v
>>v>>.>.v.
v>v.vv.v..
>.>>..v...
.vv..>.>v.
v.v..>>v.v
....v..v.>"""

  @Test def part1Test =
    val cucumbers = readCucumbers(part1Example.linesIterator)
    assertEquals(58, findSafeTime(cucumbers))

  @Test def stepTest =
    val input = "...>>>>>..."
    val cucumbers = readCucumbers(Seq(input).iterator)
    val stepped = step(cucumbers)
    assertEquals(1, stepped.eastFacing.diff(cucumbers.eastFacing).size)
    val stepped2 = step(stepped)
    assertEquals(2, stepped2.eastFacing.diff(stepped.eastFacing).size)

  @Test def stepEastSouthTest =
    val input = """..........
.>v....v..
.......>..
.........."""
    val cucumbers = readCucumbers(input.linesIterator)
    val stepped = step(cucumbers)
    assertEquals(1, stepped.eastFacing.diff(cucumbers.eastFacing).size)
    assertEquals(2, stepped.southFacing.diff(cucumbers.southFacing).size)

