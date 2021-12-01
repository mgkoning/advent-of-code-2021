import org.junit.Test
import org.junit.Assert.*

class Day01Test:
  val input =
    """199
200
208
210
200
207
240
269
260
263"""

  @Test def part1(): Unit =
    assertEquals(7, Day01.differences(input.linesIterator.map(_.toInt).toList).filter(0 < _).length)
