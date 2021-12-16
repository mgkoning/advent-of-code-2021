import org.junit.Test
import org.junit.Assert.*

class Day03Test:
  val input = """00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010"""

  @Test def part2() =
    assertEquals((23, 10), Day03.oxygenCo2(input.linesIterator.map(_.toSeq).toList))
