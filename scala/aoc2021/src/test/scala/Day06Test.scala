import org.junit.Test
import org.junit.Assert.*
import Day06.*

class Day06Test:
  val input = "3,4,3,1,2"

  @Test
  def part1 =
    val ages = readAges(input.map(identity))
    assertEquals(26, lanternfishAfterDays(18, ages))
    assertEquals(5934, lanternfishAfterDays(80, ages))
    assertEquals(26984457539L, lanternfishAfterDays(256, ages))

