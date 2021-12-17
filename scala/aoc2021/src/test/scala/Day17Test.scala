import org.junit.Test
import org.junit.Assert.*
import Day17.*

class Day17Test:
  val input = readTargetArea(List("target area: x=20..30, y=-10..-5"))

  @Test def testPart1 =
    assertEquals(45, findTrickShot(findTrajectoriesToHit(input)))

  @Test def testPart2 =
    assertEquals(112, findTrajectoriesToHit(input).length)