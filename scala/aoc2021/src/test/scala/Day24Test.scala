import org.junit.Test
import org.junit.Assert.*
import Day24.*

class Day24Test:
  val programTimesThree = """inp z
inp x
mul z 3
eql z x"""
  @Test def sampleTimesThree =
    val program = readProgram(programTimesThree.linesIterator)
    assertEquals(1L, runProgram(program)(List(3, 9)).getOrElse("z", -1))
    assertEquals(0L, runProgram(program)(List(3, 8)).getOrElse("z", -1))

  val programBits = """inp w
add z w
mod z 2
div w 2
add y w
mod y 2
div w 2
add x w
mod x 2
div w 2
mod w 2"""
  @Test def sampleBits =
    val program = readProgram(programBits.linesIterator)
    assertEquals("1110", runProgram(program)(List(14)).toList.sortBy(_._1).map(_._2).mkString)
