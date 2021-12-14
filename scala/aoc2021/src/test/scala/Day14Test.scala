import org.junit.Test
import org.junit.Assert.*
import Day14.*

class Day14Test {
  val input = """NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C"""

  @Test def parseInputTest =
    val (polymer, rules) = parseInput(input.linesIterator.toSeq)
    assertEquals("NNCB", polymer.mkString)
    assertEquals(16, rules.size)
    assertEquals(Some('B'), rules.get(('N', 'B')))

  @Test def stepTest =
    val (polymer, rules) = parseInput(input.linesIterator.toSeq)
    assertEquals("NCNBCHB", step(rules)(polymer).mkString)

}
