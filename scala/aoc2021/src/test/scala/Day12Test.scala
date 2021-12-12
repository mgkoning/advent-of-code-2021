import org.junit.Test
import org.junit.Assert.*
import Day12.*

class Day12Test {

  @Test def findAllPathsTest =
    assertEquals(10, pathCount(smallSample, findAllPaths))
    assertEquals(19, pathCount(slightlyLargerSample, findAllPaths))
    assertEquals(226, pathCount(evenLargerSample, findAllPaths))

  def pathCount(s: String, find: Graph => List[Path]) =
    find(makeGraph(readConnections(s.linesIterator.toSeq))).size

  @Test def findAllPathsWithDuplicateTest =
    assertEquals(36, pathCount(smallSample, findAllPathsWithDuplicate))
    assertEquals(103, pathCount(slightlyLargerSample, findAllPathsWithDuplicate))
    assertEquals(3509, pathCount(evenLargerSample, findAllPathsWithDuplicate))

  val smallSample = """start-A
start-b
A-c
A-b
b-d
A-end
b-end"""

  val slightlyLargerSample = """dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc"""

  val evenLargerSample = """fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW"""
}
