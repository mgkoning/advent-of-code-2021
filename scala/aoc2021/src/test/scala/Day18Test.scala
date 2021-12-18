import org.junit.Test
import org.junit.Assert.*
import Day18.*

class Day18Test:
  val homework1 = """[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
[7,[5,[[3,8],[1,4]]]]
[[2,[2,2]],[8,[8,1]]]
[2,9]
[1,[[[9,3],9],[[9,0],[0,7]]]]
[[[5,[7,4]],7],1]
[[[[4,2],2],6],[8,7]]"""

  val homework2 = """[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"""

  @Test def readNumberTest =
    assertEquals(Pair(Num(1), Num(2)), readNumber("[1,2]"))
    assertEquals(Pair(Pair(Num(1), Num(2)), Num(3)), readNumber("[[1,2],3]"))
    assertEquals(Pair(Num(9), Pair(Num(8), Num(7))), readNumber("[9,[8,7]]"))
    assertEquals(Pair(Pair(Num(1), Num(9)), Pair(Num(8), Num(7))), readNumber("[[1,9],[8,7]]"))

  @Test def magnitudeTest =
    assertEquals(29, SnailfishNumber.magnitude(Pair(Num(9), Num(1))))
    assertEquals(
      3488,
      SnailfishNumber.magnitude(readNumber("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"))
    )

  @Test def reduceTest =
    assertEquals(Pair(Num(1), Num(2)), SnailfishNumber.reduce(Pair(Num(1), Num(2))))
    assertEquals(
      readNumber("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"),
      SnailfishNumber.plus(readNumber("[[[[4,3],4],4],[7,[[8,4],9]]]"), Pair(Num(1), Num(1)))
    )

  @Test def splitTest =
    assertEquals(Pair(Num(5), Num(5)), SnailfishNumber.reduce(Num(10)))
    assertEquals(Pair(Num(5), Num(6)), SnailfishNumber.reduce(Num(11)))
    assertEquals(Pair(Num(6), Num(6)), SnailfishNumber.reduce(Num(12)))
    assertEquals(Pair(Num(1), Pair(Num(6), Num(6))), SnailfishNumber.reduce(Pair(Num(1), Num(12))))

  @Test def homeworkTest =
    assertEquals(3488, SnailfishNumber.magSum(readNumbers(homework1.linesIterator.toSeq)))
    assertEquals(4140, SnailfishNumber.magSum(readNumbers(homework2.linesIterator.toSeq)))

  @Test def showTest =
    assertEquals(
      "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]",
      SnailfishNumber.show(readNumber("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]")))
    assertEquals(
      "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]",
      SnailfishNumber.show(
        SnailfishNumber.plus(readNumber("[[[[4,3],4],4],[7,[[8,4],9]]]"),readNumber("[1,1]")))
    )