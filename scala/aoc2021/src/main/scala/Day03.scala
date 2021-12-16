import scala.annotation.tailrec
import scala.io.Source

object Day03 extends PuzzleSolution:
  val bitValues = (0 to 15).map(scala.math.pow(2, _).longValue).toList

  def title = "Binary Diagnostic"

  def solve(input: Source): Unit =
    val lines = input.getLines.map(_.toSeq).toList
    println("Part 1:")
    val (gammaRate, epsilonRate) = gammaEpsilon(lines)
    println(gammaRate * epsilonRate)
    println("Part 2:")
    val (oxygen, co2) = oxygenCo2(lines)
    println(oxygen * co2)

  def gammaEpsilon(lines: Seq[Seq[Char]]) =
    val result = lines.foldLeft(Accumulator.zero)(Accumulator.add)
    val gammaBits = result.oneBits
    val epsilonBits = gammaBits.map(1 - _)
    (binaryToDecimal(gammaBits), binaryToDecimal(epsilonBits))

  def oxygenCo2(lines: List[Seq[Char]]) =
    def accumulateAtIndex(l: List[Seq[Char]], index: Int) =
      l.map(_.drop(index).take(1)).foldLeft(Accumulator.zero)(Accumulator.add)
    @tailrec def rating(
      lines: List[Seq[Char]], index: Int, ifMoreOnes: Char, ifMoreZeroes: Char): Long =
      lines match
        case List(l) => binaryToDecimal(l.map(bitToInt))
        case List(_*) =>
          val acc = accumulateAtIndex(lines, index)
          val oneCount = acc.oneCounts.head
          val desiredBit = if acc.count - oneCount <= oneCount then ifMoreOnes else ifMoreZeroes
          val linesRemaining = lines.filter(_.drop(index).head == desiredBit)
          rating(linesRemaining, index + 1, ifMoreOnes, ifMoreZeroes)

    val oxygen = rating(lines, 0, '1', '0')
    val co2 = rating(lines, 0, '0', '1')
    (oxygen, co2)

  def bitToInt(bit: Char) = if bit == '1' then 1 else 0

  def binaryToDecimal(bits: Seq[Int]) =
    bits.reverse.zip(bitValues).map((b, v) => b * v).sum

  case class Accumulator(count: Int, oneCounts: Seq[Int]):
    def oneBits = oneCounts.map(i => if count - i < i then 1 else 0)
  object Accumulator:
    val zero = Accumulator(0, Seq.fill(15)(0))
    def add(acc: Accumulator, bits: Seq[Char]) = Accumulator(
      acc.count + 1,
      acc.oneCounts.zip(bits).map((c, b) => c + bitToInt(b))
    )

