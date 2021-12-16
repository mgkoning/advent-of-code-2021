import scala.io.Source

object Day08 extends PuzzleSolution:
  def title = "Seven Segment Search"

  type Digit = String

  case class Display(signals: Seq[Digit], output: Seq[Digit])

  case class DecodedDisplay(display: Display, signalsMap: Map[Digit, Int])

  def solve(input: Source) =
    val inputs = parseInput(input.getLines())
    println("Part 1:")
    val decodedDisplays = inputs.map(decode)
    val part1 = decodedDisplays.map(findUniqueNumberDigits).sum
    println(part1)

    println("Part 2:")
    val part2 = decodedDisplays.map(displayNumber).sum
    println(part2)

  def decode(display: Display): DecodedDisplay =
    val sortedSignals = display.signals.sortBy(_.size).toList
    val List(one, seven, four, _, _, _, _, _, _, eight) = sortedSignals
    val fiveSegments = sortedSignals.drop(3).take(3)
    val sixSegments = sortedSignals.drop(6).take(3)
    val three = fiveSegments.find(overlap(_, one) == 2).get
    val five = fiveSegments.find(x => overlap(x, four) == 3 && x != three).get
    val two = fiveSegments.find(x => x != three && x != five).get
    val nine = sixSegments.find(overlap(_, four) == 4).get
    val six = sixSegments.find(x => overlap(x, five) == 5 && x != nine).get
    val zero = sixSegments.find(x => x != nine && x != six).get
    DecodedDisplay(
      display,
      Seq(zero, one, two, three, four, five, six, seven, eight, nine).zipWithIndex.toMap
    )

  val uniqueNumberDigits = Set(1, 4, 7, 8)
  def findUniqueNumberDigits(decoded: DecodedDisplay): Int =
    val DecodedDisplay(Display(_, output), signalsMap) = decoded
    output.count(o => uniqueNumberDigits.contains(signalsMap.getOrElse(o, -1)))

  def displayNumber(decoded: DecodedDisplay): Int =
    val DecodedDisplay(Display(_, output), signalsMap) = decoded
    output.map(signalsMap.getOrElse(_, -1))
      .foldLeft(0)((sum, next) => sum * 10 + next)

  def overlap(one: Digit, other: Digit): Int = one.intersect(other).size

  def parseInput(lines: Iterator[String]): Seq[Display] =
    def sortedDigit(digit: Digit) = String(digit.toArray.sorted)
    def getDigits(values: String) = values.split(' ').map(sortedDigit)
    def parseLine(line: String) = line match
      case s"$signals | $output" => Display(getDigits(signals), getDigits(output))
    lines.toSeq.map(parseLine)
