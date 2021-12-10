import scala.annotation.tailrec
object Day10 extends PuzzleSolution {
  def title = "Syntax Scoring"

  def solve(input: scala.io.Source) =
    val lines = input.getLines.toSeq
    println("Part 1:")
    val parsedLines = lines.map(parseLine).toList
    val corrupt = parsedLines.map(scoreCorrupt).sum
    println(corrupt)
    println("Part 2:")
    val incompleteScores = parsedLines.map(scoreIncomplete).flatMap(_.toSeq).sorted
    println(incompleteScores.drop(incompleteScores.size / 2).head)

  def scoreCorrupt(e: Either[Char, List[Char]]): Int =
    e.fold(scoreUnmatched, Function.const(0))

  def scoreIncomplete(e: Either[Char, List[Char]]): Either[Char, Long] =
    e.map(cs => cs.foldLeft(0L)((s, c) => s * 5 + scoreMissing(c)))

  // parses the given line and returns Left with an incorrect closing char if found, or Right with
  // the missing closing chars at the end of the line.
  def parseLine(line: String): Either[Char, List[Char]] =
    @tailrec def parseLineInner(stack: List[Char], remaining: List[Char]): Either[Char, List[Char]] =
      remaining match
        case Nil => Right(stack.map(matchFor))
        case (x :: xs) if openChars.contains(x) => parseLineInner(x :: stack, xs)
        case (x :: xs) =>
            val (top :: rest) = stack
            if x == matchFor(top)
              then parseLineInner(rest, xs)
              else Left(x)
    parseLineInner(List.empty, line.toList)

  val openChars = Set('(', '[', '{', '<')

  def matchFor(open: Char): Char =
    open match
      case '(' => ')'
      case '[' => ']'
      case '{' => '}'
      case '<' => '>'

  def scoreUnmatched(close: Char): Int =
    close match
      case ')' => 3
      case ']' => 57
      case '}' => 1197
      case '>' => 25137

  def scoreMissing(close: Char): Int =
    close match
      case ')' => 1
      case ']' => 2
      case '}' => 3
      case '>' => 4

}
