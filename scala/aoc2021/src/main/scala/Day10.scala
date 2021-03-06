import scala.annotation.tailrec
object Day10 extends PuzzleSolution:
  def title = "Syntax Scoring"

  def solve(input: scala.io.Source) =
    val lines = input.getLines.toList
    println("Part 1:")
    val parsedLines = lines map parseLine.andThen(score)
    val corrupt = parsedLines.flatMap(_.left.toSeq).sum
    println(corrupt)
    println("Part 2:")
    val incompleteScores = parsedLines.flatMap(_.toSeq).sorted
    println(incompleteScores.drop(incompleteScores.size / 2).head)

  def score(e: Either[Char, List[Char]]): Either[Int, Long] =
    e.map(_.map(scoreMissing).foldLeft(0L)((s, c) => s * 5 + c))
      .left.map(scoreUnmatched)

  // parses the given line and returns Left with an incorrect closing char if found, or Right with
  // the missing closing chars at the end of the line.
  def parseLine(line: String): Either[Char, List[Char]] =
    @tailrec def parseLineInner(stack: List[Char], remaining: List[Char]): Either[Char, List[Char]] =
      remaining match
        case Nil => Right(stack.map(matchFor))
        case x :: xs if openChars contains x => parseLineInner(x :: stack, xs)
        case x :: xs =>
            val top :: rest = stack
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
