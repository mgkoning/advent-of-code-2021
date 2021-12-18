import scala.annotation.tailrec

object Day18 extends PuzzleSolution:
  def title = "Snailfish"

  sealed trait SnailfishNumber
  case class Pair(left: SnailfishNumber, right: SnailfishNumber) extends SnailfishNumber
  case class Num(value: Int) extends SnailfishNumber

  object SnailfishNumber:
    def show(n: SnailfishNumber): String =
      n match
        case Num(v) => v.toString
        case Pair(l, r) => s"[${show(l)},${show(r)}]"

    def plus(left: SnailfishNumber, right: SnailfishNumber): SnailfishNumber =
      reduce(Pair(left, right))

    def magSum(numbers: Seq[SnailfishNumber]): Int =
      magnitude(numbers.reduce(plus))

    type Exploding = (Option[Int], Option[Int])
    @tailrec def reduce(number: SnailfishNumber): SnailfishNumber =
      // Pushing a number to the right down a tree means taking the left-most
      // branches from that subtree since that is the number most immediately to
      // the right of the original tree. Likewise, pushing a number to the left
      // down a tree means taking the right-side branches.
      def pushDownRight(n: SnailfishNumber, value: Int): SnailfishNumber =
        n match
          case Num(v) => Num(v + value)
          case Pair(l, r) => Pair(pushDownRight(l, value), r)
      def pushDownLeft(n: SnailfishNumber, value: Int): SnailfishNumber =
        n match
          case Num(v) => Num(v + value)
          case Pair(l, r) => Pair(l, pushDownLeft(r, value))

      // Explode produces `Some` if an explosion occurred. Otherwise, this (sub)tree of expressions
      // did not match the criteria. `Exploding` holds the numbers to push to the left and to the
      // right. They will move up until they can be pushed into the subtree matching the direction
      // of the number. So, if a left child explodes, the left number goes up, but the right number
      // goes down to the right-side subtree. Likewise, a right child exploding means the left
      // number is immediately pushed down the left-side subtree and the right-side number goes up.
      // If no subtree to push down to is encountered, the number keeps moving up until the root is
      // reached and then it is discarded by `map(_._1)`.
      def explode(n: SnailfishNumber, nesting: Int = 0): Option[(SnailfishNumber, Exploding)] =
        n match
          case Num(_) => None
          case Pair(Num(l), Num(r)) if nesting == 4 =>
            Some((Num(0), (Some(l), Some(r))))
          case Pair(left, right) =>
            explode(left, nesting + 1) match
              case Some((result, (toLeft, Some(toRight)))) =>
                Some((Pair(result, pushDownRight(right, toRight)), (toLeft, None)))
              case Some((result, exploding)) =>
                Some((Pair(result, right), exploding))
              case None =>
                explode(right, nesting + 1) match
                  case None => None
                  case Some((result, (Some(toLeft), toRight))) =>
                    Some((Pair(pushDownLeft(left, toLeft), result), (None, toRight)))
                  case Some((result, exploding)) =>
                    Some(Pair(left, result), exploding)

      // Split returns `Some` if any of the numbers in this tree split.
      def split(number: SnailfishNumber): Option[SnailfishNumber] =
        number match
          case Num(v) if 9 < v => Some(Pair(Num(v / 2), Num(v - v / 2)))
          case Num(_) => None
          case Pair(left, right) =>
            split(left).map(Pair(_, right)).orElse(split(right).map(Pair(left, _)))

      // Try exploding a number. If that fails, split a number. If both return None, we're done;
      // otherwise, reduce the result of the explosion or split (whichever came first).
      explode(number).map(_._1).orElse(split(number)) match
        case Some(reduced) => reduce(reduced)
        case None => number

    def magnitude(number: SnailfishNumber): Int =
      number match
        case Num(i) => i
        case Pair(l, r) => 3 * magnitude(l) + 2 * magnitude(r)

  def solve(input: scala.io.Source) =
    println("Part 1:")
    val numbers = readNumbers(input.getLines.toSeq)
    println(SnailfishNumber.magSum(numbers))
    println("Part 2:")
    println(
      numbers.concat(numbers.reverse).combinations(2)
        .map(SnailfishNumber.magSum)
        .toList.sorted.reverse.head
    )

  def readNumbers(lines: Seq[String]): List[SnailfishNumber] =
    lines.map(readNumber).toList

  def readNumber(line: String): SnailfishNumber =
    // Use a stack to build the SnailfishNumber tree.
    // Whenever a ']' is encountered, the last two entries on the stack are
    // combined into a pair. Numbers are always single digits and they are
    // pushed on the stack as they're encountered: the ']' will pop them off and
    // combine them to a pair. We can skip both '[' and ','.
    // The result should be a list with one item, so we end with `.head`.
    line.toSeq.foldLeft(List.empty[SnailfishNumber])(
      (acc, char) =>
        char match
          case '[' | ',' => acc
          case ']' =>
            val right::left::rest = acc
            Pair(left, right)::rest
          case c if c.isDigit => Num(c - '0')::acc
    ).head
