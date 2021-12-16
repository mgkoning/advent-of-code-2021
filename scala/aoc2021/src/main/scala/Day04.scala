import scala.util.matching.Regex
import scala.io.Source

object Day04 extends PuzzleSolution:

  type Card = Map[Position, Square]

  case class Position(x: Int, y: Int)
  case class Square(number: Int, marked: Boolean = false)
  case class BingoCard(card: Card, bingo: Boolean = false)

  def title = "Giant Squid"

  def solve(input: Source) =
    val (rolls, cards) = parseInput(input.getLines)
    println("Part 1:")
    val winners = allWinners(rolls, cards)
    val (winningCard, winningNumber) = winners.head
    println(score(winningCard) * winningNumber)
    println("Part 2:")
    val (lastWinner, lastWinningNumber) = winners.last
    println(score(lastWinner) * lastWinningNumber)

  def allWinners(rolls: Seq[Int], cards: List[BingoCard]): Seq[(Card, Int)] =
    rolls
      // determine all game states, discard winners after the round they won
      .scanLeft(cards)
        ((bingoCards, number) => bingoCards.filter(!_.bingo).map(callNumber(number)))
      // find the winner for a round, if any.
      .map(cards => cards.find(_.bingo).map(_.card))
      // 'merge' with the rolls, skipping the initial game state
      .drop(1).zip(rolls)
      // only emit a new entry in the result sequence if we have a winner this round
      .flatMap((maybeCard, roll) => maybeCard.map(card => (card, roll)).toList)

  def score(card: Card): Int = card.values.filter(!_.marked).map(_.number).sum

  // Call the number on a specific bingo card. Mark the card and call Bingo!
  def callNumber(number: Int)(bingoCard: BingoCard): BingoCard =
    val (markedCard, foundAt) = mark(bingoCard.card, number)
    // we can only have Bingo! now if we just marked a square.
    val nowHasBingo = foundAt.map(p => hasBingo(markedCard, p)).getOrElse(false)
    BingoCard(markedCard, nowHasBingo)

  // Marks the card with the number, if found. The updated card is returned along with the position.
  def mark(card: Card, number: Int): (Card, Option[Position]) =
    card.find(_._2.number == number) match
      case Some((position, square)) =>
        (card.updated(position, square.copy(marked = true)), Some(position))
      case None => (card, None)

  def hasBingo(card: Card, intersection: Position): Boolean =
    def allPositionsMarked(positions: Seq[Position]) =
      positions.forall(pos => card.get(pos).map(_.marked).getOrElse(false))
    allPositionsMarked((0 until 5).map(i => intersection.copy(x = i)))
      || allPositionsMarked((0 until 5).map(i => intersection.copy(y = i)))

  def parseInput(input: Iterator[String]): (Seq[Int], List[BingoCard]) =
    val lines = input.toList
    val rolls = lines.head.split(',').map(_.toInt)
    val cards = lines.drop(1).grouped(6).map(c => parseCard(c.drop(1))).map(BingoCard(_)).toList
    (rolls, cards)

  val spaces: Regex = """\s+""".r
  def parseCard(lines: List[String]): Card =
    lines.zipWithIndex
      .flatMap(
        (line, y) => spaces.split(line.trim).zipWithIndex
          .map((number, x) => (Position(x, y), Square(number.toInt))))
      .toMap

