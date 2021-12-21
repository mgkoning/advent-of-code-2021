object Day21 extends PuzzleSolution:
  def title = "Dirac Dice"

  sealed trait Player
  case object PlayerOne extends Player
  case object PlayerTwo extends Player
  case class Pawn(player: Player, position: Int, score: Int = 0)
  case class Game(current: Pawn, next: Pawn, turns: Int = 0):
    def winner(target: Int): Option[Pawn] =
      Seq(current, next).find(target <= _.score)
    def hasWinner(target: Int): Boolean = winner(target).isDefined

  case class RealGame(positions: Map[Game, Long])

  def solve(input: scala.io.Source) =
    println("Part 1:")
    val startPositions = readStartingPositions(input.getLines.toSeq)
    val game = practiceGame(startPositions)
    println(game.turns * 3 * Math.min(game.current.score, game.next.score))
    println("Part 2:")
    val realGame = playGame(startPositions)
    val winCounts = realGame.positions
      .toList
      .flatMap((game, count) => game.winner(21).map(p => (p.player, count)))
      .groupBy(_._1)
      .mapValues(_.map(_._2).sum)
    println(winCounts.values.max)

  def practiceGame(startPositions: Game): Game =
    val rolls = LazyList.continually(List.range(1, 101)).flatten.grouped(3)
    val states = rolls.scanLeft(startPositions)((game, rolls) =>
      game.copy(current = game.next, next = addRoll(game.current, rolls.sum), turns = game.turns + 1))
    states.dropWhile(!_.hasWinner(1000)).next

  def playGame(startPositions: Game): RealGame =
    def play(game: RealGame): RealGame =
      val (inProgress, completed) = game.positions.partition((game, _) => !game.hasWinner(21))
      if inProgress.isEmpty then game
      else
        val newPositions = inProgress.toSeq.flatMap(game =>
            val (Game(current, next, turns), gameCount) = game
            addPossibleRolls(current)
              .map(newCurrent => (Game(next, newCurrent, turns + 1), gameCount)))
        val newGame = RealGame(
          newPositions.foldLeft(completed)((m, pos) =>
            val (game, count) = pos
            m.updatedWith(game)(v => Some(v.getOrElse(0L) + count))))
        play(newGame)
    play(RealGame(Map.empty.updated(startPositions, 1L)))

  val possibleRolls = (for a <- 1 to 3; b <- 1 to 3; c <- 1 to 3 yield a + b + c).toList
  def addPossibleRolls(player: Pawn): Seq[Pawn] =
    possibleRolls.map(roll => addRoll(player, roll))

  def addRoll(player: Pawn, roll: Int): Pawn =
    val newPosition = (player.position + roll - 1) % 10 + 1
    player.copy(position = newPosition, player.score + newPosition)

  def readStartingPositions(lines: Seq[String]): Game =
    val Seq(player1, player2) = lines
    Game(Pawn(PlayerOne, player1.last - '0'), Pawn(PlayerTwo, player2.last - '0'))