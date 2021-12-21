import org.junit.Test
import org.junit.Assert.*
import Day21.*

class Day21Test:
  @Test def part2 =
    val realGame = playGame(Game(Pawn(PlayerOne, 4, 0), Pawn(PlayerTwo, 8, 0)))
    val winCounts = realGame.positions
      .toList
      .flatMap((game, count) => game.winner(21).map(p => (p.player, count)))
      .groupBy(_._1)
      .mapValues(_.map(_._2).sum)
    assertEquals(444356092776315L, winCounts.values.max)
    assertEquals(341960390180808L, winCounts.values.min)
