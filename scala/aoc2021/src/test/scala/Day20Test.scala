import org.junit.Test
import org.junit.Assert.*
import Day20.*

class Day20Test:
  val input = """..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#

#..#.
#....
##..#
..#..
..###"""

  @Test def part1Test =
    val (algo, pixels) = readInput(input.linesIterator.toSeq)
    val image = Image(pixels, false)
    assertEquals(10, image.knownPixels.count(_._2))
    val enhanceStep = enhance(algo)
    assertEquals(24, enhanceStep(image).knownPixels.count(_._2))
    assertEquals(35, enhanceStep(enhanceStep(image)).knownPixels.count(_._2))
