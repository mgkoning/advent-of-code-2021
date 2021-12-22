object Day14 extends PuzzleSolution:
  def title = "Extended Polymerization"

  type Rules = Map[(Char, Char), Char]
  type PolymerPair = (Char, Char)
  type PolymerPairCount = (PolymerPair, Long)

  def solve(input: scala.io.Source) =
    val (polymer, rules) = parseInput(input.getLines.toSeq)
    println("Part 1:")
    val afterStep10 = resultAfter(rules, 10)(polymer)
    println(afterStep10.values.max - afterStep10.values.min)
    println("Part 2:")
    val lastChar = polymer.last
    val initialPolymerPairCounts = polymer.zip(polymer.drop(1))
      .groupBy(identity).mapValues(_.size.toLong).toList
    val after40 = LazyList.iterate(initialPolymerPairCounts)(stepPairwise(rules)).drop(40).head
    println(answer(lastChar, after40))
    println("Part 2 memoized:")
    val productionResult = runMemoized(rules)(polymer, 40)
    println(answer(lastChar, productionResult))

  def answer(lastChar: Char, pairCounts: List[PolymerPairCount]): Long =
    val charCounts = pairCounts.groupMap(_._1._1)(_._2)
      .map((c, v) => if c == lastChar then v.sum + 1 else v.sum)
    charCounts.max - charCounts.min

  def resultAfter(rules: Rules, steps: Int)(polymer: List[Char]): Map[Char, Int] =
    val afterSteps = LazyList.iterate(polymer)(step(rules)).drop(steps).head
    afterSteps.groupBy(identity).mapValues(_.size).toMap

  def stepPairwise(rules: Rules)(pairs: List[PolymerPairCount]): List[PolymerPairCount] =
    val applyRuleFor = applyRule(rules)
    val newPairs = pairs.flatMap((pair, count) => applyRuleFor(pair).map(newPair => (newPair, count)))
    newPairs.foldLeft(Map.empty[PolymerPair, Long])(
      (m, next) => 
        val (pair, count) = next
        m.updatedWith(pair)(v => v.map(_ + count).orElse(Some(count)))).toList

  def applyRule(rules: Rules)(pair: PolymerPair): List[PolymerPair] =
    val (left, right) = pair
    rules.get(pair).map(b => List((left, b), (b, right))).getOrElse(List(pair))

  type PairProductionCache = Map[(PolymerPair, Int), List[PolymerPairCount]]
  def runMemoized(rules: Rules)(polymer: List[Char], steps: Int): List[PolymerPairCount] =
    val applyRuleFor = applyRule(rules)

    def foldPairCount(pairs: List[PolymerPair], steps: Int, cache: PairProductionCache): (List[PolymerPairCount], PairProductionCache) =
      val startValue = (List.empty[PolymerPairCount], cache)
      pairs.foldLeft(startValue)((acc, pair) =>
        val (result, cache) = acc
        val (pairResults, updatedCache) = pairCount(pair, steps, cache)
        (result.concat(pairResults).groupBy(_._1).mapValues(_.map(_._2).sum).toList, updatedCache))

    def pairCount(pair: PolymerPair, steps: Int, cache: PairProductionCache): (List[PolymerPairCount], PairProductionCache) =
      if steps <= 0 then
        val pairCount = List((pair, 1L))
        (pairCount, cache.updated((pair, steps), pairCount))
      else cache.get((pair, steps)) match
        case Some(result) => (result, cache)
        case None =>
          val (result, updatedCache) = foldPairCount(applyRuleFor(pair), steps - 1, cache)
          (result, updatedCache.updated((pair, steps), result))

    val emptyCache = Map.empty[(PolymerPair, Int), List[PolymerPairCount]]
    val (total, _) = foldPairCount(polymer.zip(polymer.drop(1)), steps, emptyCache)
    total

  def step(rules: Rules)(polymer: List[Char]): List[Char] =
    def polymerize(remaining: List[Char], soFar: List[Char]): List[Char] =
      remaining match
        case Nil => soFar.reverse
        case x :: Nil => polymerize(Nil, x :: soFar)
        case a :: (rest @ b :: _) =>
          rules.get((a, b)) match
            case None => polymerize(rest, a :: soFar)
            case Some(c) => polymerize(rest, c :: a :: soFar)
    polymerize(polymer.toList, List.empty)

  def parseInput(lines: Seq[String]): (List[Char], Rules) =
    val polymer = lines.head
    val rules = lines.drop(2)
      .map(line => ((line.charAt(0), line.charAt(1)), line.charAt(6)))
      .toMap
    (polymer.toList, rules)

