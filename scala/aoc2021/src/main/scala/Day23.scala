import scala.annotation.tailrec
object Day23 extends PuzzleSolution:
  def title = "Amphipod"

  def solve(input: scala.io.Source) =
    // val initialPositions = readInitialPositions(input.getLines)
    println("Part 1:")
    // val result = organize(initialPositions)
    // println(result)
    
    // no generic solutions - just the moves as worked out on paper.
    println(5*1000 + 8*1000 + 8*100 + 4*100 + 6*10 + 7*10 + 8 + 8)
    println("Part 2:")
    println(
      7*1 + 7*100 + 4*10 + 8*1 + 5*10 + 2*100 + 7*10 + 8*1 + 8*10 + 5*100 + 6*100 + 6*1000 + 3*1 +
      7*100 + 9*10 + 9*1000 + 10*1000 + 10*1000 + 10*1000 + 9*100 + 5*1 + 5*1 + 9*1 +9*1)

  // --- Everything below this line is just for nostalgia purposes --- //
  sealed trait Location
  case class Hallway(space: Int) extends Location
  case class SideRoom(homeOf: Char, space: Int) extends Location
  val topRow = List(SideRoom('A',1), SideRoom('B',1),SideRoom('C',1),SideRoom('D',1))
  val bottomRow = List(SideRoom('A',2), SideRoom('B',2),SideRoom('C',2),SideRoom('D',2))

  case class Amphipod(kind: Char, position: Location)

  type Edge = (Location, Location)
  def makeEdges(connected: Seq[(Location, Location)]): Seq[Edge] =
    connected.flatMap(t => List(t, t.swap))
  val hallway = (0 to 10).map(Hallway.apply)
  val hallwayEdges = makeEdges(hallway.zip(hallway.drop(1)))
  val sideRoomEdges = makeEdges(topRow.zip(bottomRow))
  val outsideSideRoom = (2 to 8 by 2).map(Hallway.apply)
  val sideRoomToHallwayEdges = makeEdges(outsideSideRoom.zip(topRow)).toList
  val graph = hallwayEdges.concat(sideRoomEdges).concat(sideRoomToHallwayEdges).groupMap(_._1)(_._2)

  def energyRequired(kind: Char): Int =
    kind match { case 'A' => 1; case 'B' => 10; case 'C' => 100; case 'D' => 1000 }

  def legalHallwayStop(location: Hallway) = !outsideSideRoom.contains(location)

  case class State(positions: List[Amphipod], energyExpended: Long, heuristic: (Long, Long, Long, Long))
  val validHallwayMoves = hallway.diff(outsideSideRoom).toList
  def organize(startPositions: List[Amphipod]): Long =
    def isDone(candidate: Amphipod, all: List[Amphipod]): Boolean =
      candidate.position match
        case Hallway(_) => false
        case SideRoom(homeOf, _) if homeOf != candidate.kind => false
        case SideRoom(_, position) if position == 2 => true
        case SideRoom(_, _) =>
          val other = all.filter(o => o != candidate && o.kind == candidate.kind).head
          isDone(other, all)

    @tailrec def organizeStep(states: List[State], minimumSoFar: Long): Long =
      states match
        case Nil => minimumSoFar
        case state::remaining =>
            val State(positions, energyExpended, _) = state
            val notDone = positions.filter(!isDone(_, positions))
            if notDone.isEmpty then
              val newMinimum = math.min(minimumSoFar, state.energyExpended)
              println(s"Successful move: ${state.energyExpended}, min ${newMinimum}")
              organizeStep(remaining.filter(s => s.energyExpended < newMinimum), newMinimum)
            else
              //showState(state)
              val moves = notDone.flatMap(findLegalMoves(state)).filter(s => s.energyExpended < minimumSoFar).sortBy(_.heuristic)
              organizeStep(moves:::remaining, minimumSoFar)

    def occupant(location: Location, others: List[Amphipod]): Option[Amphipod] =
      others.find(_.position == location)

    def findLegalMoves(state: State)(mover: Amphipod): List[State] =
      val others = state.positions.filter(_ != mover)
      val bottomHome = SideRoom(mover.kind, 2)
      val bottomHomeOccupiedCorrectly =
        occupant(bottomHome, others).map(o => bottomHome.homeOf == o.kind).getOrElse(false)
      val destinations: Seq[Location] = mover.position match
        case Hallway(_) =>
          val moveToBottomHome = occupant(bottomHome, others).map(_ => List.empty).getOrElse(List(bottomHome))
          val topHome = SideRoom(mover.kind, 1)
          val moveToTopHome =
            if !bottomHomeOccupiedCorrectly then List.empty
            else occupant(topHome, others).map(_ => List.empty).getOrElse(List(topHome))
          moveToTopHome ::: moveToBottomHome
        case SideRoom(mover.kind, 2) => List.empty
        case SideRoom(mover.kind, 1) if bottomHomeOccupiedCorrectly => List.empty
        case SideRoom(_, _) => validHallwayMoves.filter(occupant(_, others).isEmpty)
      val result = destinations.toList.flatMap(destination =>
        determineDistance(mover.position, destination, others).map(distance =>
          //println(s"Add move from ${mover.position} to $destination (${showState(state)})")
          val positions = mover.copy(position = destination)::others
          State(positions, state.energyExpended + distance * energyRequired(mover.kind),
            determineHeuristic(positions))))
      result

    def determineHeuristic(state: List[Amphipod]) =
      def heuristicFor(kind: Char): Long =
        state.filter(_.kind == kind)
          .map(a => determineDistance(a.position, SideRoom(kind, 1), List.empty).getOrElse(100000)).sum
      (heuristicFor('D'), heuristicFor('C'), heuristicFor('B'), heuristicFor('A'))
    def determineDistance(from: Location, to: Location, others: List[Amphipod]): Option[Int] =
      val occupied = others.map(_.position).toSet
      def findPath(toVisit: List[(List[Location], Int)], seen: Set[Location]): Option[Int] =
        toVisit match
          case Nil => None
          case v::vs =>
            val (visited, distance) = v
            val moves = graph.getOrElse(visited.head, List.empty)
              .filter(!seen.contains(_))
              .filter(!occupied.contains(_))
            if moves.exists(_ == to) then Some(distance + 1)
            else
              val nextMoves = moves.map(m => (m::visited, distance + 1)).toList
              findPath((nextMoves ::: vs).sortBy(_._2), seen ++ moves)
      findPath(List((List(from), 0)),Set(from))

    organizeStep(List(State(startPositions, 0, (0, 0, 0, 0))), 1000000L)

  def showState(state: State): String =
    def showLocation(loc: Location) =
      loc match { case Hallway(i) => s"H$i"; case SideRoom(h, i) => s"$h$i" }
    state.positions.map(a => s"${a.kind}@${showLocation(a.position)}").sorted.mkString(",")
      + s"@${state.energyExpended}"
  def readInitialPositions(lines: Iterator[String]): List[Amphipod] =
    lines.toSeq.foldLeft(List.empty)((list, line) =>
      line match
        case s"###$a#$b#$c#$d###" if a != "" =>
          list ++ List(a, b, c, d).map(_(0)).zip(topRow).map(Amphipod.apply)
        case s"  #$a#$b#$c#$d#" if a != "" =>
          list ++ List(a, b, c, d).map(_(0)).zip(bottomRow).map(Amphipod.apply)
        case _ => list)
