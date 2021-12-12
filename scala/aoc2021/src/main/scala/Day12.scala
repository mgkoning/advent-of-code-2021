import scala.annotation.tailrec
object Day12 extends PuzzleSolution {
  def title = "Passage Pathing"

  sealed trait Area
  object Area:
    def smallOption(area: Area): Option[Area] = area match
      case SmallCave(_) => Some(area)
      case _ => None
  case object Start extends Area
  case object End extends Area
  case class SmallCave(name: String) extends Area
  case class LargeCave(name: String) extends Area

  type Connection = (Area, Area)
  type Graph = Map[Area, Set[Area]]
  type Path = List[Area]

  def solve(input: scala.io.Source) =
    val connections = readConnections(input.getLines.toSeq)
    println("Part 1:")
    val graph = makeGraph(connections)
    println(findAllPaths(graph).size)
    println("Part 2:")
    println(findAllPathsWithDuplicate(graph).size)

  def findAllPaths(graph: Graph): List[Path] =
    def findPathsFrom(path: Path): List[Path] =
      val next = path.head
      next match
        case End => List(path)
        case _ =>
          val visitedSmall = path.map(Area.smallOption).flatMap(_.toSeq).toSet
          val possible = graph.getOrElse(next, Set.empty).filter(!visitedSmall.contains(_))
          possible.toList.flatMap(p => findPathsFrom(p :: path))
    findPathsFrom(List(Start))

  def findAllPathsWithDuplicate(graph: Graph): List[Path] =
    case class PathWithDup(areas: Path, duplicateSmallCave: Option[Area])
    def findPathsFrom(path: PathWithDup): List[Path] =
      val PathWithDup(areas, duplicateSmallCave) = path
      val next = areas.head
      next match
        case End => List(areas)
        case _ =>
          val visitedSmall = areas.map(Area.smallOption).flatMap(_.toSeq).toSet
          val possible = graph.getOrElse(next, Set.empty)
          val allowed = possible.filter(!visitedSmall.contains(_)).toList
          val allowedDuplicates = duplicateSmallCave match
            case Some(_) => List.empty
            case None => possible.filter(visitedSmall.contains(_)).toList
          allowed.flatMap(p => findPathsFrom(path.copy(areas = p :: areas)))
            .concat(allowedDuplicates.flatMap(d => findPathsFrom(PathWithDup(d :: areas, Some(d)))))
    findPathsFrom(PathWithDup(List(Start), None))

  def makeGraph(connections: Seq[Connection]): Graph =
    def addDestination(destination: Area)(current: Option[Set[Area]]): Option[Set[Area]] =
      Some(current.getOrElse(Set.empty) + destination)
    connections.foldLeft(Map.empty)
      ((graph, connection) => connection match
        // Since Start and End are special, make sure the graph reflects their status.
        case Start -> x => graph.updatedWith(Start)(addDestination(x))
        case x -> Start => graph.updatedWith(Start)(addDestination(x))
        case End -> x => graph.updatedWith(x)(addDestination(End))
        case x -> End => graph.updatedWith(x)(addDestination(End))
        // Otherwise, add the connection and its reverse.
        case from -> to => graph
          .updatedWith(from)(addDestination(to))
          .updatedWith(to)(addDestination(from)))

  def readConnections(lines: Seq[String]): Seq[Connection] =
    def toArea(s: String) = s match
      case "start" => Start
      case "end" => End
      case _ if s.forall(_.isLower) => SmallCave(s)
      case _ => LargeCave(s)
    def toConnection(line: String) = line match
      case s"$from-$to" => toArea(from) -> toArea(to)
    lines.map(toConnection)
}
