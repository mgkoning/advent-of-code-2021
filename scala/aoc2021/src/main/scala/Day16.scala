import scala.annotation.tailrec

object Day16 extends PuzzleSolution:
  def title = "Packet Decoder"

  def solve(input: scala.io.Source) =
    println("Part 1:")
    val packet = Packet.parseFromHex(input.toSeq)
    println(getVersions(packet).sum)
    println("Part 2:")
    println(getValue(packet))

  def getVersions(packet: Packet): List[Long] =
    packet match
      case Literal(version, _) => List(version)
      case Operator(version, _, subPackets) => version :: (subPackets.flatMap(getVersions))

  def getValue(packet: Packet): Long =
    packet match
      case Literal(_, value) => value
      case Operator(_, packetType, packets) =>
        val values = packets.map(getValue)
        def boolBinaryOp(op: (Long, Long) => Boolean) =
          val List(left, right) = values
          if op(left, right) then 1L else 0L
        packetType match
          case Sum => values.sum
          case Product => values.product
          case Min => values.min
          case Max => values.max
          case GreaterThan => boolBinaryOp(_ > _)
          case LessThan => boolBinaryOp(_ < _)
          case EqualTo => boolBinaryOp(_ == _)

