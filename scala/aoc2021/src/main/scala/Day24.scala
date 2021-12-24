import scala.annotation.tailrec
object Day24 extends PuzzleSolution:
  def title = "Arithmetic Logic Unit"

  def solve(input: scala.io.Source) =
    println("Part 1:")
    val program = readProgram(input.getLines)
    val coeff = readCoeff(input.reset.mkString)
    val verifyByProgram = (l: List[Int]) => runProgram(program)(l).getOrElse("z", 0)
    val verifyByRun = run(coeff)
    findFirstModelNumber(coeff, 9 to 1 by -1).map(highest =>
      println(s"${highest.mkString} (verification: ${verifyByProgram(highest)}, ${verifyByRun(highest)})"))
    println("Part 2:")
    findFirstModelNumber(coeff, 1 to 9).map(lowest =>
      println(s"${lowest.mkString} (verification: ${verifyByProgram(lowest)}, ${verifyByRun(lowest)})"))

  def toModelNumber(num: Long): List[Int] =
    @tailrec def runDivMod(num: Long, result: List[Int]): List[Int] =
      if num == 0 then result
      else runDivMod(num / 10, (num % 10).toInt::result)
    runDivMod(num, List.empty)

  def findFirstModelNumber(coeff: List[(Int, Int, Int)], order: Range): Option[List[Int]] =
    def fitDivisions(z: Long, params: List[(Int, Int, Int)]): Option[List[Int]] =
      params match
        case Nil if z == 0 => Some(List.empty)
        case Nil => None
        case (coeff@(a, b, c))::ps if a != 1 =>
          val requiredDigit = (z % 26 + b).toInt
          if requiredDigit < 1 || 9 < requiredDigit then None
          else fitDivisions(runForDigit(coeff, z, requiredDigit), ps).map(tail => requiredDigit::tail)
        case p::ps =>
          order.map(d => fitDivisions(runForDigit(p, z, d), ps).map(d::_)).filter(_.isDefined).map(_.get).headOption
    fitDivisions(0, coeff)

  def readCoeff(program: String): List[(Int, Int, Int)] =
    program.split("inp w").drop(1).map(chunk =>
      val rightOperands = chunk.linesIterator.drop(1).map(_.substring(6)).toList
      (rightOperands(3).toInt, rightOperands(4).toInt, rightOperands(14).toInt)).toList

  def runForDigit(params: (Int, Int, Int), acc: Long, digit: Int): Long =
    val (z0, (a, b, c)) = (acc, params)
    val cond = digit == (acc % 26 + b)
    val z1 = (z0 / a)
    if !cond then (z1 * 26) + digit + c else z1

  def run(coeff: List[(Int,Int,Int)])(number: List[Int]): Long =
    number.zip(coeff).foldLeft(0L)((z0, iter) =>
      val (num, params) = iter
      runForDigit(params, z0, num))

  def runProgram(program: Seq[Instruction])(number: List[Int]): Register =
    program.foldLeft((number, Map.empty[String, Long]))
      ((acc, instr) => instr(acc._1, acc._2))._2

  type Inputs = List[Int]
  type Register = Map[String, Long]
  type Instruction = (Inputs, Register) => (Inputs, Register)
  val binaryOp: ((Long, Long) => Long) => ((String, Register => Long) => Instruction) =
    fn => (v, get) => (i, register) =>
      val (operand0, operand1) = (register.getOrElse(v, 0L), get(register))
      val newVal = fn(operand0, operand1)
      (i, register.updated(v, newVal))
  val input: (String => Instruction) =
    v => (input, register) =>
      (input.tail, register.updated(v, input.head))
  val add = binaryOp(_ + _)
  val mul = binaryOp(_ * _)
  val div = binaryOp(_ / _)
  val mod = binaryOp(_ % _)
  val eql = binaryOp((a, b) => if a == b then 1 else 0)

  def readProgram(lines: Iterator[String]): Seq[Instruction] =
    val opMap = Map("add" -> add, "mul" -> mul, "div" -> div, "mod" -> mod, "eql" -> eql)
    lines.toSeq.map(line =>
      line match
        case s"inp $v" => input(v)
        case s"$op $v $b" if b(0).isLetter =>
          opMap.get(op).map(opFn => opFn(v, _.getOrElse(b, 0))).get
        case s"$op $v $b" =>
          opMap.get(op).map(opFn => opFn(v, _ => b.toInt)).get
        )
