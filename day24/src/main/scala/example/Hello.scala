package example
import scala.util.parsing.combinator._
import better.files.Resource

object InpurParser extends RegexParsers with JavaTokenParsers {

  def instruction: Parser[Instruction] = {inp | add | mul | div | mod | eql}

  def inp: Parser[Instruction] = {
      literal("inp") ~ variable ^^ { case _ ~ a => Inp(a)}
  }
    def add: Parser[Instruction] = {
      literal("add") ~ variable ~ placeholder ^^ { case _ ~ a ~ b => Add(a, b)}
  }
    def mul: Parser[Instruction] = {
      literal("mul") ~ variable ~ placeholder ^^ { case _ ~ a ~ b => Mul(a, b)}
  }
    def div: Parser[Instruction] = {
      literal("div") ~ variable ~ placeholder ^^ { case _ ~ a ~ b => Div(a, b)}
  }
    def mod: Parser[Instruction] = {
      literal("mod") ~ variable ~ placeholder ^^ { case _ ~ a ~ b => Mod(a, b)}
  }
    def eql: Parser[Instruction] = {
      literal("eql") ~ variable ~ placeholder ^^ { case _ ~ a ~ b => Eql(a, b)}
  }
  def placeholder: Parser[PlaceHolder] = variable | num

  def num: Parser[PlaceHolder] = {wholeNumber ^^ {str => Num(str.toInt)}}

  def variable: Parser[Variable] = x | y | z | w
  def x: Parser[Variable] = {literal("x") ^^ { _ => X}}
  def y: Parser[Variable] = {literal("y") ^^ { _ => Y}}
  def z: Parser[Variable] = {literal("z") ^^ { _ => Z}}
  def w: Parser[Variable] = {literal("w") ^^ { _ => W}}
}

 
object Hello extends App {
  def parseData(input: String): List[Instruction] = {
    input.split("\n").toList.map(str => InpurParser.parse(InpurParser.instruction, str).get)
  }
  def run(instructions: List[Instruction], input: List[Int]): State = {
    instructions.foldLeft((State.initial, input)){
      case ((st, x :: xs), Inp(ph)) =>
        (st.store(ph, x), xs)
      case ((st, input), Add(a, b)) =>
        (st.store(a, st.get(a) + st.get(b)), input)
      case ((st, input), Mul(a, b)) =>
        (st.store(a, st.get(a) * st.get(b)), input)
      case ((st, input), Div(a, b)) =>
        (st.store(a, st.get(a) / st.get(b)), input)
      case ((st, input), Mod(a, b)) =>
        (st.store(a, st.get(a) % st.get(b)), input)
      case ((st, input), Eql(a, b)) =>
        (st.store(a, if (st.get(a) == st.get(b)) 1 else 0), input)
      case _ => throw new IllegalStateException()
    }
  }._1
  val data = parseData(Resource.getAsString("input.txt"))
  println(run(data, List(1, 3, 5, 7, 9, 2, 4, 6, 8, 9, 9, 9, 9, 9)))
}


final case class State(values: Map[Variable, Long]) {
  def store(ph: Variable, value: Long): State =
    State(values + (ph -> value))

  def get(ph: PlaceHolder): Long = ph match {
    case Num(n) => n
    case other: Variable => values.getOrElse(other, 0)
  }
}

object State {
  val initial = State(Map.empty)
}

sealed trait Instruction
final case class Inp(a: Variable) extends Instruction
final case class Add(a: Variable, b: PlaceHolder) extends Instruction
final case class Mul(a: Variable, b: PlaceHolder) extends Instruction
final case class Div(a: Variable, b: PlaceHolder) extends Instruction
final case class Mod(a: Variable, b: PlaceHolder) extends Instruction
final case class Eql(a: Variable, b: PlaceHolder) extends Instruction

sealed trait PlaceHolder
final case class Num(value: Int) extends PlaceHolder
sealed trait Variable extends PlaceHolder
case object W extends Variable
case object X extends Variable
case object Y extends Variable
case object Z extends Variable
