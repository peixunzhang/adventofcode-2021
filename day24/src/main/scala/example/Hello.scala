package example
import scala.util.parsing.combinator._
import better.files.Resource

object InpurParser extends RegexParsers with JavaTokenParsers {

  def instruction: Parser[Instruction] = {inp | add | mul | div | mod | eql}

  def inp: Parser[Instruction] = {
      literal("inp") ~ variable ^^ { case _ ~ a => Instruction.Inp(a)}
  }
    def add: Parser[Instruction] = {
      literal("add") ~ variable ~ placeholder ^^ { case _ ~ a ~ b => Instruction.Add(a, b)}
  }
    def mul: Parser[Instruction] = {
      literal("mul") ~ variable ~ placeholder ^^ { case _ ~ a ~ b => Instruction.Mul(a, b)}
  }
    def div: Parser[Instruction] = {
      literal("div") ~ variable ~ placeholder ^^ { case _ ~ a ~ b => Instruction.Div(a, b)}
  }
    def mod: Parser[Instruction] = {
      literal("mod") ~ variable ~ placeholder ^^ { case _ ~ a ~ b => Instruction.Mod(a, b)}
  }
    def eql: Parser[Instruction] = {
      literal("eql") ~ variable ~ placeholder ^^ { case _ ~ a ~ b => Instruction.Eql(a, b)}
  }
  def placeholder: Parser[PlaceHolder] = variable | num

  def num: Parser[PlaceHolder] = {wholeNumber ^^ {str => Num(str.toInt)}}

  def variable: Parser[Variable] = x | y | z | w
  def w: Parser[Variable] = {literal("w") ^^ { _ => Variable.W}}
  def x: Parser[Variable] = {literal("x") ^^ { _ => Variable.X}}
  def y: Parser[Variable] = {literal("y") ^^ { _ => Variable.Y}}
  def z: Parser[Variable] = {literal("z") ^^ { _ => Variable.Z}}
}

 
object Hello extends App {
  def parseData(input: String): List[Instruction] = {
    input.split("\n").toList.map(str => InpurParser.parse(InpurParser.instruction, str).get)
  }

  def interpret[V](
    instructions: List[Instruction],
    input: List[Int],
    initialState: State[V],
    onInp: Int => V,
    onAdd: (V, V) => V,
    onMult: (V, V) => V,
    onDiv: (V, V) => V,
    onMod: (V, V) => V,
    onEql: (V, V) => V,
  ): State[V] =
    instructions.foldLeft((initialState, input)) {
      case ((st, x :: xs), Instruction.Inp(ph)) =>
        (st.store(ph, onInp(x)), xs)
      case ((st, input), Instruction.Add(a, b)) =>
        (st.store(a, onAdd(st.get(a), st.get(b))), input)
      case ((st, input), Instruction.Mul(a, b)) =>
        (st.store(a, onMult(st.get(a), st.get(b))), input)
      case ((st, input), Instruction.Div(a, b)) =>
        (st.store(a, onDiv(st.get(a), st.get(b))), input)
      case ((st, input), Instruction.Mod(a, b)) =>
        (st.store(a, onMod(st.get(a), st.get(b))), input)
      case ((st, input), Instruction.Eql(a, b)) =>
        (st.store(a, onEql(st.get(a), st.get(b))), input)
      case _ => throw new IllegalStateException("exhausted input")      
    }._1

  def run(instructions: List[Instruction], input: List[Int]): State[Long] =
    interpret(
      instructions, 
      input, 
      State.initial(0, _.toLong),
      _.toLong,
      _ + _,
      _ * _,
      _ / _,
      _ % _,
      (a, b) => if (a == b) 1 else 0
    )
  
  def runExpr(instructions: List[Instruction], input: List[Int]): State[Expr] =
    interpret[Expr](
      instructions,
      input,
      State.initial(Expr.Constant(0), Expr.Constant(_)),
      Expr.Var(_),
      Expr.Add(_, _),
      Expr.Mul(_, _),
      Expr.Div(_, _),
      Expr.Mod(_, _),
      Expr.Eql(_, _)
    )

  val data = parseData(Resource.getAsString("input.txt"))
  println(runExpr(data, List(1, 3, 5, 7, 9, 2, 4, 6, 8, 9, 9, 9, 9, 9)).get(Variable.Z).show)
}


final case class State[V](
  values: Map[Variable, V],
  default: V,
  liftConstant: Int => V
) {
  def store(ph: Variable, value: V): State[V] =
    copy(values = values + (ph -> value))

  def get(ph: PlaceHolder): V = ph match {
    case Num(n) => liftConstant(n)
    case other: Variable => values.getOrElse(other, default)
  }
}

object State {
  def initial[V](default: V, liftConstant: Int => V) = State(Map.empty, default, liftConstant)
}

sealed trait Instruction
object Instruction {
  final case class Inp(a: Variable) extends Instruction
  final case class Add(a: Variable, b: PlaceHolder) extends Instruction
  final case class Mul(a: Variable, b: PlaceHolder) extends Instruction
  final case class Div(a: Variable, b: PlaceHolder) extends Instruction
  final case class Mod(a: Variable, b: PlaceHolder) extends Instruction
  final case class Eql(a: Variable, b: PlaceHolder) extends Instruction
}

sealed trait Expr { self =>
  import Expr._

  def show: String =
    self match {
      case Constant(value) => value.toString()
      case Var(value) => s"Var($value)"
      case Add(a, b) => s"(${a.show} + ${b.show})"
      case Mul(a, b) => s"(${a.show} * ${b.show})"
      case Div(a, b) => s"(${a.show} / ${b.show})"
      case Mod(a, b) => s"(${a.show} % ${b.show})"
      case Eql(a, b) => s"(${a.show} == ${b.show})"
    }
}
object Expr {
  final case class Constant(value: Int) extends Expr
  final case class Var(value: Int) extends Expr
  final case class Add(a: Expr, b: Expr) extends Expr
  final case class Mul(a: Expr, b: Expr) extends Expr
  final case class Div(a: Expr, b: Expr) extends Expr
  final case class Mod(a: Expr, b: Expr) extends Expr
  final case class Eql(a: Expr, b: Expr) extends Expr  
}

sealed trait PlaceHolder

final case class Num(value: Int) extends PlaceHolder

sealed trait Variable extends PlaceHolder
object Variable {
  case object W extends Variable
  case object X extends Variable
  case object Y extends Variable
  case object Z extends Variable
}
