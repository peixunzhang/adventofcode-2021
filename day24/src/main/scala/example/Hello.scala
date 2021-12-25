package example
import scala.util.parsing.combinator._
import better.files.Resource

object InputParser extends RegexParsers with JavaTokenParsers {

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
    input.split("\n").toList.map(str => InputParser.parse(InputParser.instruction, str).get)
  }

  def interpret[V](
    instructions: List[Instruction],
    input: List[Int],
    default: Variable => V,
    liftConstant: Int => V,
    onInp: Int => V,
    onAdd: (V, V) => V,
    onMult: (V, V) => V,
    onDiv: (V, V) => V,
    onMod: (V, V) => V,
    onEql: (V, V) => V,
  ): State[V] =
    instructions.foldLeft((State.initial[V](default, liftConstant), input)) {
      case ((st, x :: xs), Instruction.Inp(ph)) =>
        (st.store(ph, onInp(x)), xs)
      case ((st, input), Instruction.Add(a, b)) =>
        val result = onAdd(st.get(a), st.get(b))
        (st.store(a, result), input)
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
      _ => 0,
      _.toLong,
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
      Expr.VarRef(_),
      Expr.Constant(_),
      Expr.Input(_),
      _ + _,
      _ * _,
      _ / _,
      _ % _,
      _ eql _
    )

  val data = parseData(Resource.getAsString("input.txt"))
  //vprintln(findHighest(data))
  println(runExpr(data, List(1)).get(Variable.Z))
  // val expr = runExpr(data, List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)).get(Variable.X)
  // println(expr)
  // println(run(data, List(1, 3, 5, 7, 9, 2, 4, 6, 8, 9, 9, 9, 9, 9)).get(Variable.Z))
}


final case class State[V](
  values: Map[Variable, V],
  default: Variable => V,
  liftConstant: Int => V
) {
  def store(ph: Variable, value: V): State[V] =
    copy(values = values + (ph -> value))

  def get(ph: PlaceHolder): V = ph match {
    case Num(n) => liftConstant(n)
    case other: Variable => values.getOrElse(other, default(other))
  }
}

object State {
  def initial[V](default: Variable => V, liftConstant: Int => V) = State(Map.empty, default, liftConstant)
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

  def findInputRefs: Set[Int] =
    self match {
      case Input(v) => Set(v)
      case Add(a, b) => a.findInputRefs ++ b.findInputRefs
      case Mul(a, b) => a.findInputRefs ++ b.findInputRefs
      case Div(a, b) => a.findInputRefs ++ b.findInputRefs
      case Mod(a, b) => a.findInputRefs ++ b.findInputRefs
      case Constant(_) => Set.empty
      case Eql(a, b) => a.findInputRefs ++ b.findInputRefs
      case VarRef(_) => Set.empty
    }

  def +(that: Expr) : Expr =
    (self, that) match {
      case (s, Constant(0)) => s
      case (Constant(0), t) => t
      case (s, t) if s == t =>
        Mul(s, Constant(2))
      case (s, t) => Add(s, t)
    }

  def *(that: Expr) : Expr =
    (self, that) match {
      case (s, Constant(1)) => s
      case (Constant(1), t) => t
      case (_, Constant(0)) => Constant(0)
      case (Constant(0), _) => Constant(0)
      case (s, t) => Mul(s, t)
    }

  def /(that: Expr) : Expr =
    (self, that) match {
      case (s, Constant(1)) => s
      case (s, t) => Div(s, t)
    }

  def %(that: Expr) : Expr =
    (self, that) match {
      case (_, Constant(0)) => Constant(0)
      case (Constant(0), _) => Constant(0)
      case (s, t) => Mod(s, t)
    }

  def eql(that: Expr): Expr =
    (self, that) match {
      case (Constant(s), Constant(t)) if s == t => Constant(1)
      case (Constant(s), Constant(t)) if s != t => Constant(0)
      case (s, t) => Eql(s, t)
    }


  override def toString(): String = {
    val sb = new StringBuilder()
    showHelper(sb)
    sb.toString()
  }

  private def showHelper(sb: StringBuilder): Unit =
    self match {
      case Constant(value) => sb.addAll(s"Constant($value)")
      case Input(value) => sb.addAll(s"Input($value)")
      case Add(a, b) =>
        sb.addOne('(')
        a.showHelper(sb)
        sb.addAll(" + ")
        b.showHelper(sb)
        sb.addOne(')')
      case Mul(a, b) =>
        sb.addOne('(')
        a.showHelper(sb)
        sb.addAll(" * ")
        b.showHelper(sb)
        sb.addOne(')')
      case Div(a, b) =>
        sb.addOne('(')
        a.showHelper(sb)
        sb.addAll(" / ")
        b.showHelper(sb)
        sb.addOne(')')
      case Mod(a, b) =>
        sb.addOne('(')
        a.showHelper(sb)
        sb.addAll(" % ")
        b.showHelper(sb)
        sb.addOne(')')
      case Eql(a, b) =>
        sb.addOne('(')
        a.showHelper(sb)
        sb.addAll(" == ")
        b.showHelper(sb)
        sb.addOne(')')
      case VarRef(variable) =>
        sb.addAll(variable.toString())
    }
}
object Expr {
  final case class Constant(value: Int) extends Expr
  final case class Input(value: Int) extends Expr
  final case class Add(a: Expr, b: Expr) extends Expr
  final case class Mul(a: Expr, b: Expr) extends Expr
  final case class Div(a: Expr, b: Expr) extends Expr
  final case class Mod(a: Expr, b: Expr) extends Expr
  final case class Eql(a: Expr, b: Expr) extends Expr
  final case class VarRef(variable: Variable) extends Expr
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
