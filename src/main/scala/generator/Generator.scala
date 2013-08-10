package generator

import evaluator._

object Generator {

  def solve3(ops: Array[String]) = {
    val unaryOp = BVParser.parseUnary(ops.head)
    val id = IdGenerator.next()
    for (x <- gen2(unaryOp, id))
      yield Lambda(List(id), x)
  }

  def gen3(ops: Array[String]) = {
    val opFunc = BVParser.parseUnary(ops.head)
    val id = IdGenerator.next()

  }

  def gen2(unaryOp: Expr => UnaryOp, x: Id) = {
    for (expr <- gen1(x))
      yield unaryOp(expr)
  }

  def gen1(x: Id) = {
    List(x, One(), Zero())
  }
}

object IdGenerator {
  var index = 0
  def next() = {
    val id = Id(name = s"x_${index}")
    index = index + 1
    id
  }
}

  /*
case Not(x) => app("not", x)
case Shl1(x) => app("shl1", x)
case Shr1(x) => app("shr1", x)
case Shr4(x) => app("shr4", x)
case Shr16(x) => app("shr16", x)
*/
object  BVParser {
  def parseUnary(arg: String): Expr => UnaryOp = arg match {
    case "not"  => x => Not(x)
    case "shl1" => x => Shl1(x)
    case "shr1" => x => Shr1(x)
    case "shr4" => x => Shr4(x)
    case "shr16" => x => Shr16(x)
    case _ => throw new Exception(s"illegal unary op: ${arg}")
  }
}
