import evaluator._

import org.scalatest._

class EvaluatorSpec extends FlatSpec {
  val foldTest =
    Lambda(
      List(Id("x")),
      Fold(Id("x"), Zero(),
        Lambda(List(Id("y"), Id("z")), Or(Id("y"), Id("z")))))

  "fold" should "work" in {
    val input = BigInt("1122334455667788", 16)
    assert(evaluator.eval(foldTest)(evaluator.emptyBindings)(input) === BigInt(255))
  }

  "print" should "work" in {
    assert(evaluator.print(foldTest) === "(lambda (x) (fold x 0 (lambda (y z) (or y z))))")
  }

  "size" should "work" in {
    assert(evaluator.size(foldTest) === 8)
  }
}
