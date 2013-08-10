import evaluator._
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class BVSpec extends FunSuite with ShouldMatchers {
  test("Expr.size") {
    If0(Id("x_0"), One(), Zero()).size should be(4)
    Plus(One(), One()).size should be(3)
    Not(If0(One(), One(), One())).size should be(5)
  }
}
