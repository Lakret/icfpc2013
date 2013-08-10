import generator._
import evaluator._
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class GeneratorTest extends FunSuite with ShouldMatchers {
  test("gen3") {
    Generator.solve3(Array("shr1")).map( x => evaluator.print(x)) should be(List("(lambda (x_0) (not x_0))", "(lambda (x_0) (not 1))", "(lambda (x_0) (not 0))"))
  }
}
