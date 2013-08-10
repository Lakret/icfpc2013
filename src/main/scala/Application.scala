import generator.Generator
import scala.concurrent._
import evaluator._
import scala.util.Random

object Application {
  implicit val ec = ExecutionContext.Implicits.global
  def inputs(): Array[String] = {
    Array.fill(2){
      "0x" + BigInt(64, Random).toString(16).toUpperCase
    }
  }

  def main(args: Array[String])  {
    /*
    val problemsFuture = ApiClient.Problems(Some("problems.json"))
    val problems = Await.result(problemsFuture, 5.second)
    */
    val f = ApiClient.Training(TrainRequest(Some(3)))
    for {
      first <- f
    } yield {
      val train = first.get
      val solutions = Generator.solve3(train.operators)
      val args = inputs()
      val eval = EvalRequest(arguments = args, id = Some(train.id))
      for { evalResponseOpt <- ApiClient.Eval(eval) }
        yield {
          val evalResp = evalResponseOpt.get
          val answer = evalResp.outputs.get
          val accepted = solutions.filter {
            lambda => {
              val res = args.zipWithIndex.forall{ case (x, i) =>  testLambda(lambda, x, answer(i)) }
              res
            }
           }
          val d = accepted
          sys.exit()
        }

      }
  }

  def testLambda(lambda: Lambda, arg: String, expected: String) = {
    val result = evaluator.eval(lambda)(Map.empty[String, BigInt])(BigInt(arg.substring(2), 16))
    val parsed = BigInt(expected.substring(2), 16)
    result == parsed
  }

  def solve(variants: List[Expr]) = {

  }


  def TimeoutWrapper[T](arg: Future[T]) = {
    val result = arg.map(Some[T])
    val timeout: Future[Option[T]] = future {
      Thread.sleep(5000)
      println("timeout exceeded")
      None
    }
    Future.sequence(Seq(result, timeout)) map {
      x => x.flatten.head
    }
  }
}
