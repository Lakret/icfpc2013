import scala.concurrent.ExecutionContext

object Application {
  implicit val ec = ExecutionContext.Implicits.global

  def main(args: Array[String])  {
    /*
    val problemsFuture = ApiClient.Problems(Some("problems.json"))
    val problems = Await.result(problemsFuture, 5.second)
    */
    for {
      first <- ApiClient.Training(TrainRequest(Some(4)))
      second <- ApiClient.Training(TrainRequest(Some(5)))
    } yield {
      println(first)
      println(second)
      sys.exit()
    }

  }
}
