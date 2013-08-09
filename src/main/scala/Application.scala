import scala.concurrent.ExecutionContext

object Application {
  implicit val ec = ExecutionContext.Implicits.global

  def main(args: Array[String]) = {
    /*
    val problemsFuture = ApiClient.Problems(Some("problems.json"))
    val problems = Await.result(problemsFuture, 5.second)
    */
    ApiClient.Training(TrainRequest(size = Some(3))) andThen {
      case result if result.isSuccess => {
        println(result.get)
        ApiClient.Training(TrainRequest(size = Some(4)))
      }
    } onSuccess {
      case train => println(train)
    }
    ApiClient.cleanup()
  }

  def trainRequest = {
    ApiClient.Training(TrainRequest(size = Some(3)))
  }
}
