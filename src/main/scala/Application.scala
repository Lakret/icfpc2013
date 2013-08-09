import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.util.Try

object Application {
  implicit val ec = ApiClient.system.dispatcher

  def main(args: Array[String]) = {
    /*
    val problemsFuture = ApiClient.Problems(Some("problems.json"))
    val problems = Await.result(problemsFuture, 5.second)
    */
    ApiClient.Training(TrainRequest(size = Some(3))) onSuccess   {
      case train => {
        println(train)
      }
    }
  }
}
