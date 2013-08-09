import akka.io.IO
import akka.pattern.ask
import akka.actor.ActorSystem
import akka.event.Logging
import scala.concurrent.duration._
import scala.util.Success
import spray.can.Http
import spray.client.pipelining._
import spray.http._
import spray.http.Uri.Query
import spray.httpx.SprayJsonSupport
import spray.json.{pimpAny, DefaultJsonProtocol}
import spray.util._

case class TrainRequest(size: Option[Int], operators: Option[List[String]])
case class TrainingProblem(challenge: String, id: String, size: Int, operators: List[String])

object ApiProtocol extends DefaultJsonProtocol {
  implicit val trainingFormat = jsonFormat2(TrainRequest)
  implicit val triningProblem = jsonFormat4(TrainingProblem)
}

object Application {
  implicit val system = ActorSystem("kontur-ukko-icfpc2013")
  val log = Logging(system, getClass)
  import system.dispatcher
  import ApiProtocol._
  import SprayJsonSupport._
  val pipeline = sendReceive ~> unmarshal[TrainingProblem]

  val host = "icfpc2013.cloudapp.net"
  val suffix = "vpsH1H"
  def authToken = "0201LxVGJ0tIY2MX5ce2dKYAU4b2NXYSXpOmNoQv"
  def buildUrl(path: String) = {
    Uri.from(scheme = "http", host = host, path = "/" + path, query = Query("auth" -> (authToken + suffix)))
  }
  def main(arg: Array[String]) {
    val trainingUrl = buildUrl("train")
    val request = TrainRequest(size = Some(3), operators = None)
    val response = pipeline(Post(trainingUrl, request))
    response.onComplete {
      case Success(r) =>
        log.info("response '{}'", r)
        shutdown()
    }
  }

  def shutdown(): Unit = {
    IO(Http).ask(Http.CloseAll)(1.second).await
    system.shutdown()
  }
}
