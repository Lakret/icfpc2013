import akka.io.IO
import akka.pattern.ask
import akka.actor.ActorSystem
import akka.event.Logging
import com.typesafe.config.ConfigFactory
import scala.concurrent.duration._
import scala.util.Success
import spray.can.Http
import spray.client.pipelining._
import spray.http._
import spray.http.Uri.Query
import spray.httpx.SprayJsonSupport
import spray.util._

object Application {
  def main(arg : Array[String]) {
    println("do nothing")
  }
  // val config = ConfigFactory.load()
  // val authToken = config.getString("auth.token")
  // val host = config.getString("host")

  // implicit val system = ActorSystem("kontur-ukko-icfpc2013")
  // val log = Logging(system, getClass)
  // import system.dispatcher
  // import SprayJsonSupport._
  // val pipeline = sendReceive ~> unmarshal[TrainingProblem]


  // def buildUrl(path: String) = {
  //   Uri.from(scheme = "http", host = host, path = "/" + path, query = Query("auth" -> authToken))
  // }
  // def main(arg: Array[String]) {
  //   val trainingUrl = buildUrl("train")
  //   val request = TrainRequest(size = Some(3), operators = None)
  //   val response = pipeline(Post(trainingUrl, request))
  //   response.onComplete {
  //     case Success(r) =>
  //       log.info("response '{}'", r)
  //       shutdown()
  //   }
  // }

  // def shutdown(): Unit = {
  //   IO(Http).ask(Http.CloseAll)(1.second).await
  //   system.shutdown()
  // }
}
