import akka.actor.ActorSystem
import akka.event.Logging
import akka.io.IO
import akka.pattern.ask
import com.typesafe.config.ConfigFactory
import java.io.PrintWriter
import scala.concurrent.duration.DurationLong
import scala.concurrent.Future
import spray.can.Http
import spray.client.pipelining._
import spray.http.{HttpResponse, Uri}
import spray.http.Uri.Query
import spray.httpx.SprayJsonSupport
import spray.util.pimpFuture


object ApiClient {

  val config = ConfigFactory.load()
  val authToken = config.getString("auth.token")
  val host = config.getString("host")

  implicit val system = ActorSystem("kontur-ukko-icfpc2013")
  val log = Logging(system, getClass)

  import system.dispatcher
  import SprayJsonSupport._
  import ApiProtocol._


  private def buildUrl(path: String) = {
    Uri.from(scheme = "http", host = host, path = "/" + path, query = Query("auth" -> authToken))
  }

  def response[T](future: Future[T]) = {
    future onComplete { case _ => shutdown() }
    future
  }

  def Training(request: TrainRequest): Future[TrainingProblem] = {
    val pipeline = sendReceive ~> unmarshal[TrainingProblem]
    val url = buildUrl("train")
    val future = pipeline(Post(url, request))
    response(future)
  }

  def Status() = {
    val pipeline = sendReceive ~> unmarshal[Status]
    val url = buildUrl("status")
    val future = pipeline(Post(url))
    response(future)
  }

  def Eval(request: EvalRequest) = {
    val pipeline = sendReceive ~> unmarshal[EvalResponse]
    val url = buildUrl("eval")
    val future = pipeline(Post(url, request))
    response(future)
  }

  def Guess(request: Guess) = {
    val pipeline = sendReceive ~> unmarshal[GuessResponse]
    val url = buildUrl("guess")
    val future = pipeline(Post(url, request))
    response(future)
  }

  def Problems(writeToFile: Option[String] = None) = {
    val pipeline = sendReceive
    val url = buildUrl("myproblems")
    val f: Future[HttpResponse] = pipeline(Post(url))
    val future = f.map {
      case response => {
        writeToFile.map { x => Some(new PrintWriter(x)).map {
            writer => writer.write(response.entity.asString); writer.close()
          }
        }
        val unmarshaler = unmarshal[Array[Problem]]
        unmarshaler(response)
      }
    }
    response(future)
  }

  private def shutdown(): Unit = {
    IO(Http).ask(Http.CloseAll)(1.second).await
    system.shutdown()
  }

}
