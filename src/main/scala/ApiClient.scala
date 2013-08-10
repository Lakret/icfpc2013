import com.typesafe.config.ConfigFactory
import dispatch._, Defaults._
import java.io.PrintWriter
import org.json4s.jackson.Serialization
import scala.concurrent.Future
import org.json4s._
import org.json4s.jackson.JsonMethods._


object ApiClient {

  val config = ConfigFactory.load()
  val authToken = config.getString("auth.token")
  val hostname = config.getString("host")
  implicit val formats = DefaultFormats


  private def buildRequest(path: String, data: Option[AnyRef]) = {
    val req = (host(hostname) / path <<? Map("auth" -> authToken)).POST
    if (data.isDefined) req << Serialization.write(data.get) else req
  }

  def makeRequest[TResp : Manifest](path: String, data: Option[AnyRef] = None): Future[Option[TResp]] = {
    val request = buildRequest(path, data)
    val future = Http(request OK as.String).option
    future.map { resp =>
      if (resp.isDefined)
        Some(parse(resp.get).extract[TResp])
      else
        None
    }
  }

  def Training(request: TrainRequest) = {
    makeRequest[TrainingProblem]("train", Some(request))
  }

  def Status() = {
    makeRequest[TrainingProblem]("status")
  }

  def Eval(request: EvalRequest) = {
    makeRequest[Status]("status", Some(request))
  }

  def Guess(request: Guess) = {
    makeRequest[GuessResponse]("guess", Some(request))
  }

  def Problems(writeToFile: Option[String] = None) = {
    val future = makeRequest[List[Problem]]("myproblems")
    future map {
      case Some(problems) => {
        writeToFile.map { x => Some(new PrintWriter(x)).map {
            writer => {
              val str = Serialization.write(problems)
              writer.write(str)
              writer.close()
            }
          }
        }
      }
      case None => ()
    }
    future
  }

}
