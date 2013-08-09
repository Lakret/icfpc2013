import spray.json.DefaultJsonProtocol

case class Problem(
  id: String,
  size: Int,
  operators: Array[String],
  solved: Option[Boolean],
  timeLeft: Option[Int])

case class EvalRequest(
  id: Option[String],
  program: Option[String] = None,
  arguments: Array[String])

case class EvalResponse(
  status: String,
  outputs: Option[Array[String]],
  message: Option[String])

case class Guess(
  id: String,
  program: String)

case class GuessResponse(
  status: String,
  values: Option[Array[String]],
  message: Option[String],
  lightning: Option[Boolean])

case class TrainRequest(
  size: Option[Int] = None,
  operators: Option[Array[String]] = None)

case class TrainingProblem(
  challenge: String,
  id: String,
  size: Int,
  operators: Array[String])

case class Window(
  resetsIn: Int,
  amount: Int,
  limit: Int)

case class Status(
  easyChairId: String,
  contestScore: Int,
  lightningScore: Int,
  trainingScore: Int,
  mismatches: Int,
  numRequests: Int,
  requestWindow: Window,
  cpuWindow: Window,
  cpuTotalTime: Int)

object ApiProtocol extends DefaultJsonProtocol {
  implicit val problemFormat = jsonFormat5(Problem)
  implicit val evalRequestFormat = jsonFormat3(EvalRequest)
  implicit val evalResponseFormat = jsonFormat3(EvalResponse)
  implicit val guessFormat = jsonFormat2(Guess)
  implicit val guessResponseFormat = jsonFormat4(GuessResponse)
  implicit val trainRequestFormat = jsonFormat2(TrainRequest)
  implicit val trainingProblemFormat = jsonFormat4(TrainingProblem)
  implicit val windowFormat = jsonFormat3(Window)
  implicit val statusFormat = jsonFormat9(Status)
}
