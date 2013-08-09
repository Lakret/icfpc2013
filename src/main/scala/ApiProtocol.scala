import spray.json.DefaultJsonProtocol

case class Problem(
  id: String,
  size: Int,
  operators: List[String],
  solved: Option[Boolean],
  timeLeft: Option[Int]) //time left in seconds

case class EvalRequest(
  id: Option[String],
  program: Option[String],
  arguments: List[String])

case class EvalResponse(
  status: String,
  outputs: Option[List[String]],
  message: Option[String])

case class TrainRequest(
  size: Option[Int],
  operators: Option[List[String]])

case class TrainingProblem(
  challenge: String,
  id: String,
  size: Int,
  operators: List[String])

object ApiProtocol extends DefaultJsonProtocol {
  implicit val problemFormat = jsonFormat5(Problem)
  implicit val evalRequest = jsonFormat3(EvalRequest)
  implicit val evalResponse = jsonFormat3(EvalResponse)
  implicit val trainingFormat = jsonFormat2(TrainRequest)
  implicit val trainingProblem = jsonFormat4(TrainingProblem)
}
