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

