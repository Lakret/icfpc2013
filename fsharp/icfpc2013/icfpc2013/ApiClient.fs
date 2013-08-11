module ApiClient
open ApiProtocol
open JsonSerializer
open System
open System.IO
open System.Net
open System.Text
open System.Threading

let baseUrl = "http://icfpc2013.cloudapp.net"
let authToken = "0201LxVGJ0tIY2MX5ce2dKYAU4b2NXYSXpOmNoQvvpsH1H"
let timeout = 1000

let rec request<'b, 'a> (path: string)(data: Option<'a>) =  
    let body = if data.IsSome then JsonSerializer.toJson data.Value else ""
    let bodyBytes = Encoding.Default.GetBytes(body)
    let builder = new UriBuilder(baseUrl)
    builder.Path <- path
    builder.Query <- "auth=" + authToken
    let req = HttpWebRequest.Create(builder.Uri) :?> HttpWebRequest
    req.Method <- "POST"
    req.ContentType <- "application/json"
    req.GetRequestStream().Write(bodyBytes, 0, bodyBytes.Length)
    req.GetRequestStream().Close()
    try
        let resp = req.GetResponse() :?> HttpWebResponse
        if resp.StatusCode = HttpStatusCode.OK then
            let respBody = (new StreamReader(resp.GetResponseStream())).ReadToEnd()
            JsonSerializer.fromJson<'b> respBody
        else
            failwith ("wrong status code: " + resp.StatusCode.ToString())
    with
        | :? WebException as e when e.Status = WebExceptionStatus.ProtocolError -> 
                Thread.Sleep(timeout)
                request path data

let train data: TrainingProblem = 
    request "train" (Some data)

let status: Status = 
    request "status" None

let eval data: EvalResponse = 
    request "eval" (Some data)

let guess data: GuessResponse =
    request "guess" (Some data)

let problems: array<Problem> = 
    request "myproblems" None

let cachedProblems =
    let json = File.ReadAllText("problems.json")
    JsonSerializer.fromJson<array<Problem>> json

open NUnit.Framework
open FsUnit
[<TestFixtureAttribute>]
type ``api client tests``() = 
    [<TestAttribute>]
    member x.TrainTest() = 
        let req = { TrainRequest.size = Some 3; operators = None }
        let result = train req
        result.size |> should equal req.size.Value
    [<TestAttribute>]
    member x.StatusTest() =
        let result = status
        result.contestScore |> should equal 0
    [<TestAttribute>]
    member x.ProblemsTest() =
        let problems = problems
        let solved = Array.filter (fun x -> x.solved.IsSome) problems
        let json = JsonSerializer.toJson problems
        File.WriteAllText("problems.json", json)
        problems.Length |> should equal 1820
    [<TestAttribute>]
    member x.CachedProblems() =
        let problems = cachedProblems
        problems.Length |> should equal 1820
                 

