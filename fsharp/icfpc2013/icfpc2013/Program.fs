module Main
open ApiProtocol
open ApiClient
open Generator
open BV
open System
open System.Globalization
open System.Diagnostics
open Microsoft.FSharp.Collections


let rnd = 
    let ran = new System.Random(int System.DateTime.Now.Ticks)
    ran
let printUint64 x =
    sprintf "0x%X" x
let parseUint64 (x: string) =
    UInt64.Parse(x.[2..], NumberStyles.HexNumber)
let genUint64 _ =
    let buffer = Array.create sizeof<UInt64> 0x00uy
    rnd.NextBytes(buffer)
    BitConverter.ToUInt64(buffer, 0)
    
let genArgs = 
    Array.init 256 genUint64

let printArgs args = 
    Array.map printUint64 args

let isSolution expr arg out = 
    let result = BV.eval expr arg
    result = out

let rec solve id (exprs: seq<Expr>) =
    match exprs with
    | e when Seq.isEmpty e -> failwith "no solution"
    | _ -> 
        let head = Seq.head exprs
        let tail = Seq.skip 1 exprs
        let program = BV.print head
        let guess = {Guess.id = id; program = program}    
        let resp = ApiClient.guess guess 
        if resp.status = "win" then 
            head
        else if resp.status = "mismatch" then
            let mismatch = Array.map parseUint64 resp.values.Value
            let rest = PSeq.filter (fun e -> isSolution e (mismatch.[0]) (mismatch.[1])) tail
            solve id rest
        else
            failwith resp.message.Value


let solveTask id size operators solver =
    printfn "->task %s %d %A" id size operators
    let sw = new Stopwatch()
    sw.Start()
    let candidats = solver operators |> Seq.toArray
    printfn "%d expr-s generated in %d" candidats.Length sw.ElapsedMilliseconds
    sw.Reset()
    let args = genArgs
    let printedArgs = printArgs args
    let evalResponse = ApiClient.eval {id = Some id; program = None; arguments = printedArgs}
    printfn "requested in %d" sw.ElapsedMilliseconds
    sw.Reset()
    let argToOutput = Seq.zip args (Array.map parseUint64 evalResponse.outputs.Value)
    let evalTest expr =
        Seq.forall (fun (arg, out) -> isSolution expr arg out) argToOutput
    let solutions = PSeq.filter evalTest candidats
    printfn "filtered in %d" sw.ElapsedMilliseconds
    let solution = solve id solutions
    printfn "solved in %d" sw.ElapsedMilliseconds
    sw.Stop()
    solution
            
let solveTraining size = 
    let solver = Generator.solve size
    let problem = ApiClient.train {TrainRequest.size = Some size; operators = None }
    let result = solveTask problem.id problem.size problem.operators solver
    (problem, result)

let solveReal problem solver = 
    solveTask problem.id problem.size problem.operators solver

let solveAllReal size = 
    let solver = Generator.solve size
    let problems = ApiClient.problems
    let ourProblems = Array.filter (fun x -> x.size = size && x.solved.IsNone || x.solved.IsSome && x.solved.Value = false && x.timeLeft.Value > 0) problems
    Array.map (fun x -> solveReal x solver) ourProblems

[<EntryPoint>]
let main argv = 
    let cmplx = if argv.Length > 0 then Int32.Parse(argv.[0]) else 10
    let count = if argv.Length > 1 then Int32.Parse(argv.[1]) else 5
    let sw = new Stopwatch()
    sw.Start()
    let trains = Seq.toArray (seq {
            for _ in 1..count do 
                yield solveTraining cmplx
        })
    sw.Stop()
    printfn "Total %d ms" sw.ElapsedMilliseconds
    //let results = solveAllReal 7
    Console.ReadKey() |> ignore
    0
//    let problems = ApiClient.problems
//    let smallProblems = Array.filter (fun x -> x.size = 3 && (x.solved.IsNone || x.solved.IsSome && x.solved.Value = false)) problems
//    let results = Array.map solveReal l
//    let startD = DateTime.Now
//    let a = Seq.toArray (seq { for x in 1..20 do
//                                let status = ApiClient.train {TrainRequest.size = Some 3; operators = None }
//                                yield status
//        })
//    printfn "%d" (int (DateTime.Now - startD).TotalSeconds)
