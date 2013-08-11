module Main
open ApiProtocol
open ApiClient
open Generator
open BV
open System
open System.Globalization
open System.Threading

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
    BV.eval expr arg = out

let rec solve id (exprs: array<Expr>) =
    match exprs with
    | [| |] -> failwith "no solution"
    | _ -> 
        let program = BV.print exprs.[0] 
        let guess = {Guess.id = id; program = program}    
        let resp = ApiClient.guess guess 
        if resp.status = "win" then 
            exprs.[0]
        else if resp.status = "mismatch" then
            let mismatch = Array.map parseUint64 resp.values.Value
            let rest = Array.filter (fun e -> isSolution e (mismatch.[0]) (mismatch.[1])) exprs.[1..]
            solve id rest
        else
            failwith resp.message.Value


let solveTask id size operators =
    let candidats = Generator.solve3 operators
    let args = genArgs
    let printedArgs = printArgs args
    let evalResponse = ApiClient.eval {id = Some id; program = None; arguments = printedArgs}
    let argToOutput = Seq.zip args (Array.map parseUint64 evalResponse.outputs.Value)
    let evalTest expr =
        Seq.forall (fun (arg, out) -> isSolution expr arg out) argToOutput
    let solutions = Seq.filter evalTest candidats
    solve id (Seq.toArray solutions)
            
let solveTraining = 
    let problem = ApiClient.train {TrainRequest.size = Some 3; operators = None }
    solveTask problem.id problem.size problem.operators

let solveReal problem = 
    solveTask problem.id problem.size problem.operators

[<EntryPoint>]
let main argv = 
//    let problems = ApiClient.problems
//    let smallProblems = Array.filter (fun x -> x.size = 3 && (x.solved.IsNone || x.solved.IsSome && x.solved.Value = false)) problems
//    let results = Array.map solveReal l
    let startD = DateTime.Now
    let a = Seq.toArray (seq { for x in 1..20 do
                                let status = ApiClient.train {TrainRequest.size = Some 3; operators = None }
                                yield status
        })
    printfn "%d" (int (DateTime.Now - startD).TotalSeconds)
    0 // return an integer exit code
