module Generator
open BV

let mutable id = 0
let nextId =
    let value = sprintf "x_%d" id 
    id <- id + 1
    value


let parseUnary op =
    match op with
    | "not" -> Not
    | "shl1" -> Shl1
    | "shr1" -> Shr1
    | "shr4" -> Shr4
    | "shr16" -> Shr16
    | _ -> failwith ("unknown operator: " + op)


let gen1 (args) =
    seq {
        yield! args
        yield One
        yield Zero
        }

let gen2 (unaryOps)(args) = 
    seq {
        for op in unaryOps do
            for s1 in gen1(args) do
                yield op s1
    } 
    
let solve3 ops =
    let arg = nextId
    let unaryOps = Seq.map parseUnary ops
    seq {
        for expr in gen2 unaryOps [|Id(arg)|] do
            yield Lambda([|arg|], expr)
    }  
    
open NUnit.Framework
open FsUnit
[<TestFixtureAttribute>]
type ``Solve simple test``() =
    [<TestAttribute>]
    member x.Solve3Test() =
        let solution = Seq.toArray (solve3 [|"not"|]) 
        solution.Length |> should equal 3    

