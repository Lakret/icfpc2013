module Generator
open BV

let mutable id = 0
let nextId _ =
    let value = sprintf "x_%d" id 
    id <- id + 1
    value

let isUnary op =
    match op with
    | "not" | "shl1" | "shr1" | "shr4" | "shr16" -> true
    | _ -> false

let isBinary op =
    match op with
    | "and" | "or" | "xor" | "plus" -> true
    | _ -> false

let isIf0 op = op = "if0"
let isFold op = op = "fold"

let parseUnary op =
    match op with
    | "not" -> Not
    | "shl1" -> Shl1
    | "shr1" -> Shr1
    | "shr4" -> Shr4
    | "shr16" -> Shr16
    | _ -> failwith ("unknown operator: " + op)

let parseBinary op = 
    match op with
    | "and" -> And
    | "or" -> Or
    | "xor" -> Xor
    | "plus" -> Plus
    | _ -> failwith ("unknown operator: " + op)

let parseIf0Op op = If0
let parseFold op = Fold
   
let rec gen size (unaryOps, binaryOps, if0Ops, foldOps, args) =
    let allArgs = (unaryOps, binaryOps, if0Ops, foldOps, args)
    let gen1 = fun _ -> gen 1 allArgs
    let genUnary _ =
        let size' = size - 1
        seq {
            for uOp in unaryOps do
                for x in gen size' allArgs do
                yield uOp x
            }
    let genBinary _ =
        let size' = size - 1
        seq {
            for op in binaryOps do
                for i in 1..(size'/2) do
                    for x in gen i allArgs do
                        for y in gen (size' - i) allArgs do
                            yield op (x, y)
            } 
    let genIf0 _ = 
        let size' = size - 1
        seq {
            for op in if0Ops do
                for i in 1..(size' - 2) do
                    for j in 1..(size' - 2) do
                        let k = size' - i - j
                        if k > 0 then
                          for cond in gen i allArgs do
                            for thenB in gen j allArgs do
                                for elseB in gen k allArgs do
                                    yield op (cond, thenB, elseB)                            
            }
    let genFold _ = 
        let size' = size - 2
        let x = nextId ()
        let y = nextId ()
        let innerA = [|Id(x); Id(y)|]
        let innerArgs = (unaryOps, binaryOps, if0Ops, foldOps, innerA)
        seq {
            for op in foldOps do
                for i in 1..(size' - 2) do
                    for j in 1..(size' - 2) do
                        let k = size' - i - j
                        if k > 0 then
                          for e0 in gen i allArgs do
                            for e1 in gen j allArgs do
                                for e2 in gen k innerArgs do
                                    yield op (e0, e1, Lambda([|x; y|], e2))                            
            }
    match size with
    | 1 -> 
        seq {
            yield! args
            yield One
            yield Zero
        }
    | 2 -> seq {
            yield! genUnary size
         } 
    | 3 -> seq {
        yield! genUnary ()
        yield! genBinary ()
        } 
    | 4 | 5 -> seq {
        yield! genIf0 ()
        yield! genBinary ()
        yield! genUnary ()
        }
    | _ -> seq {
        yield! genFold ()
        yield! genIf0 ()
        yield! genBinary ()
        yield! genUnary ()
        }

let genTFold size (unaryOps, binaryOps, if0Ops, foldOps, args: array<Expr>) =
        let allArgs = (unaryOps, binaryOps, if0Ops, foldOps, args)
        let size' = size - 2
        let x = nextId ()
        let y = nextId ()
        let innerA = [|Id(x); Id(y)|]
        let innerArgs = (unaryOps, binaryOps, if0Ops, foldOps, innerA)
        seq {
            for e2 in gen (size' - 2) innerArgs do
                yield Fold(args.[0], Zero, Lambda([|x; y|], e2))
            }

let solve size ops = 
    let arg = nextId ()
    let unaryOps = Array.filter isUnary ops |> Array.map parseUnary
    let binaryOps = Array.filter isBinary ops |> Array.map parseBinary
    let if0Ops = Array.filter isIf0 ops |> Array.map parseIf0Op
    let foldOps = Array.filter isFold ops |> Array.map parseFold
    let s = if (Seq.exists (fun x -> x = "tfold") ops) then
                genTFold (size - 1) (unaryOps, binaryOps, if0Ops, foldOps, [|Id(arg)|])
            else
                gen (size - 1) (unaryOps, binaryOps, if0Ops, foldOps, [|Id(arg)|]) 
    seq {
        for expr in s do
            yield Lambda([|arg|], expr)
    } 
    
//let solve3 ops =
//    let arg = nextId
//    let unaryOps = Array.filter isUnary ops |> Array.map parseUnary
//    seq {
//        for expr in gen2 unaryOps [|Id(arg)|] do
//            yield Lambda([|arg|], expr)
//    }  
//
//let solve4 ops =
//    let arg = nextId
//    let unaryOps = Array.filter isUnary ops |> Array.map parseUnary
//    let binaryOps = Array.filter isBinary ops |> Array.map parseBinary
//    seq {
//        for expr in gen3 unaryOps binaryOps [|Id(arg)|] do
//            yield Lambda([|arg|], expr)
//    }
//
//let solve5 ops =
//    let arg = nextId
//    let unaryOps = Array.filter isUnary ops |> Array.map parseUnary
//    let binaryOps = Array.filter isBinary ops |> Array.map parseBinary
//    let if0Ops = Array.filter isIf0 ops |> Array.map parseIf0Op
//    seq {
//        for expr in gen4 unaryOps binaryOps if0Ops [|Id(arg)|] do
//            yield Lambda([|arg|], expr)
//    }
    
open NUnit.Framework
open FsUnit
[<TestFixtureAttribute>]
type ``Solve simple test``() =
    [<TestAttribute>]
    member x.Solve3Test() =
        let solution = Seq.toArray (solve 5 [|"plus"; "shr1"|]) 
        solution.Length |> should equal 57    
    [<TestAttribute>]
    member x.Solve6Test() =
        let solution = Seq.toArray (solve 6 [|"plus"; "and"|]) 
        solution.Length |> should equal 3

