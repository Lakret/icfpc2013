module BV

open System

let malformedInput x = failwith <| sprintf "malformed input %A" x

type Expr =
    | Zero
    | One
    | Id of string
    | Lambda of (string array) * Expr
    | Shl1 of Expr
    | Shr1 of Expr
    | Shr4 of Expr
    | Shr16 of Expr
    | Not of Expr
    | And of Expr * Expr
    | Or of Expr * Expr
    | Xor of Expr * Expr
    | Plus of Expr * Expr
    | Fold of Expr * Expr * Expr //arg, acc, lambda
    | If0 of Expr * Expr * Expr //cond, then, else

let (|UnaryOp|BinaryOp|FoldOp|If0Op|LambdaOp|IdOp|NumOp|) x =
    match x with
    | Zero | One -> NumOp(x)
    | Id x -> IdOp(x)
    | Lambda(args, body) -> LambdaOp(args, body)
    | Shl1 x | Shr1 x | Shr4 x | Shr16 x | Not x ->
        UnaryOp x
    | And(x, y) | Or(x, y) | Xor(x, y) | Plus(x, y) ->
        BinaryOp(x, y)
    | Fold(arg, acc, lambda) -> FoldOp(arg, acc, lambda)
    | If0(cond, thenB, elseB) -> If0Op(cond, thenB, elseB)

let rec size exp = 
    match exp with
    | LambdaOp (_, x) -> 1 + size x
    | NumOp _ -> 1
    | IdOp _ -> 1
    | UnaryOp x -> 1 + size x
    | BinaryOp(x, y) -> 1 + size x + size y
    | FoldOp(arg, acc, Lambda(_, body)) -> 2 + (List.sumBy (fun x -> size x) [arg; acc; body])
    | If0Op(cond, thenB, elseB) -> 1 + (List.sumBy (fun x -> size x) [cond; thenB; elseB])
    | x -> malformedInput x

let rec evaluate (env: Map<string, uint64>) exp = 
    match exp with
    | Lambda (_, x) -> evaluate env x
    | Zero -> 0UL
    | One -> 1UL
    | Id x -> env.[x]
    | Not x -> ~~~(evaluate env x)
    | Shl1 x -> (evaluate env x) <<< 1
    | Shr1 x -> (evaluate env x) >>> 1
    | Shr4 x -> (evaluate env x) >>> 4
    | Shr16 x -> (evaluate env x) >>> 16
    | And (x, y) -> (evaluate env x) &&& (evaluate env y)
    | Or (x, y) -> (evaluate env x) ||| (evaluate env y)
    | Xor (x, y) -> (evaluate env x) ^^^ (evaluate env y)
    | Plus (x, y) -> (evaluate env x) + (evaluate env y)
    | If0(cond, thenB, elseB) -> 
        if (evaluate env cond) = 0UL then
            evaluate env thenB
        else
            evaluate env elseB
    | Fold(init, currAcc, Lambda([|x; acc|], body)) ->
        let init'  = evaluate env init
        let initHex = sprintf "%X" <| init'
        let initHex' = initHex.PadLeft(16, '0')
        initHex'.ToCharArray()
        |> Array.fold 
            (fun ((curr : string), res) elem -> 
                if curr.Length = 2 then 
                    (elem.ToString(), res @ [curr])
                else 
                    (curr + elem.ToString(), res))
            ("", [])
        |> (fun (last, all) -> all @ [last])
        |> List.rev
        |> List.fold 
            (fun accv elem ->
                let v = Convert.ToUInt64(elem, 16)
                let newEnv = env |> ((Map.remove x) >> (Map.remove acc) >> (Map.add x v) >> (Map.add acc accv))
                evaluate newEnv body)
            (evaluate env currAcc)
    | x -> malformedInput x

let eval (lambda: Expr)(arg: uint64) =
     match lambda with
     | Lambda([|x|], exp) ->
        let env = Map.empty.Add(x, arg) 
        evaluate env exp
     | _ -> failwith "could only eval lambdas"

let rec print expr =
    match expr with
    | Lambda(args, x) -> 
        let args' = String.concat " " args
        let inner = print x
        sprintf "(lambda (%s) %s)" args' inner
    | Zero -> "0"
    | One -> "1"
    | Id(name) -> name
    | Not x -> sprintf "(not %s)" (print x)
    | Shl1 x -> sprintf "(shl1 %s)" (print x)
    | Shr1 x -> sprintf "(shr1 %s)" (print x)
    | Shr4 x -> sprintf "(shr4 %s)" (print x)
    | Shr16 x -> sprintf "(shr16 %s)" (print x)
    | And (x, y) -> sprintf "(and %s %s)" (print x) (print y)
    | Or (x, y) -> sprintf "(or %s %s)" (print x) (print y)
    | Xor (x, y) -> sprintf "(xor %s %s)" (print x) (print y)
    | Plus (x, y) -> sprintf "(plus %s %s)" (print x) (print y)
    | Fold(init, currAcc, step) ->
        sprintf "(fold %s %s %s)" (print init) (print currAcc) (print step)
    | If0(cond, thenB, elseB) -> 
        sprintf "(if0 %s %s %s)" (print cond) (print thenB) (print elseB)

open NUnit.Framework
open FsUnit
[<TestFixtureAttribute>]
type ``simple eval test``() =
    let exp = Lambda([|"x_1"|], Not(Id("x_1")))
    
    [<TestAttribute>]
    member x.EvalLambdaTest() =
        let result = eval exp 0xffffffffffffffffUL
        result |> should equal 0UL

    [<TestAttribute>]
    member x.PrintTest() =
        let result = print exp
        result |> should equal "(lambda (x_1) (not x_1))"

    [<TestAttribute>]
    member x.EvalFoldTest() =
        let expr =
            Lambda([|"x"|], 
                Fold(Id("x"), Zero,
                    Lambda([|"y"; "z"|], Or(Id("y"), Id("z")))))
        let input = 0x1122334455667788UL
        eval expr input |> should equal 255
    [<TestAttribute>]
    member x.EvalFoldExtraTest() =
        let expr =
            Lambda([|"x"|], 
                Fold(Id("x"), Zero,
                    Lambda([|"x"; "y"|], Plus(Id("x"), Id("x")))))
        let input = 16027026018299601879UL
        eval expr input |> should equal 444
    [<TestAttribute>]
    member x.EvalFoldExtraTest1() =
        let expr =
            Lambda([|"x"|], 
                Fold(Id("x"), Zero,
                    Lambda([|"x"; "y"|], Or(Id("x"), Id("y")))))
        let input = 788468176388864416UL
        eval expr input |> should equal 251
