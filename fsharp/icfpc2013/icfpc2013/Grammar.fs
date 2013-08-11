module BV

type Expr =
| Zero
| One
| Id of string
| Lambda of array<string> * Expr
| Shl1 of Expr
| Shr1 of Expr
| Shr4 of Expr
| Shr16 of Expr
| Not of Expr
| And of Expr * Expr

let rec size exp = 
    match exp with
    | Lambda (_, x) -> 1 + size x
    | Zero -> 1
    | One -> 1
    | Id _ -> 1
    | Not x -> 1 + size x
    | Shl1 x -> 1 + size x
    | Shr1 x -> 1 + size x
    | Shr4 x -> 1 + size x
    | Shr16 x -> 1 + size x
    | And (x, y) -> 1 + size x + size y

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

open NUnit.Framework
open FsUnit
[<TestFixtureAttribute>]
type ``simple eval test``() =
    let exp = Lambda([|"x_1"|], Not(Id("x_1")))
    [<TestAttribute>]
    member x.EvalLambdaTest() =
        let result = eval exp 0xffffffffffffffffUL
        result |> should equal 0UL
    member x.PrintTest() =
        let result = print exp
        result |> should equal "(lambda (x_1) (not x_1))"
