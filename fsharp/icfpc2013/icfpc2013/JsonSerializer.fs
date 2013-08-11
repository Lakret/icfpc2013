module JsonSerializer

open System
open Microsoft.FSharp.Reflection
open Newtonsoft.Json
open Newtonsoft.Json.Converters

type OptionConverter() =
    inherit JsonConverter()
    
    override x.CanConvert(t) = 
        t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>>

    override x.WriteJson(writer, value, serializer) =
        let value = 
            if value = null then null
            else 
                let _,fields = FSharpValue.GetUnionFields(value, value.GetType())
                fields.[0]  
        serializer.Serialize(writer, value)

    override x.ReadJson(reader, t, existingValue, serializer) =        
        let innerType = t.GetGenericArguments().[0]
        let innerType = 
            if innerType.IsValueType then (typedefof<Nullable<_>>).MakeGenericType([|innerType|])
            else innerType        
        let value = serializer.Deserialize(reader, innerType)
        let cases = FSharpType.GetUnionCases(t)
        if value = null then FSharpValue.MakeUnion(cases.[0], [||])
        else FSharpValue.MakeUnion(cases.[1], [|value|])


let toJson value =
    JsonConvert.SerializeObject(value, new OptionConverter())

let fromJson<'a> json =
   JsonConvert.DeserializeObject<'a> (json, new OptionConverter())


open NUnit.Framework
open FsUnit
open ApiProtocol

[<TestFixture>]
type ``serilization test``() =
    [<Test>]
    member x.``test simple equal``() =
        let value = { TrainRequest.size = Some 10; operators = Some [|"not"|] }
        let json = toJson value
        json |> should equal """{"size":10,"operators":["not"]}"""
        let deser = fromJson<TrainRequest> json
        deser.size |> should equal value.size
        deser.operators |> should equal value.operators