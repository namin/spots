#light

// For F# Version 1.9.4.19 (not CTP).

// ----------------------------
// Listing 9-11.

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Typed
open Microsoft.FSharp.Quotations.Raw

type Error = Err of float

let (|BinaryOp|_|) t =
    match t with
    | GenericTopDefnApp <@@ ( + ) @@> (tyargs,[xt;yt]) -> Some ("+", xt, yt)
    | GenericTopDefnApp <@@ ( - ) @@> (tyargs,[xt;yt]) -> Some ("-", xt, yt)
    | GenericTopDefnApp <@@ ( * ) @@> (tyargs,[xt;yt]) -> Some ("*", xt, yt)
    | GenericTopDefnApp <@@ ( / ) @@> (tyargs,[xt;yt]) -> Some ("/", xt, yt)
    | _ -> None

let (|UnaryOp|_|) t =
    match t with
    | GenericTopDefnApp <@@ abs @@> (tyargs,[xt]) -> Some ("abs", xt)
    | _ -> None

let (|TopDefApp|_|) t =
    match t with
    | App(ResolvedTopDefnUse(info,Lambda(v,body)),arg) -> Some (v,body,arg)
    | _ -> None

let (|TopDef|_|) t =
    match t with
    | ResolvedTopDefnUse(info,body) -> Some (body)
    | _ -> None
                    
let rec errorEstimateAux t (env : Map<_,_>) =

    match t with
    | BinaryOp (op, xt, yt) ->
        let x,Err(xerr) = errorEstimateAux xt env
        let y,Err(yerr) = errorEstimateAux yt env
        match op with
        | "+" -> (x+y,Err(xerr+yerr))
        | "-" -> (x-y,Err(xerr+yerr))
        | "*" -> (x*y,Err(abs(x)*yerr+abs(y)*xerr+xerr*yerr))
        | "/" -> let xa,ya = abs(x), abs(y)
                 (x/y,Err((xa*yerr+ya*xerr)/(ya*(ya-yerr))))
        | _ -> failwith "unrecognized binary operator: %s" op
        
    | UnaryOp (op, xt) ->
        let x,Err(xerr) = errorEstimateAux xt env
        match op with
        | "abs" -> (abs(x),Err(xerr))
        | _ -> failwith "unrecognized unary operator: %s" op
        
    | Let((var,vet), bodyt) ->
        let varv,verr = errorEstimateAux vet env
        errorEstimateAux bodyt (env.Add(var.Name,(varv,verr)))

    | TopDefApp(v, body, arg) ->
        errorEstimateAux  (MkLet((v,arg),body)) env

    | Var(x) -> env.[x]

    | Double(n) -> (n,Err(0.0))

    | _ -> failwithf "unrecognized term: %A" t

let rec errorEstimateRaw (t : Expr) =
    match t with
    | Lambda(x,t) ->
        (fun xv -> errorEstimateAux t (Map.of_seq [(x.Name,xv)]))
    | TopDef(body) ->
        errorEstimateRaw body
    | _ -> failwithf "unrecognized term: %A - expected a lambda" t

let rec errorEstimate (t : Expr<float -> float>) = errorEstimateRaw t.Raw

// ----------------------------

let (±) x = Err(x)

fsi.AddPrinter (fun (x:float,Err(v)) -> sprintf "%g±%g" x v)

errorEstimate <@ fun x -> x+2.0*x+3.0*x*x @> (1.0,±0.1)
// 6.0±0.93
errorEstimate <@ fun x -> let y = x + x in y*y + 2.0 @> (1.0,±0.1)
// 6.0±0.84
[<ReflectedDefinition>]
let poly x = x+2.0*x+3.0/(x*x)

errorEstimate <@ poly @> (3.0,±0.1)
// 9.33333±0.3242352
errorEstimate <@ poly @> (30271.3,±0.0001)
// 90813.9±0.0003
errorEstimate <@ fun x -> poly (x*x) @> (3.0,±1.0)
// 27.037±20.931
// ----------------------------
