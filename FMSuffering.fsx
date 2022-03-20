// This script implements our interactive FMSuffering

// We need to import a couple of modules, including the generated lexer and parser
open System.Collections.Generic
#r "FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll"
open FSharp.Text.Lexing
open System
#load "FMSufferingTypesAST.fs"
open FMSufferingTypesAST
#load "FMSufferingParser.fs"
open FMSufferingParser
#load "FMSufferingLexer.fs"
open FMSufferingLexer
#load "FMSufferingPrettyPrinter.fs"
open FMSufferingPrettyPrinter
#load "FMSufferingProgramGraph.fs"
open FMSufferingProgramGraph

type EvalStatus = Running | Stuck | Terminated
let evalStatusToString = function
  | Running -> "running"
  | Stuck -> "stuck"
  | Terminated -> "terminated"

let rec evalArith (arith: arithExpr) (varMem: Dictionary<string, int>) (arrMem: Dictionary<string, List<int>>) : int =
  match arith with
  | Num(x) -> (int) x
  | GetVariable(name) -> varMem[name]
  | GetArrayItem(name, index) -> arrMem[name][evalArith index varMem arrMem]
  | TimesArithExpr(a1, a2) -> (evalArith a1 varMem arrMem) * (evalArith a2 varMem arrMem)
  | DivArithExpr(a1, a2) -> (evalArith a1 varMem arrMem) / (evalArith a2 varMem arrMem)
  | PlusArithExpr(a1, a2) -> (evalArith a1 varMem arrMem) + (evalArith a2 varMem arrMem)
  | MinusArithExpr(a1, a2) -> (evalArith a1 varMem arrMem) - (evalArith a2 varMem arrMem)
  | PowArithExpr(a1, a2) -> pown (evalArith a1 varMem arrMem) (evalArith a2 varMem arrMem)
  | UMinusArithExpr(a) -> -(evalArith a varMem arrMem)

let rec evalBool (bool: boolExpr) (varMem: Dictionary<string, int>) (arrMem: Dictionary<string, List<int>>) =
  match bool with
  | True -> true
  | False -> false
  | StrongAndExpr(b1, b2) -> let b1r = evalBool b1 varMem arrMem
                             let b2r = evalBool b2 varMem arrMem
                             b1r && b2r
  | StrongOrExpr(b1, b2) ->  let b1r = evalBool b1 varMem arrMem
                             let b2r = evalBool b2 varMem arrMem
                             b1r || b2r
  | WeakAndExpr(b1, b2) -> (evalBool b1 varMem arrMem) && (evalBool b2 varMem arrMem)
  | WeakOrExpr(b1, b2) -> (evalBool b1 varMem arrMem) || (evalBool b2 varMem arrMem)
  | NotExpr(b) -> not (evalBool b varMem arrMem)
  | EqualExpr(a1, a2) -> (evalArith a1 varMem arrMem) = (evalArith a2 varMem arrMem)
  | NotEqualExpr(a1, a2) -> (evalArith a1 varMem arrMem) <> (evalArith a2 varMem arrMem)
  | GreaterExpr(a1, a2) -> (evalArith a1 varMem arrMem) > (evalArith a2 varMem arrMem)
  | GreaterEqualExpr(a1, a2) -> (evalArith a1 varMem arrMem) >= (evalArith a2 varMem arrMem)
  | LesserExpr(a1, a2) -> (evalArith a1 varMem arrMem) < (evalArith a2 varMem arrMem)
  | LesserEqualExpr(a1, a2) -> (evalArith a1 varMem arrMem) <= (evalArith a2 varMem arrMem)

let evalCommand (command: command) (varMem: Dictionary<string, int>) (arrMem: Dictionary<string, List<int>>) =
  match command with
  | Assign(varName, value) -> varMem[varName] <- (evalArith value varMem arrMem)
  | AssignArray(arrName, index, value) -> arrMem[arrName][(evalArith index varMem arrMem)] <- (evalArith value varMem arrMem)
  | Skip -> ()

let rec evalConnections (connections: List<edge * node>) (varMem: Dictionary<string, int>) (arrMem: Dictionary<string, List<int>>) =
  match connections[0] with
  | (CommandEdge command, nextNode) ->
    evalCommand command varMem arrMem
    nextNode
  | (BoolEdge b, nextNode) ->
    if (evalBool b varMem arrMem) then
      nextNode
    else
      // TODO: DETECT IF STUCK
      evalConnections (connections.GetRange(1, connections.Count - 1)) varMem arrMem

let evalStep (node: node) (varMem: Dictionary<string, int>) (arrMem: Dictionary<string, List<int>>) = evalConnections node.connections varMem arrMem

let getNodeName (nodeNames: List<(string * node)>) node = fst (nodeNames |> Seq.find (fun (_, fNode) -> fNode = node))

let printVarMem (varMem: Dictionary<string, int>) = Seq.fold (fun acc (KeyValue(key, value)) -> $"{acc}{key}={value}, ") "" varMem
let printArr (arr: List<int>) = Seq.fold (fun acc value -> $"{acc}{value}, ") "" arr
let printArrMem (arrMem: Dictionary<string, List<int>>) = Seq.fold (fun acc (KeyValue(key, value)) -> $"{acc}{key}=[{printArr value}], ") "" arrMem

let printState (status: EvalStatus) node nodeNames varMem arrMem =
  $"Status: {evalStatusToString status}\nNode: {getNodeName nodeNames node}\n{printVarMem varMem}\n{printArrMem arrMem}"

let readArray (values: string) =
  new List<int>(
    values.Split "," |>
    Array.map (fun el -> int el))

// Memory is expected to be in the same format as the way you type it into the field on FM4Fun
let readMemory (str: string) (varMem: Dictionary<string, int>) (arrMem: Dictionary<string, List<int>>) =
  str.Split "," |>
  Array.map (fun el -> el.Trim().Split "=") |>
  Array.iter (fun el -> if el[1].[0] = '[' then 
                          arrMem.Add(el[0].Trim(), readArray el[1])
                        else
                          varMem.Add(el[0].Trim(), int el[1]))

let parse input =
  // translate string into a buffer of characters
  let lexbuf = LexBuffer<char>.FromString input
  // translate the buffer into a stream of tokens and parse them
  let res = FMSufferingParser.start FMSufferingLexer.tokenize lexbuf
  // return the result of parsing (i.e. value of type "expr")
  res

// We implement here the function that interacts with the user
let rec compute n =
        printf "Enter a GCL program: "
        try
        // We parse the input string
        // If the input starts with # (would be illegal in the language)
        // then we interpret what's after the # as a path and load code from that file.
        // Otherwise we just interpret the input as code.

        // #path=examples/average_array.gcl, -pg, -pp
        // #-pg, -pp
        // skip

        // A=1, B=2

        let cons = Console.ReadLine()

        let mutable pg = false
        let mutable pp = false
        let mutable ev = false
        let mutable dt = false
        
        let e =
          if cons.[0] = '#' then
            let args = cons.[1..].Split(",") |> Array.map (fun el -> el.Trim())
            pg <- Array.contains "pg" args //program graph
            pp <- Array.contains "pp" args //pretty printer
            ev <- Array.contains "ev" args //evaluator function
            dt <- Array.contains "dt" args //determenistic
            if args[0].StartsWith("path=") then
              parse (System.IO.File.ReadAllText args[0].[5..])
            else
              parse (Console.ReadLine())
          else
            pg <- true
            pp <- true
            ev <- true
            dt <- true
            parse cons

        printfn "Valid code!\n"

        let startNode = { connections = new List<(edge * node)>() }
        let endNode = { connections = new List<(edge * node)>() }
        let nodeNames = new List<(string * node)>()
        nodeNames.Add("q▷", startNode)
        nodeNames.Add("q◀", endNode)
        programGraphCommand startNode endNode dt e
        let pgString = pg2dot nodeNames startNode

        if pg then //btw these aren't errors they're features
          printfn "Program graph:"
          printfn "%s\n" pgString

        if pp then printfn "Pretty print:\n%s\n" (prettyPrintCommand e)

        if ev then
          let varMem = new Dictionary<string, int>()
          let arrMem = new Dictionary<string, List<int>>()
          printf "Enter program memory: "
          readMemory (Console.ReadLine()) varMem arrMem
          let nextNode = evalStep startNode varMem arrMem
          printfn "%s" (printState Running nextNode nodeNames varMem arrMem)
        
        compute n - 1
        with
          | Failure msg -> printfn "Invalid code! Error: %s" msg; compute n - 1

// Start interacting with the user
compute 3
