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
#load "FMSufferingEval.fs"
open FMSufferingEval

type S = node * List<edge> * node

let rec walk (predicateNodes: List<node>) (spfs: List<S>) (startNode: node) (actions: List<edge>) (currentNode: node) =
  Seq.iter (fun connection ->
    let newActions = new List<edge>(actions)
    newActions.Add(fst connection)
    if predicateNodes.Contains(snd connection) then
      spfs.Add((startNode, newActions, snd connection))
    else
      walk predicateNodes spfs startNode newActions (snd connection)
  ) currentNode.connections

let buildSPF (predicateNodes: List<node>) (spfs: List<S>) =
  Seq.iter (fun n -> walk predicateNodes spfs n (new List<edge>()) n) predicateNodes


// Replace instances of varname with value in the predicate
let rec replaceAssignBool (varName: string) (value: arithExpr) (predicate: boolExpr) =
  match predicate with
  | True -> True
  | False -> False
  | StrongAndExpr(b1, b2) -> StrongAndExpr (replaceAssignBool varName value b1, replaceAssignBool varName value b2)
  | StrongOrExpr(b1, b2) ->  StrongOrExpr (replaceAssignBool varName value b1, replaceAssignBool varName value b2)
  | WeakAndExpr(b1, b2) -> WeakAndExpr (replaceAssignBool varName value b1, replaceAssignBool varName value b2)
  | WeakOrExpr(b1, b2) -> WeakOrExpr (replaceAssignBool varName value b1, replaceAssignBool varName value b2)
  | NotExpr(b) -> NotExpr (replaceAssignBool varName value b) 
  | EqualExpr(a1, a2) -> EqualExpr (replaceAssignArith varName value a1, replaceAssignArith varName value a2)
  | NotEqualExpr(a1, a2) -> NotEqualExpr (replaceAssignArith varName value a1, replaceAssignArith varName value a2)
  | GreaterExpr(a1, a2) -> GreaterExpr (replaceAssignArith varName value a1, replaceAssignArith varName value a2)
  | GreaterEqualExpr(a1, a2) -> GreaterEqualExpr (replaceAssignArith varName value a1, replaceAssignArith varName value a2)
  | LesserExpr(a1, a2) -> LesserExpr (replaceAssignArith varName value a1, replaceAssignArith varName value a2)
  | LesserEqualExpr(a1, a2) -> LesserEqualExpr (replaceAssignArith varName value a1, replaceAssignArith varName value a2)

and replaceAssignArith (varName: string) (value: arithExpr) (predicate: arithExpr) =
  match predicate with
  | Num(x) -> Num(x)
  | GetVariable(name) -> if name = varName then value else GetVariable(name)
  | GetArrayItem(name, index) -> GetArrayItem(name, index)
  | TimesArithExpr(a1, a2) -> TimesArithExpr (replaceAssignArith varName value a1, replaceAssignArith varName value a2)
  | DivArithExpr(a1, a2) -> DivArithExpr (replaceAssignArith varName value a1, replaceAssignArith varName value a2)
  | PlusArithExpr(a1, a2) -> PlusArithExpr (replaceAssignArith varName value a1, replaceAssignArith varName value a2)
  | MinusArithExpr(a1, a2) -> MinusArithExpr (replaceAssignArith varName value a1, replaceAssignArith varName value a2)
  | PowArithExpr(a1, a2) -> PowArithExpr (replaceAssignArith varName value a1, replaceAssignArith varName value a2)
  | UMinusArithExpr(a) -> UMinusArithExpr (replaceAssignArith varName value a)

// Replace instances of arrName[index] with value
let rec replaceAssignArrayBool (varName: string) (index: arithExpr) (value: arithExpr) (predicate: boolExpr) =
  match predicate with
  | True -> True
  | False -> False
  | StrongAndExpr(b1, b2) -> StrongAndExpr (replaceAssignArrayBool varName index value b1, replaceAssignArrayBool varName index value b2)
  | StrongOrExpr(b1, b2) ->  StrongOrExpr (replaceAssignArrayBool varName index value b1, replaceAssignArrayBool varName index value b2)
  | WeakAndExpr(b1, b2) -> WeakAndExpr (replaceAssignArrayBool varName index value b1, replaceAssignArrayBool varName index value b2)
  | WeakOrExpr(b1, b2) -> WeakOrExpr (replaceAssignArrayBool varName index value b1, replaceAssignArrayBool varName index value b2)
  | NotExpr(b) -> NotExpr (replaceAssignArrayBool varName index value b) 
  | EqualExpr(a1, a2) -> EqualExpr (replaceAssignArrayArith varName index value a1, replaceAssignArrayArith varName index value a2)
  | NotEqualExpr(a1, a2) -> NotEqualExpr (replaceAssignArrayArith varName index value a1, replaceAssignArrayArith varName index value a2)
  | GreaterExpr(a1, a2) -> GreaterExpr (replaceAssignArrayArith varName index value a1, replaceAssignArrayArith varName index value a2)
  | GreaterEqualExpr(a1, a2) -> GreaterEqualExpr (replaceAssignArrayArith varName index value a1, replaceAssignArrayArith varName index value a2)
  | LesserExpr(a1, a2) -> LesserExpr (replaceAssignArrayArith varName index value a1, replaceAssignArrayArith varName index value a2)
  | LesserEqualExpr(a1, a2) -> LesserEqualExpr (replaceAssignArrayArith varName index value a1, replaceAssignArrayArith varName index value a2)

and replaceAssignArrayArith (varName: string) (index: arithExpr) (value: arithExpr) (predicate: arithExpr) =
  match predicate with
  | Num(x) -> Num(x)
  | GetVariable(name) -> GetVariable(name)
  | GetArrayItem(name, aIndex) -> if name = varName && index.Equals(aIndex) then value else GetArrayItem(name, aIndex) // a + b == b + a
  | TimesArithExpr(a1, a2) -> TimesArithExpr (replaceAssignArrayArith varName index value a1, replaceAssignArrayArith varName index value a2)
  | DivArithExpr(a1, a2) -> DivArithExpr (replaceAssignArrayArith varName index value a1, replaceAssignArrayArith varName index value a2)
  | PlusArithExpr(a1, a2) -> PlusArithExpr (replaceAssignArrayArith varName index value a1, replaceAssignArrayArith varName index value a2)
  | MinusArithExpr(a1, a2) -> MinusArithExpr (replaceAssignArrayArith varName index value a1, replaceAssignArrayArith varName index value a2)
  | PowArithExpr(a1, a2) -> PowArithExpr (replaceAssignArrayArith varName index value a1, replaceAssignArrayArith varName index value a2)
  | UMinusArithExpr(a) -> UMinusArithExpr (replaceAssignArrayArith varName index value a)

let rec removeBoolExpr (be: boolExpr) (predicate: boolExpr) =
  match predicate with
  // | StrongAndExpr(b1, b2) -> StrongAndExpr (removeBoolExpr be b1, if be.Equals(b2) then True else b2)
  | StrongAndExpr(b1, b2) ->
    if be.Equals(b2) then
      removeBoolExpr be b1
    else if be.Equals(b1) then 
      b2
    else
      StrongAndExpr (removeBoolExpr be b1, b2)
  | WeakAndExpr(b1, b2) ->
    if be.Equals(b2) then
      removeBoolExpr be b1
    else if be.Equals(b1) then 
      b2
    else
      WeakAndExpr (removeBoolExpr be b1, b2)
  | _ -> if be.Equals(predicate) then True else predicate

// b = 3
// y = 2 & b = 3 & (a = 5 | b = 3)

let bottomsUp (acts: List<edge>) (predicate: boolExpr) =
  List.fold (fun acc act -> 
    match act with
    | CommandEdge c ->
      match c with
      | Assign(varName, value) -> replaceAssignBool (varName) (value) (acc)
      | AssignArray(arrName, index, value) -> replaceAssignArrayBool (arrName) (index) (value) (acc) 
      | Skip -> acc
    | BoolEdge b -> removeBoolExpr b acc
  ) predicate (List.rev (Seq.toList acts))

// TODO: Build custom equality checker for arithmetic and boolean expressions

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
        let mutable pv = false
        
        let e =
          if cons.[0] = '#' then
            let args = cons.[1..].Split(",") |> Array.map (fun el -> el.Trim())
            pg <- Array.contains "pg" args //program graph
            pp <- Array.contains "pp" args //pretty printer
            ev <- Array.contains "ev" args //evaluator function
            dt <- Array.contains "dt" args //determenistic
            pv <- Array.contains "pv" args //program validation
            if args[0].StartsWith("path=") then
              parse (System.IO.File.ReadAllText args[0].[5..])
            else
              parse (Console.ReadLine())
          else
            pg <- true
            pp <- true
            ev <- true
            dt <- true
            pv <- true
            parse cons

        let c = match e with
                | RCommand c -> c
                | RBoolExpr _ -> failwith "Program cannot start with a boolean expression!"

        printfn "Valid code!\n"

        let startNode = { connections = new List<(edge * node)>() }
        let endNode = { connections = new List<(edge * node)>() }
        let nodeNames = new List<(string * node)>()
        nodeNames.Add("q▷", startNode)
        nodeNames.Add("q◀", endNode)
        let loopNodes = new List<node>()
        loopNodes.Add(startNode)
        programGraphCommand startNode endNode dt loopNodes c
        loopNodes.Add(endNode)
        let pgString = pg2dot nodeNames startNode

        if pg then //btw these aren't errors they're features
          printfn "Program graph:"
          printfn "%s\n" pgString

        if pp then printfn "Pretty print:\n%s\n" (prettyPrintCommand c)

        if ev then
          let varMem = new Dictionary<string, int>()
          let arrMem = new Dictionary<string, List<int>>()
          printf "Enter program memory: "
          let mem = Console.ReadLine()
          if not (String.IsNullOrWhiteSpace(mem)) then
            readMemory mem varMem arrMem
          printf "Number of steps: "
          let steps = int (Console.ReadLine())
          stepProgram startNode endNode steps nodeNames varMem arrMem
        
        if pv then
          let predicates = new List<boolExpr>()
          printfn "Enter predicate assignments"
          Seq.iter (fun lnode ->
            printf $"P({getNodeName nodeNames lnode}): "
            let line = parse (Console.ReadLine())
            let predicate = match line with
                            | RCommand _ -> failwith "Predicate must be a boolean expression!"
                            | RBoolExpr b -> b
            predicates.Add(predicate)
          ) loopNodes

          let spfs = new List<S>()
          buildSPF loopNodes spfs

          Seq.iter (fun ((sn, acts, en): S) ->
            printfn $"{prettyPrintBool predicates[loopNodes.IndexOf(sn)]} => {prettyPrintBool (bottomsUp acts (predicates[loopNodes.IndexOf(en)]))}"
          ) spfs

          printfn "%A" (Seq.map (getNodeName nodeNames) loopNodes)
          printfn "%A" (Seq.map prettyPrintBool predicates)
          printfn "%A" (Seq.map (fun (sn, acts, en) ->
            let sActs = String.Join("   ", (Seq.map prettyPrintEdge acts))
            $"{getNodeName nodeNames sn} - {sActs} - {getNodeName nodeNames en}") spfs)

        compute n - 1
        with
          | Failure msg -> printfn "Invalid code! Error: %s" msg; compute n - 1

// Start interacting with the user
compute 3
