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
#load "FMSufferingVerification.fs"
open FMSufferingVerification

open System.Text.RegularExpressions

type Sign = Negative | Zero | Positive

type SignConf = { vars: Dictionary<string, Sign>; arrs: Dictionary<string, Set<Sign>> }

let signFromString = function
  | "+" -> Positive
  | "-" -> Negative
  | "0" -> Zero
  | _ -> failwith "Invalid sign"

let readSignsArray (values: string) =
  new Set<Sign>(
    values.Split ";" |>
    Array.map (fun el -> signFromString (el.Trim())))

// let readSigns (str: string) (varSigns: Dictionary<node, Dictionary<string, Sign>>) (arrSigns: Dictionary<node, Dictionary<string, Set<Sign>>>) (startNode: node) = 
let readSigns (str: string) (signMem: Dictionary<node, List<SignConf>>) (startNode: node) =
  // Create new empty conf
  let conf = { vars = new Dictionary<string, Sign>(); arrs = new Dictionary<string, Set<Sign>>() }

  // Populate conf from the input string
  str.Split "," |>
  Array.map (fun el -> trimAll (el.Trim().Split "=")) |>
  Array.iter (fun el ->
    if el[1].[0] = '[' then
      conf.arrs[el[0]] <- readSignsArray (Regex.Replace(el[1], "\[|\]", ""))
    else
      conf.vars[el[0]] <- signFromString (el[1]))
  
  // Add the populated conf - if nothing is on node initialize first
  if not (signMem.ContainsKey(startNode)) then
    signMem.Add(startNode, new List<SignConf>())
  signMem[startNode].Add(conf)

let sortedMem (dic: Dictionary<string, 'a>) =
  Seq.sortBy (fun (KeyValue(k, v)) -> k) dic

let printSign = function
  | Negative -> "-"
  | Zero -> "0"
  | Positive -> "+"

let printSignConf (sc: SignConf) =
  printfn "%s" (Seq.fold (fun acc (KeyValue(name, sign)) -> $"{acc}{printSign sign, String.length name} ") "" (sortedMem sc.vars))

let printSignConfs (scs: List<SignConf>) =
  printfn "%s" (Seq.fold (fun acc (KeyValue(name, _)) -> $"{acc}{name} ") "" (sortedMem scs[0].vars))
  Seq.iter(fun sc ->
    printSignConf sc
  ) scs

let printSigns (signMem: Dictionary<node, List<SignConf>>) (nodeNames: List<(string * node)>) =
  Seq.iter (fun (KeyValue(n, scs)) ->
    printfn $"{(getNodeName nodeNames n)}"
    printSignConfs scs
    printfn ""
  ) signMem

// qstart
// x ybob z A
// + +    + {+,-,0}
// - -    - {+,-,0}
//
// q1
// x ybob z
// + -    +
// - -    -

let parse input =
  // translate string into a buffer of characters
  let lexbuf = LexBuffer<char>.FromString input
  // translate the buffer into a stream of tokens and parse them, then return it
  FMSufferingParser.start FMSufferingLexer.tokenize lexbuf

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
        let mutable ev = true
        let mutable dt = false
        let mutable pv = false
        let mutable sa = false
        
        let e =
          if cons.[0] = '#' then
            let args = cons.[1..].Split(",") |> Array.map (fun el -> el.Trim())
            pg <- Array.contains "pg" args //program graph
            pp <- Array.contains "pp" args //pretty printer
            ev <- Array.contains "ev" args //evaluator function
            dt <- Array.contains "dt" args //determenistic
            pv <- Array.contains "pv" args //program validation
            sa <- Array.contains "sa" args //Sign analysis
            if args[0].StartsWith("path=") then
              parse (System.IO.File.ReadAllText args[0].[5..])
            else
              parse (Console.ReadLine())
          else
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
            printfn $"From {getNodeName nodeNames sn} to {getNodeName nodeNames en}: {prettyPrintBool predicates[loopNodes.IndexOf(sn)]} => {prettyPrintBool (bottomsUp acts (predicates[loopNodes.IndexOf(en)]))}"
          ) spfs

          printfn "%A" (Seq.map (getNodeName nodeNames) loopNodes)
          printfn "%A" (Seq.map prettyPrintBool predicates)
          printfn "%A" (Seq.map (fun (sn, acts, en) ->
            let sActs = String.Join("   ", (Seq.map prettyPrintEdge acts))
            $"{getNodeName nodeNames sn} - {sActs} - {getNodeName nodeNames en}") spfs)

        if sa then
          printf "Enter the desired number of initial Sign Analyses: "
          let num = int (Console.ReadLine())
          let signMem = new Dictionary<node, List<SignConf>>()
          for i = 1 to num do
            printf $"Enter Sign Analysis #{i}: "
            let signA = Console.ReadLine()
            if not (String.IsNullOrWhiteSpace(signA)) then
              readSigns signA signMem startNode
          printSigns signMem nodeNames

        compute n - 1
        with
          | Failure msg -> printfn "Invalid code! Error: %s" msg; compute n - 1

// Start interacting with the user
compute 3
