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

        if pg then //btw these aren't errors they're features
          printfn "Program graph:"
          let startNode = { connections = new List<(edge * node)>() }
          let endNode = { connections = new List<(edge * node)>() }
          programGraphCommand startNode endNode dt e
          let graphVizVisited = new List<(string * node)>()
          graphVizVisited.Add("q▷", startNode)
          graphVizVisited.Add("q◀", endNode)
          printfn "%s\n" (pg2dot graphVizVisited startNode)

        if pp then printfn "Pretty print:\n%s\n" (prettyPrintCommand e)

        if ev then printfn "ev"
        
        compute n - 1
        with
          | Failure msg -> printfn "Invalid code! Error: %s" msg; compute n - 1

// Start interacting with the user
compute 3
