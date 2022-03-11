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

type node = { connections: List<(edge * node)> }
and edge = Command of command | BoolExpr of boolExpr

let start = { connections = new List<(edge * node)>() }

let rec programGraphCommand (startNode: node) (endNode: node) = function
    | Assign(a, b) -> startNode.connections.Add(Command(Assign(a, b)), endNode)
    | AssignArray(a, b, c) -> startNode.connections.Add(Command(AssignArray(a, b, c)), endNode)
    | Skip -> startNode.connections.Add(Command(Skip), endNode)
    | CommandCommand(c1, c2) -> let fresh = { connections = new List<(edge * node)>() }
                                programGraphCommand startNode fresh c1
                                programGraphCommand fresh endNode c2
    | IfStatement(a) -> programGraphGuarded startNode endNode a
    | DoStatement(a) -> programGraphGuarded startNode startNode a
                        startNode.connections.Add(BoolExpr(pgDone a), endNode)

and programGraphGuarded (startNode: node) (endNode: node) = function
    | Condition(b, c) -> let fresh = { connections = new List<(edge * node)>() }
                         programGraphCommand fresh endNode c
                         startNode.connections.Add(BoolExpr(b), fresh)
    | Choice (gc1, gc2) -> programGraphGuarded startNode endNode gc1
                           programGraphGuarded startNode endNode gc2

and pgDone = function
  | Condition(b, c) -> NotExpr b    
  | Choice(gc1, gc2) -> StrongAndExpr (pgDone gc1, pgDone gc2)

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
        let cons = Console.ReadLine()
        let e =
          if cons.StartsWith("#") then
            parse (System.IO.File.ReadAllText cons.[1..])
          else
            parse cons

        printfn "Valid code!"
        printfn "%s" (prettyPrintCommand e)
        // and print the result of evaluating it
        // printfn "Result: %f" (eval(e))
        compute n - 1
        with
          | Failure msg -> printfn "Invalid code! Error: %s" msg; compute n - 1

// Start interacting with the user
compute 3
