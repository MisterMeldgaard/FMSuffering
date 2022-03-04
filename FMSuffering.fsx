// This script implements our interactive FMSuffering

// We need to import a couple of modules, including the generated lexer and parser
#r "FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll"
open FSharp.Text.Lexing
open System
#load "FMSufferingTypesAST.fs"
open FMSufferingTypesAST
#load "FMSufferingParser.fs"
open FMSufferingParser
#load "FMSufferingLexer.fs"
open FMSufferingLexer

// We define the evaluation function recursively, by induction on the structure
// of arithmetic expressions (AST of type expr)
// let rec eval e =
//   match e with
//     | Num(x) -> x
//     | TimesArithExpr(x,y) -> eval(x) * eval (y)
//     | DivArithExpr(x,y) -> eval(x) / eval (y)
//     | PlusArithExpr(x,y) -> eval(x) + eval (y)
//     | MinusArithExpr(x,y) -> eval(x) - eval (y)
//     | PowArithExpr(x,y) -> eval(x) ** eval (y)
//     | UMinusArithExpr(x) -> - eval(x)

// We
let parse input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = FMSufferingParser.start FMSufferingLexer.tokenize lexbuf
    // return the result of parsing (i.e. value of type "expr")
    res

// We implement here the function that interacts with the user
let rec compute n =
        printf "Enter an arithmetic expression: "
        try
        // We parse the input string
        let e = parse (Console.ReadLine())
        printfn "Valid code!"
        // and print the result of evaluating it
        // printfn "Result: %f" (eval(e))
        compute n - 1
        with
          | Failure msg -> printfn "Invalid code! Error: %s" msg; compute n-1

// Start interacting with the user
compute 3
