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

let rec prettyPrintArith = function
    | Num(x) -> string x
    | GetVariable(a) -> a
    | GetArrayItem(a,x) -> sprintf "%s[%s]" a (prettyPrintArith x)
    | TimesArithExpr(x,y) -> sprintf "(%s * %s)" (prettyPrintArith x) (prettyPrintArith y)
    | DivArithExpr(x,y) -> sprintf "(%s / %s)" (prettyPrintArith x) (prettyPrintArith y)
    | PlusArithExpr(x,y) -> sprintf "(%s + %s)" (prettyPrintArith x) (prettyPrintArith y)
    | MinusArithExpr(x,y) -> sprintf "(%s - %s)" (prettyPrintArith x) (prettyPrintArith y)
    | PowArithExpr(x,y) -> sprintf "(%s ^ %s)" (prettyPrintArith x) (prettyPrintArith y)
    | UMinusArithExpr(x) -> sprintf "(-%s)" (prettyPrintArith x)

and prettyPrintBool = function
    | True -> "true"
    | False -> "false"
    | StrongAndExpr(x,y) -> sprintf "%s & %s" (prettyPrintBool x) (prettyPrintBool y)
    | StrongOrExpr(x,y) -> sprintf "%s | %s" (prettyPrintBool x) (prettyPrintBool y)
    | WeakAndExpr(x,y) -> sprintf "%s && %s" (prettyPrintBool x) (prettyPrintBool y)
    | WeakOrExpr(x,y) -> sprintf "%s || %s" (prettyPrintBool x) (prettyPrintBool y)
    | NotExpr(x) -> sprintf "!%s" (prettyPrintBool x)
    | EqualExpr(x,y) -> sprintf "%s = %s" (prettyPrintArith x) (prettyPrintArith y)
    | NotEqualExpr(x,y) -> sprintf "%s != %s" (prettyPrintArith x) (prettyPrintArith y)
    | GreaterExpr(x,y) -> sprintf "%s > %s" (prettyPrintArith x) (prettyPrintArith y)
    | GreaterEqualExpr(x,y) -> sprintf "%s >= %s" (prettyPrintArith x) (prettyPrintArith y)
    | LesserExpr(x,y) -> sprintf "%s < %s" (prettyPrintArith x) (prettyPrintArith y)
    | LesserEqualExpr(x,y) -> sprintf "%s <= %s" (prettyPrintArith x) (prettyPrintArith y)

and prettyPrintGuarded = function
    | Condition(b, c) -> sprintf "%s -> %s" (prettyPrintBool b) (prettyPrintCommand c)
    | Choice (gc1, gc2) -> sprintf "%s\n[] %s" (prettyPrintGuarded gc1) (prettyPrintGuarded gc2)

and prettyPrintCommand = function
    | Assign(a, b) -> sprintf "%s := %s" a (prettyPrintArith b)
    | AssignArray(a, b, c) -> sprintf "%s[%s] := %s" a (prettyPrintArith b) (prettyPrintArith c)
    | Skip -> "skip"
    | Break -> "break"
    | Continue -> "continue"
    | CommandCommand(c1, c2) -> sprintf "%s;\n%s" (prettyPrintCommand c1) (prettyPrintCommand c2)
    | IfStatement(a) -> sprintf "if %s\nfi" (prettyPrintGuarded a)
    | DoStatement(a) -> sprintf "do %s\nod" (prettyPrintGuarded a)


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
