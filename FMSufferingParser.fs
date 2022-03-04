// Implementation file for parser generated by fsyacc
module FMSufferingParser
#nowarn "64";; // turn off warnings that type variables used in production annotations are instantiated to concrete type
open FSharp.Text.Lexing
open FSharp.Text.Parsing.ParseHelpers
# 2 "FMSufferingParser.fsp"

open FMSufferingTypesAST

# 10 "FMSufferingParser.fs"
// This type is the type of tokens accepted by the parser
type token = 
  | TIMES
  | DIV
  | PLUS
  | MINUS
  | POW
  | LPAR
  | RPAR
  | ASSIGN
  | SKIP
  | SEMICOLON
  | IF
  | FI
  | DO
  | OD
  | LBRAC
  | RBRAC
  | ARROW
  | EQUAL
  | NOT
  | SMALLER
  | GREATER
  | AND
  | OR
  | TRUE
  | FALSE
  | EOF
  | SQBRAC
  | NAME of (string)
  | NUM of (float)
// This type is used to give symbolic names to token indexes, useful for error messages
type tokenId = 
    | TOKEN_TIMES
    | TOKEN_DIV
    | TOKEN_PLUS
    | TOKEN_MINUS
    | TOKEN_POW
    | TOKEN_LPAR
    | TOKEN_RPAR
    | TOKEN_ASSIGN
    | TOKEN_SKIP
    | TOKEN_SEMICOLON
    | TOKEN_IF
    | TOKEN_FI
    | TOKEN_DO
    | TOKEN_OD
    | TOKEN_LBRAC
    | TOKEN_RBRAC
    | TOKEN_ARROW
    | TOKEN_EQUAL
    | TOKEN_NOT
    | TOKEN_SMALLER
    | TOKEN_GREATER
    | TOKEN_AND
    | TOKEN_OR
    | TOKEN_TRUE
    | TOKEN_FALSE
    | TOKEN_EOF
    | TOKEN_SQBRAC
    | TOKEN_NAME
    | TOKEN_NUM
    | TOKEN_end_of_input
    | TOKEN_error
// This type is used to give symbolic names to token indexes, useful for error messages
type nonTerminalId = 
    | NONTERM__startstart
    | NONTERM_start
    | NONTERM_command
    | NONTERM_guardedCommand
    | NONTERM_arithExpr0
    | NONTERM_arithExpr1
    | NONTERM_arithExpr2
    | NONTERM_arithExpr3
    | NONTERM_boolExpr0
    | NONTERM_boolExpr1
    | NONTERM_boolExpr2
    | NONTERM_boolExpr3

// This function maps tokens to integer indexes
let tagOfToken (t:token) = 
  match t with
  | TIMES  -> 0 
  | DIV  -> 1 
  | PLUS  -> 2 
  | MINUS  -> 3 
  | POW  -> 4 
  | LPAR  -> 5 
  | RPAR  -> 6 
  | ASSIGN  -> 7 
  | SKIP  -> 8 
  | SEMICOLON  -> 9 
  | IF  -> 10 
  | FI  -> 11 
  | DO  -> 12 
  | OD  -> 13 
  | LBRAC  -> 14 
  | RBRAC  -> 15 
  | ARROW  -> 16 
  | EQUAL  -> 17 
  | NOT  -> 18 
  | SMALLER  -> 19 
  | GREATER  -> 20 
  | AND  -> 21 
  | OR  -> 22 
  | TRUE  -> 23 
  | FALSE  -> 24 
  | EOF  -> 25 
  | SQBRAC  -> 26 
  | NAME _ -> 27 
  | NUM _ -> 28 

// This function maps integer indexes to symbolic token ids
let tokenTagToTokenId (tokenIdx:int) = 
  match tokenIdx with
  | 0 -> TOKEN_TIMES 
  | 1 -> TOKEN_DIV 
  | 2 -> TOKEN_PLUS 
  | 3 -> TOKEN_MINUS 
  | 4 -> TOKEN_POW 
  | 5 -> TOKEN_LPAR 
  | 6 -> TOKEN_RPAR 
  | 7 -> TOKEN_ASSIGN 
  | 8 -> TOKEN_SKIP 
  | 9 -> TOKEN_SEMICOLON 
  | 10 -> TOKEN_IF 
  | 11 -> TOKEN_FI 
  | 12 -> TOKEN_DO 
  | 13 -> TOKEN_OD 
  | 14 -> TOKEN_LBRAC 
  | 15 -> TOKEN_RBRAC 
  | 16 -> TOKEN_ARROW 
  | 17 -> TOKEN_EQUAL 
  | 18 -> TOKEN_NOT 
  | 19 -> TOKEN_SMALLER 
  | 20 -> TOKEN_GREATER 
  | 21 -> TOKEN_AND 
  | 22 -> TOKEN_OR 
  | 23 -> TOKEN_TRUE 
  | 24 -> TOKEN_FALSE 
  | 25 -> TOKEN_EOF 
  | 26 -> TOKEN_SQBRAC 
  | 27 -> TOKEN_NAME 
  | 28 -> TOKEN_NUM 
  | 31 -> TOKEN_end_of_input
  | 29 -> TOKEN_error
  | _ -> failwith "tokenTagToTokenId: bad token"

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
let prodIdxToNonTerminal (prodIdx:int) = 
  match prodIdx with
    | 0 -> NONTERM__startstart 
    | 1 -> NONTERM_start 
    | 2 -> NONTERM_command 
    | 3 -> NONTERM_command 
    | 4 -> NONTERM_command 
    | 5 -> NONTERM_command 
    | 6 -> NONTERM_command 
    | 7 -> NONTERM_command 
    | 8 -> NONTERM_guardedCommand 
    | 9 -> NONTERM_guardedCommand 
    | 10 -> NONTERM_arithExpr0 
    | 11 -> NONTERM_arithExpr0 
    | 12 -> NONTERM_arithExpr0 
    | 13 -> NONTERM_arithExpr1 
    | 14 -> NONTERM_arithExpr1 
    | 15 -> NONTERM_arithExpr1 
    | 16 -> NONTERM_arithExpr2 
    | 17 -> NONTERM_arithExpr2 
    | 18 -> NONTERM_arithExpr3 
    | 19 -> NONTERM_arithExpr3 
    | 20 -> NONTERM_arithExpr3 
    | 21 -> NONTERM_arithExpr3 
    | 22 -> NONTERM_arithExpr3 
    | 23 -> NONTERM_boolExpr0 
    | 24 -> NONTERM_boolExpr0 
    | 25 -> NONTERM_boolExpr0 
    | 26 -> NONTERM_boolExpr1 
    | 27 -> NONTERM_boolExpr1 
    | 28 -> NONTERM_boolExpr1 
    | 29 -> NONTERM_boolExpr2 
    | 30 -> NONTERM_boolExpr2 
    | 31 -> NONTERM_boolExpr2 
    | 32 -> NONTERM_boolExpr2 
    | 33 -> NONTERM_boolExpr2 
    | 34 -> NONTERM_boolExpr2 
    | 35 -> NONTERM_boolExpr2 
    | 36 -> NONTERM_boolExpr3 
    | 37 -> NONTERM_boolExpr3 
    | 38 -> NONTERM_boolExpr3 
    | 39 -> NONTERM_boolExpr3 
    | _ -> failwith "prodIdxToNonTerminal: bad production index"

let _fsyacc_endOfInputTag = 31 
let _fsyacc_tagOfErrorTerminal = 29

// This function gets the name of a token as a string
let token_to_string (t:token) = 
  match t with 
  | TIMES  -> "TIMES" 
  | DIV  -> "DIV" 
  | PLUS  -> "PLUS" 
  | MINUS  -> "MINUS" 
  | POW  -> "POW" 
  | LPAR  -> "LPAR" 
  | RPAR  -> "RPAR" 
  | ASSIGN  -> "ASSIGN" 
  | SKIP  -> "SKIP" 
  | SEMICOLON  -> "SEMICOLON" 
  | IF  -> "IF" 
  | FI  -> "FI" 
  | DO  -> "DO" 
  | OD  -> "OD" 
  | LBRAC  -> "LBRAC" 
  | RBRAC  -> "RBRAC" 
  | ARROW  -> "ARROW" 
  | EQUAL  -> "EQUAL" 
  | NOT  -> "NOT" 
  | SMALLER  -> "SMALLER" 
  | GREATER  -> "GREATER" 
  | AND  -> "AND" 
  | OR  -> "OR" 
  | TRUE  -> "TRUE" 
  | FALSE  -> "FALSE" 
  | EOF  -> "EOF" 
  | SQBRAC  -> "SQBRAC" 
  | NAME _ -> "NAME" 
  | NUM _ -> "NUM" 

// This function gets the data carried by a token as an object
let _fsyacc_dataOfToken (t:token) = 
  match t with 
  | TIMES  -> (null : System.Object) 
  | DIV  -> (null : System.Object) 
  | PLUS  -> (null : System.Object) 
  | MINUS  -> (null : System.Object) 
  | POW  -> (null : System.Object) 
  | LPAR  -> (null : System.Object) 
  | RPAR  -> (null : System.Object) 
  | ASSIGN  -> (null : System.Object) 
  | SKIP  -> (null : System.Object) 
  | SEMICOLON  -> (null : System.Object) 
  | IF  -> (null : System.Object) 
  | FI  -> (null : System.Object) 
  | DO  -> (null : System.Object) 
  | OD  -> (null : System.Object) 
  | LBRAC  -> (null : System.Object) 
  | RBRAC  -> (null : System.Object) 
  | ARROW  -> (null : System.Object) 
  | EQUAL  -> (null : System.Object) 
  | NOT  -> (null : System.Object) 
  | SMALLER  -> (null : System.Object) 
  | GREATER  -> (null : System.Object) 
  | AND  -> (null : System.Object) 
  | OR  -> (null : System.Object) 
  | TRUE  -> (null : System.Object) 
  | FALSE  -> (null : System.Object) 
  | EOF  -> (null : System.Object) 
  | SQBRAC  -> (null : System.Object) 
  | NAME _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | NUM _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
let _fsyacc_gotos = [| 0us; 65535us; 1us; 65535us; 0us; 1us; 3us; 65535us; 0us; 2us; 15us; 13us; 23us; 14us; 3us; 65535us; 16us; 17us; 19us; 20us; 25us; 24us; 20us; 65535us; 5us; 6us; 7us; 8us; 10us; 11us; 16us; 29us; 19us; 29us; 25us; 29us; 53us; 26us; 55us; 27us; 56us; 28us; 59us; 29us; 61us; 29us; 64us; 29us; 66us; 29us; 69us; 30us; 71us; 31us; 72us; 32us; 73us; 33us; 74us; 34us; 75us; 35us; 81us; 29us; 22us; 65535us; 5us; 40us; 7us; 40us; 10us; 40us; 16us; 40us; 19us; 40us; 25us; 40us; 36us; 37us; 38us; 39us; 53us; 40us; 55us; 40us; 56us; 40us; 59us; 40us; 61us; 40us; 64us; 40us; 66us; 40us; 69us; 40us; 71us; 40us; 72us; 40us; 73us; 40us; 74us; 40us; 75us; 40us; 81us; 40us; 25us; 65535us; 5us; 45us; 7us; 45us; 10us; 45us; 16us; 45us; 19us; 45us; 25us; 45us; 36us; 45us; 38us; 45us; 41us; 42us; 43us; 44us; 47us; 48us; 53us; 45us; 55us; 45us; 56us; 45us; 59us; 45us; 61us; 45us; 64us; 45us; 66us; 45us; 69us; 45us; 71us; 45us; 72us; 45us; 73us; 45us; 74us; 45us; 75us; 45us; 81us; 45us; 26us; 65535us; 5us; 46us; 7us; 46us; 10us; 46us; 16us; 46us; 19us; 46us; 25us; 46us; 36us; 46us; 38us; 46us; 41us; 46us; 43us; 46us; 47us; 46us; 49us; 50us; 53us; 46us; 55us; 46us; 56us; 46us; 59us; 46us; 61us; 46us; 64us; 46us; 66us; 46us; 69us; 46us; 71us; 46us; 72us; 46us; 73us; 46us; 74us; 46us; 75us; 46us; 81us; 46us; 5us; 65535us; 16us; 22us; 19us; 22us; 25us; 22us; 56us; 58us; 81us; 58us; 7us; 65535us; 16us; 63us; 19us; 63us; 25us; 63us; 56us; 63us; 59us; 60us; 61us; 62us; 81us; 63us; 9us; 65535us; 16us; 68us; 19us; 68us; 25us; 68us; 56us; 68us; 59us; 68us; 61us; 68us; 64us; 65us; 66us; 67us; 81us; 68us; 10us; 65535us; 16us; 76us; 19us; 76us; 25us; 76us; 56us; 76us; 59us; 76us; 61us; 76us; 64us; 76us; 66us; 76us; 77us; 78us; 81us; 76us; |]
let _fsyacc_sparseGotoTableRowOffsets = [|0us; 1us; 3us; 7us; 11us; 32us; 55us; 81us; 108us; 114us; 122us; 132us; |]
let _fsyacc_stateToProdIdxsTableElements = [| 1us; 0us; 1us; 0us; 2us; 1us; 5us; 1us; 1us; 2us; 2us; 3us; 1us; 2us; 3us; 2us; 10us; 11us; 1us; 3us; 3us; 3us; 10us; 11us; 1us; 3us; 1us; 3us; 3us; 3us; 10us; 11us; 1us; 4us; 2us; 5us; 5us; 2us; 5us; 8us; 1us; 5us; 1us; 6us; 2us; 6us; 9us; 1us; 6us; 1us; 7us; 2us; 7us; 9us; 1us; 7us; 3us; 8us; 23us; 24us; 1us; 8us; 2us; 9us; 9us; 1us; 9us; 3us; 10us; 11us; 21us; 3us; 10us; 11us; 22us; 9us; 10us; 11us; 22us; 29us; 30us; 31us; 32us; 33us; 34us; 8us; 10us; 11us; 29us; 30us; 31us; 32us; 33us; 34us; 3us; 10us; 11us; 29us; 3us; 10us; 11us; 30us; 3us; 10us; 11us; 31us; 3us; 10us; 11us; 32us; 3us; 10us; 11us; 33us; 3us; 10us; 11us; 34us; 1us; 10us; 3us; 10us; 13us; 14us; 1us; 11us; 3us; 11us; 13us; 14us; 3us; 12us; 13us; 14us; 1us; 13us; 1us; 13us; 1us; 14us; 1us; 14us; 1us; 15us; 2us; 16us; 17us; 1us; 16us; 1us; 16us; 1us; 18us; 1us; 18us; 1us; 19us; 2us; 20us; 21us; 1us; 21us; 1us; 21us; 1us; 22us; 2us; 22us; 39us; 1us; 22us; 3us; 23us; 24us; 39us; 2us; 23us; 24us; 3us; 23us; 26us; 27us; 1us; 24us; 3us; 24us; 26us; 27us; 3us; 25us; 26us; 27us; 2us; 26us; 27us; 1us; 26us; 1us; 27us; 1us; 27us; 1us; 28us; 1us; 29us; 1us; 30us; 1us; 30us; 2us; 31us; 32us; 1us; 32us; 2us; 33us; 34us; 1us; 34us; 1us; 35us; 1us; 36us; 1us; 36us; 1us; 37us; 1us; 38us; 1us; 39us; 1us; 39us; |]
let _fsyacc_stateToProdIdxsTableRowOffsets = [|0us; 2us; 4us; 7us; 9us; 12us; 14us; 18us; 20us; 24us; 26us; 28us; 32us; 34us; 37us; 40us; 42us; 44us; 47us; 49us; 51us; 54us; 56us; 60us; 62us; 65us; 67us; 71us; 75us; 85us; 94us; 98us; 102us; 106us; 110us; 114us; 118us; 120us; 124us; 126us; 130us; 134us; 136us; 138us; 140us; 142us; 144us; 147us; 149us; 151us; 153us; 155us; 157us; 160us; 162us; 164us; 166us; 169us; 171us; 175us; 178us; 182us; 184us; 188us; 192us; 195us; 197us; 199us; 201us; 203us; 205us; 207us; 209us; 212us; 214us; 217us; 219us; 221us; 223us; 225us; 227us; 229us; 231us; |]
let _fsyacc_action_rows = 83
let _fsyacc_actionTableElements = [|4us; 32768us; 8us; 12us; 10us; 16us; 12us; 19us; 27us; 4us; 0us; 49152us; 2us; 32768us; 9us; 15us; 25us; 3us; 0us; 16385us; 2us; 32768us; 7us; 5us; 14us; 7us; 4us; 32768us; 3us; 49us; 5us; 55us; 27us; 52us; 28us; 51us; 2us; 16386us; 2us; 36us; 3us; 38us; 4us; 32768us; 3us; 49us; 5us; 55us; 27us; 52us; 28us; 51us; 3us; 32768us; 2us; 36us; 3us; 38us; 15us; 9us; 1us; 32768us; 7us; 10us; 4us; 32768us; 3us; 49us; 5us; 55us; 27us; 52us; 28us; 51us; 2us; 16387us; 2us; 36us; 3us; 38us; 0us; 16388us; 1us; 16389us; 9us; 15us; 1us; 16392us; 9us; 15us; 4us; 32768us; 8us; 12us; 10us; 16us; 12us; 19us; 27us; 4us; 7us; 32768us; 3us; 49us; 5us; 56us; 18us; 77us; 23us; 79us; 24us; 80us; 27us; 52us; 28us; 51us; 2us; 32768us; 11us; 18us; 26us; 25us; 0us; 16390us; 7us; 32768us; 3us; 49us; 5us; 56us; 18us; 77us; 23us; 79us; 24us; 80us; 27us; 52us; 28us; 51us; 2us; 32768us; 13us; 21us; 26us; 25us; 0us; 16391us; 2us; 32768us; 16us; 23us; 22us; 59us; 4us; 32768us; 8us; 12us; 10us; 16us; 12us; 19us; 27us; 4us; 1us; 16393us; 26us; 25us; 7us; 32768us; 3us; 49us; 5us; 56us; 18us; 77us; 23us; 79us; 24us; 80us; 27us; 52us; 28us; 51us; 3us; 32768us; 2us; 36us; 3us; 38us; 15us; 54us; 3us; 32768us; 2us; 36us; 3us; 38us; 6us; 57us; 7us; 32768us; 2us; 36us; 3us; 38us; 6us; 57us; 17us; 69us; 18us; 70us; 19us; 74us; 20us; 72us; 6us; 32768us; 2us; 36us; 3us; 38us; 17us; 69us; 18us; 70us; 19us; 74us; 20us; 72us; 2us; 16413us; 2us; 36us; 3us; 38us; 2us; 16414us; 2us; 36us; 3us; 38us; 2us; 16415us; 2us; 36us; 3us; 38us; 2us; 16416us; 2us; 36us; 3us; 38us; 2us; 16417us; 2us; 36us; 3us; 38us; 2us; 16418us; 2us; 36us; 3us; 38us; 4us; 32768us; 3us; 49us; 5us; 55us; 27us; 52us; 28us; 51us; 2us; 16394us; 0us; 41us; 1us; 43us; 4us; 32768us; 3us; 49us; 5us; 55us; 27us; 52us; 28us; 51us; 2us; 16395us; 0us; 41us; 1us; 43us; 2us; 16396us; 0us; 41us; 1us; 43us; 4us; 32768us; 3us; 49us; 5us; 55us; 27us; 52us; 28us; 51us; 0us; 16397us; 4us; 32768us; 3us; 49us; 5us; 55us; 27us; 52us; 28us; 51us; 0us; 16398us; 0us; 16399us; 1us; 16401us; 4us; 47us; 4us; 32768us; 3us; 49us; 5us; 55us; 27us; 52us; 28us; 51us; 0us; 16400us; 4us; 32768us; 3us; 49us; 5us; 55us; 27us; 52us; 28us; 51us; 0us; 16402us; 0us; 16403us; 1us; 16404us; 14us; 53us; 4us; 32768us; 3us; 49us; 5us; 55us; 27us; 52us; 28us; 51us; 0us; 16405us; 4us; 32768us; 3us; 49us; 5us; 55us; 27us; 52us; 28us; 51us; 7us; 32768us; 3us; 49us; 5us; 56us; 18us; 77us; 23us; 79us; 24us; 80us; 27us; 52us; 28us; 51us; 0us; 16406us; 2us; 32768us; 6us; 82us; 22us; 59us; 8us; 32768us; 3us; 49us; 5us; 56us; 18us; 77us; 22us; 61us; 23us; 79us; 24us; 80us; 27us; 52us; 28us; 51us; 1us; 16407us; 21us; 64us; 7us; 32768us; 3us; 49us; 5us; 56us; 18us; 77us; 23us; 79us; 24us; 80us; 27us; 52us; 28us; 51us; 1us; 16408us; 21us; 64us; 1us; 16409us; 21us; 64us; 8us; 32768us; 3us; 49us; 5us; 56us; 18us; 77us; 21us; 66us; 23us; 79us; 24us; 80us; 27us; 52us; 28us; 51us; 0us; 16410us; 7us; 32768us; 3us; 49us; 5us; 56us; 18us; 77us; 23us; 79us; 24us; 80us; 27us; 52us; 28us; 51us; 0us; 16411us; 0us; 16412us; 4us; 32768us; 3us; 49us; 5us; 55us; 27us; 52us; 28us; 51us; 1us; 32768us; 17us; 71us; 4us; 32768us; 3us; 49us; 5us; 55us; 27us; 52us; 28us; 51us; 5us; 32768us; 3us; 49us; 5us; 55us; 17us; 73us; 27us; 52us; 28us; 51us; 4us; 32768us; 3us; 49us; 5us; 55us; 27us; 52us; 28us; 51us; 5us; 32768us; 3us; 49us; 5us; 55us; 17us; 75us; 27us; 52us; 28us; 51us; 4us; 32768us; 3us; 49us; 5us; 55us; 27us; 52us; 28us; 51us; 0us; 16419us; 4us; 32768us; 5us; 81us; 18us; 77us; 23us; 79us; 24us; 80us; 0us; 16420us; 0us; 16421us; 0us; 16422us; 7us; 32768us; 3us; 49us; 5us; 56us; 18us; 77us; 23us; 79us; 24us; 80us; 27us; 52us; 28us; 51us; 0us; 16423us; |]
let _fsyacc_actionTableRowOffsets = [|0us; 5us; 6us; 9us; 10us; 13us; 18us; 21us; 26us; 30us; 32us; 37us; 40us; 41us; 43us; 45us; 50us; 58us; 61us; 62us; 70us; 73us; 74us; 77us; 82us; 84us; 92us; 96us; 100us; 108us; 115us; 118us; 121us; 124us; 127us; 130us; 133us; 138us; 141us; 146us; 149us; 152us; 157us; 158us; 163us; 164us; 165us; 167us; 172us; 173us; 178us; 179us; 180us; 182us; 187us; 188us; 193us; 201us; 202us; 205us; 214us; 216us; 224us; 226us; 228us; 237us; 238us; 246us; 247us; 248us; 253us; 255us; 260us; 266us; 271us; 277us; 282us; 283us; 288us; 289us; 290us; 291us; 299us; |]
let _fsyacc_reductionSymbolCounts = [|1us; 2us; 3us; 6us; 1us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 1us; 3us; 3us; 1us; 3us; 1us; 2us; 1us; 1us; 4us; 3us; 3us; 4us; 1us; 3us; 4us; 1us; 3us; 4us; 3us; 4us; 3us; 4us; 1us; 2us; 1us; 1us; 3us; |]
let _fsyacc_productionToNonTerminalTable = [|0us; 1us; 2us; 2us; 2us; 2us; 2us; 2us; 3us; 3us; 4us; 4us; 4us; 5us; 5us; 5us; 6us; 6us; 7us; 7us; 7us; 7us; 7us; 8us; 8us; 8us; 9us; 9us; 9us; 10us; 10us; 10us; 10us; 10us; 10us; 10us; 11us; 11us; 11us; 11us; |]
let _fsyacc_immediateActions = [|65535us; 49152us; 65535us; 16385us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 16388us; 65535us; 65535us; 65535us; 65535us; 65535us; 16390us; 65535us; 65535us; 16391us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 16397us; 65535us; 16398us; 16399us; 65535us; 65535us; 16400us; 65535us; 16402us; 16403us; 65535us; 65535us; 16405us; 65535us; 65535us; 16406us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 16410us; 65535us; 16411us; 16412us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 16419us; 65535us; 16420us; 16421us; 16422us; 65535us; 16423us; |]
let _fsyacc_reductions ()  =    [| 
# 283 "FMSufferingParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : command)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startstart));
# 292 "FMSufferingParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : command)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 42 "FMSufferingParser.fsp"
                                                      _1 
                   )
# 42 "FMSufferingParser.fsp"
                 : command));
# 303 "FMSufferingParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : arithExpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 59 "FMSufferingParser.fsp"
                                                                           Assign(_1,_3) 
                   )
# 59 "FMSufferingParser.fsp"
                 : command));
# 315 "FMSufferingParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : arithExpr)) in
            let _6 = (let data = parseState.GetInput(6) in (Microsoft.FSharp.Core.Operators.unbox data : arithExpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 60 "FMSufferingParser.fsp"
                                                                           AssignArray(_1,_3,_6) 
                   )
# 60 "FMSufferingParser.fsp"
                 : command));
# 328 "FMSufferingParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 61 "FMSufferingParser.fsp"
                                                                           Skip 
                   )
# 61 "FMSufferingParser.fsp"
                 : command));
# 338 "FMSufferingParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : command)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : command)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 62 "FMSufferingParser.fsp"
                                                                           CommandCommand(_1,_3) 
                   )
# 62 "FMSufferingParser.fsp"
                 : command));
# 350 "FMSufferingParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : guardedCommand)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 63 "FMSufferingParser.fsp"
                                                                           IfStatement(_2) 
                   )
# 63 "FMSufferingParser.fsp"
                 : command));
# 361 "FMSufferingParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : guardedCommand)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 64 "FMSufferingParser.fsp"
                                                                           DoStatement(_2) 
                   )
# 64 "FMSufferingParser.fsp"
                 : command));
# 372 "FMSufferingParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : boolExpr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : command)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 67 "FMSufferingParser.fsp"
                                                                           Condition(_1,_3) 
                   )
# 67 "FMSufferingParser.fsp"
                 : guardedCommand));
# 384 "FMSufferingParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : guardedCommand)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : guardedCommand)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 68 "FMSufferingParser.fsp"
                                                                      Choice(_1,_3) 
                   )
# 68 "FMSufferingParser.fsp"
                 : guardedCommand));
# 396 "FMSufferingParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : arithExpr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : arithExpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 71 "FMSufferingParser.fsp"
                                                         PlusArithExpr(_1,_3) 
                   )
# 71 "FMSufferingParser.fsp"
                 : arithExpr));
# 408 "FMSufferingParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : arithExpr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : arithExpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 72 "FMSufferingParser.fsp"
                                                         MinusArithExpr(_1,_3) 
                   )
# 72 "FMSufferingParser.fsp"
                 : arithExpr));
# 420 "FMSufferingParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : arithExpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 73 "FMSufferingParser.fsp"
                                                         _1 
                   )
# 73 "FMSufferingParser.fsp"
                 : arithExpr));
# 431 "FMSufferingParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : arithExpr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : arithExpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 76 "FMSufferingParser.fsp"
                                                         TimesArithExpr(_1,_3) 
                   )
# 76 "FMSufferingParser.fsp"
                 : arithExpr));
# 443 "FMSufferingParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : arithExpr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : arithExpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 77 "FMSufferingParser.fsp"
                                                         DivArithExpr(_1,_3) 
                   )
# 77 "FMSufferingParser.fsp"
                 : arithExpr));
# 455 "FMSufferingParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : arithExpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 78 "FMSufferingParser.fsp"
                                                         _1 
                   )
# 78 "FMSufferingParser.fsp"
                 : arithExpr));
# 466 "FMSufferingParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : arithExpr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : arithExpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 81 "FMSufferingParser.fsp"
                                                         PowArithExpr(_1,_3) 
                   )
# 81 "FMSufferingParser.fsp"
                 : arithExpr));
# 478 "FMSufferingParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : arithExpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 82 "FMSufferingParser.fsp"
                                                         _1 
                   )
# 82 "FMSufferingParser.fsp"
                 : arithExpr));
# 489 "FMSufferingParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : arithExpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 85 "FMSufferingParser.fsp"
                                                         UMinusArithExpr(_2) 
                   )
# 85 "FMSufferingParser.fsp"
                 : arithExpr));
# 500 "FMSufferingParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : float)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 86 "FMSufferingParser.fsp"
                                                         Num(_1) 
                   )
# 86 "FMSufferingParser.fsp"
                 : arithExpr));
# 511 "FMSufferingParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 87 "FMSufferingParser.fsp"
                                                         GetVariable(_1) 
                   )
# 87 "FMSufferingParser.fsp"
                 : arithExpr));
# 522 "FMSufferingParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : arithExpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 88 "FMSufferingParser.fsp"
                                                         GetArrayItem(_1,_3) 
                   )
# 88 "FMSufferingParser.fsp"
                 : arithExpr));
# 534 "FMSufferingParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : arithExpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 89 "FMSufferingParser.fsp"
                                                         _2 
                   )
# 89 "FMSufferingParser.fsp"
                 : arithExpr));
# 545 "FMSufferingParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : boolExpr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : boolExpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 92 "FMSufferingParser.fsp"
                                                         StrongOrExpr(_1,_3) 
                   )
# 92 "FMSufferingParser.fsp"
                 : boolExpr));
# 557 "FMSufferingParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : boolExpr)) in
            let _4 = (let data = parseState.GetInput(4) in (Microsoft.FSharp.Core.Operators.unbox data : boolExpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 93 "FMSufferingParser.fsp"
                                                         WeakOrExpr(_1,_4) 
                   )
# 93 "FMSufferingParser.fsp"
                 : boolExpr));
# 569 "FMSufferingParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : boolExpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 94 "FMSufferingParser.fsp"
                                                         _1 
                   )
# 94 "FMSufferingParser.fsp"
                 : boolExpr));
# 580 "FMSufferingParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : boolExpr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : boolExpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 97 "FMSufferingParser.fsp"
                                                          StrongAndExpr(_1,_3) 
                   )
# 97 "FMSufferingParser.fsp"
                 : boolExpr));
# 592 "FMSufferingParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : boolExpr)) in
            let _4 = (let data = parseState.GetInput(4) in (Microsoft.FSharp.Core.Operators.unbox data : boolExpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 98 "FMSufferingParser.fsp"
                                                          WeakAndExpr(_1,_4) 
                   )
# 98 "FMSufferingParser.fsp"
                 : boolExpr));
# 604 "FMSufferingParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : boolExpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 99 "FMSufferingParser.fsp"
                                                          _1 
                   )
# 99 "FMSufferingParser.fsp"
                 : boolExpr));
# 615 "FMSufferingParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : arithExpr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : arithExpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 102 "FMSufferingParser.fsp"
                                                               EqualExpr(_1,_3) 
                   )
# 102 "FMSufferingParser.fsp"
                 : boolExpr));
# 627 "FMSufferingParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : arithExpr)) in
            let _4 = (let data = parseState.GetInput(4) in (Microsoft.FSharp.Core.Operators.unbox data : arithExpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 103 "FMSufferingParser.fsp"
                                                               NotEqualExpr(_1,_4) 
                   )
# 103 "FMSufferingParser.fsp"
                 : boolExpr));
# 639 "FMSufferingParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : arithExpr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : arithExpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 104 "FMSufferingParser.fsp"
                                                               GreaterExpr(_1,_3) 
                   )
# 104 "FMSufferingParser.fsp"
                 : boolExpr));
# 651 "FMSufferingParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : arithExpr)) in
            let _4 = (let data = parseState.GetInput(4) in (Microsoft.FSharp.Core.Operators.unbox data : arithExpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 105 "FMSufferingParser.fsp"
                                                               GreaterEqualExpr(_1,_4) 
                   )
# 105 "FMSufferingParser.fsp"
                 : boolExpr));
# 663 "FMSufferingParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : arithExpr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : arithExpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 106 "FMSufferingParser.fsp"
                                                               SmallerExpr(_1,_3) 
                   )
# 106 "FMSufferingParser.fsp"
                 : boolExpr));
# 675 "FMSufferingParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : arithExpr)) in
            let _4 = (let data = parseState.GetInput(4) in (Microsoft.FSharp.Core.Operators.unbox data : arithExpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 107 "FMSufferingParser.fsp"
                                                               SmallerEqualExpr(_1,_4) 
                   )
# 107 "FMSufferingParser.fsp"
                 : boolExpr));
# 687 "FMSufferingParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : boolExpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 108 "FMSufferingParser.fsp"
                                                               _1 
                   )
# 108 "FMSufferingParser.fsp"
                 : boolExpr));
# 698 "FMSufferingParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : boolExpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 111 "FMSufferingParser.fsp"
                                                         NotExpr(_2) 
                   )
# 111 "FMSufferingParser.fsp"
                 : boolExpr));
# 709 "FMSufferingParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 112 "FMSufferingParser.fsp"
                                                         True 
                   )
# 112 "FMSufferingParser.fsp"
                 : boolExpr));
# 719 "FMSufferingParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 113 "FMSufferingParser.fsp"
                                                         False 
                   )
# 113 "FMSufferingParser.fsp"
                 : boolExpr));
# 729 "FMSufferingParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : boolExpr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 114 "FMSufferingParser.fsp"
                                                         _2 
                   )
# 114 "FMSufferingParser.fsp"
                 : boolExpr));
|]
# 741 "FMSufferingParser.fs"
let tables () : FSharp.Text.Parsing.Tables<_> = 
  { reductions= _fsyacc_reductions ();
    endOfInputTag = _fsyacc_endOfInputTag;
    tagOfToken = tagOfToken;
    dataOfToken = _fsyacc_dataOfToken; 
    actionTableElements = _fsyacc_actionTableElements;
    actionTableRowOffsets = _fsyacc_actionTableRowOffsets;
    stateToProdIdxsTableElements = _fsyacc_stateToProdIdxsTableElements;
    stateToProdIdxsTableRowOffsets = _fsyacc_stateToProdIdxsTableRowOffsets;
    reductionSymbolCounts = _fsyacc_reductionSymbolCounts;
    immediateActions = _fsyacc_immediateActions;
    gotos = _fsyacc_gotos;
    sparseGotoTableRowOffsets = _fsyacc_sparseGotoTableRowOffsets;
    tagOfErrorTerminal = _fsyacc_tagOfErrorTerminal;
    parseError = (fun (ctxt:FSharp.Text.Parsing.ParseErrorContext<_>) -> 
                              match parse_error_rich with 
                              | Some f -> f ctxt
                              | None -> parse_error ctxt.Message);
    numTerminals = 32;
    productionToNonTerminalTable = _fsyacc_productionToNonTerminalTable  }
let engine lexer lexbuf startState = (tables ()).Interpret(lexer, lexbuf, startState)
let start lexer lexbuf : command =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 0))
