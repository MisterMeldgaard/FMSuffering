// C ::= x := a | A[a] := a | skip | C ; C | if GC fi | do GC od
// GC ::= b -> C | GC [] GC
// a ::= n | x | A[a] | a + a | a - a | a * a | a / a | - a | a ^ a | (a)
// b ::= true | false | b & b | b | b | b && b | b || b | !b
// | a = a | a != a | a > a | a >= a | a < a | a <= a | (b)

// This file implements a module where we define a data type "arithExpr"
// to store represent arithExpr ArithExpr expressions
module FMSufferingTypesAST

type arithExpr =
  | Num of float
  | GetVariable of string
  | GetArrayItem of (string * arithExpr)
  | TimesArithExpr of (arithExpr * arithExpr)
  | DivArithExpr of (arithExpr * arithExpr)
  | PlusArithExpr of (arithExpr * arithExpr)
  | MinusArithExpr of (arithExpr * arithExpr)
  | PowArithExpr of (arithExpr * arithExpr)
  | UMinusArithExpr of (arithExpr)

and boolExpr = 
  | True
  | False
  | StrongAndExpr of (boolExpr * boolExpr) // Strong always evaluates both expressions
  | StrongOrExpr of (boolExpr * boolExpr)
  | WeakAndExpr of (boolExpr * boolExpr) // Weak only looks at the first operator before failing
  | WeakOrExpr of (boolExpr * boolExpr)
  | NotExpr of (boolExpr)
  | EqualExpr of (arithExpr * arithExpr)
  | NotEqualExpr of (arithExpr * arithExpr)
  | GreaterExpr of (arithExpr * arithExpr)
  | GreaterEqualExpr of (arithExpr * arithExpr)
  | LesserExpr of (arithExpr * arithExpr)
  | LesserEqualExpr of (arithExpr * arithExpr)

and guardedCommand =
  | Condition of (boolExpr * command)
  | Choice of (guardedCommand * guardedCommand)

and command =
  | Assign of (string * arithExpr)
  | AssignArray of (string * arithExpr * arithExpr)
  | Skip
  | CommandCommand of (command * command)
  | IfStatement of (guardedCommand)
  | DoStatement of (guardedCommand)
