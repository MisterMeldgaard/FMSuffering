// C ::= x := a | A[a] := a | skip | C ; C | if GC fi | do GC od
// GC ::= b -> C | GC [] GC
// a ::= n | x | A[a] | a + a | a - a | a * a | a / a | - a | a ^ a | (a)
// b ::= true | false | b & b | b | b | b && b | b || b | !b
// | a = a | a != a | a > a | a >= a | a < a | a <= a | (b)

// This file implements a module where we define a data type "arithExpr"
// to store represent arithExpr ArithExpr expressions
module FMSufferingTypesAST

[<CustomEquality; NoComparison>]
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

  override this.Equals other =
    match other with
    | :? arithExpr as aOther ->
      match aOther with
      | Num(o) ->
        match this with
        | Num(t) -> o = t
        | _ -> false
      | GetVariable(o) ->
        match this with
        | GetVariable(t) -> o = t
        | _ -> false
      | GetArrayItem(o1, o2) ->
        match this with
        | GetArrayItem(t1, t2) -> o1 = t1 && o2.Equals(t2)
        | _ -> false
      | TimesArithExpr(o1, o2) ->
        match this with
        | TimesArithExpr(t1, t2) -> t1.Equals(o1) && t2.Equals(o2) || t1.Equals(o2) && t2.Equals(o1)
        | _ -> false
      | DivArithExpr(o1, o2) ->
        match this with
        | DivArithExpr(t1, t2) -> t1.Equals(o1) && t2.Equals(o2)
        | _ -> false
      | PlusArithExpr(o1, o2) ->
        match this with
        | PlusArithExpr(t1, t2) -> t1.Equals(o1) && t2.Equals(o2) || t1.Equals(o2) && t2.Equals(o1)
        | _ -> false
      | MinusArithExpr(o1, o2) ->
        match this with
        | MinusArithExpr(t1, t2) -> t1.Equals(o1) && t2.Equals(o2)
        | _ -> false
      | PowArithExpr(o1, o2) ->
        match this with
        | PowArithExpr(t1, t2) -> t1.Equals(o1) && t2.Equals(o2)
        | _ -> false
      | UMinusArithExpr(o) ->
        match this with
        | UMinusArithExpr(t) -> o.Equals(t)
        | _ -> false
      
    | _ -> false

[<CustomEquality; NoComparison>]
type boolExpr = 
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

  override this.Equals other =
    match other with
    | :? boolExpr as bOther ->
      match bOther with
      | True -> this = True
      | False -> this = False
      | StrongAndExpr(o1, o2) ->
        match this with
        | StrongAndExpr(t1, t2) -> t1.Equals(o1) && t2.Equals(o2) || t1.Equals(o2) && t2.Equals(o1)
        | _ -> false
      | StrongOrExpr(o1, o2) -> 
        match this with
        | StrongOrExpr(t1, t2) -> t1.Equals(o1) && t2.Equals(o2) || t1.Equals(o2) && t2.Equals(o1)
        | _ -> false
      | WeakAndExpr(o1, o2) ->
        match this with
        | WeakAndExpr(t1, t2) -> t1.Equals(o1) && t2.Equals(o2)
        | _ -> false
      | WeakOrExpr(o1, o2) ->
        match this with
        | WeakOrExpr(t1, t2) -> t1.Equals(o1) && t2.Equals(o2)
        | _ -> false
      | NotExpr(o) ->
        match this with
        | NotExpr(t) -> t.Equals(o)
        | _ -> false
      | EqualExpr(o1, o2) ->
        match this with
        | EqualExpr(t1, t2) -> t1.Equals(o1) && t2.Equals(o2) || t1.Equals(o2) && t2.Equals(o1)
        | _ -> false
      | NotEqualExpr(o1, o2) ->
        match this with
        | NotEqualExpr(t1, t2) -> t1.Equals(o1) && t2.Equals(o2) || t1.Equals(o2) && t2.Equals(o1)
        | _ -> false
      | GreaterExpr(o1, o2) ->
        match this with
        | GreaterExpr(t1, t2) -> t1.Equals(o1) && t2.Equals(o2)
        | _ -> false
      | GreaterEqualExpr(o1, o2) ->
        match this with
        | GreaterEqualExpr(t1, t2) -> t1.Equals(o1) && t2.Equals(o2) 
        | _ -> false
      | LesserExpr(o1, o2) ->
        match this with
        | LesserExpr(t1, t2) -> t1.Equals(o1) && t2.Equals(o2) 
        | _ -> false
      | LesserEqualExpr(o1, o2) ->
        match this with
        | LesserEqualExpr(t1, t2) -> t1.Equals(o1) && t2.Equals(o2) 
        | _ -> false
    | _ -> false

type guardedCommand =
  | Condition of (boolExpr * command)
  | Choice of (guardedCommand * guardedCommand)

and command =
  | Assign of (string * arithExpr)
  | AssignArray of (string * arithExpr * arithExpr)
  | Skip
  | CommandCommand of (command * command)
  | IfStatement of (guardedCommand)
  | DoStatement of (guardedCommand)

type parserReturnType = 
  | RCommand of command
  | RBoolExpr of boolExpr