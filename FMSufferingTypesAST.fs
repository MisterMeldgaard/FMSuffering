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
      match (aOther, this) with
      | (Num o, Num t) ->
          o = t
      | (GetVariable o, GetVariable t) ->
          o = t
      | (GetArrayItem (o1, o2), GetArrayItem (t1, t2)) ->
          o1 = t1 && o2.Equals(t2)
      | (TimesArithExpr (o1, o2), TimesArithExpr (t1, t2)) ->
          t1.Equals(o1) && t2.Equals(o2) || t1.Equals(o2) && t2.Equals(o1)
      | (DivArithExpr (o1, o2), DivArithExpr (t1, t2)) ->
          t1.Equals(o1) && t2.Equals(o2)
      | (PlusArithExpr (o1, o2), PlusArithExpr (t1, t2)) ->
          t1.Equals(o1) && t2.Equals(o2) || t1.Equals(o2) && t2.Equals(o1)
      | (MinusArithExpr (o1, o2), MinusArithExpr (t1, t2)) ->
          t1.Equals(o1) && t2.Equals(o2)
      | (PowArithExpr (o1, o2), PowArithExpr (t1, t2)) ->
          t1.Equals(o1) && t2.Equals(o2)
      | (UMinusArithExpr o, UMinusArithExpr t) ->
          o.Equals(t)
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
      match (bOther, this) with
      | (True, True) ->
          true
      | (False, False) ->
          true
      | (StrongAndExpr (o1, o2), StrongAndExpr (t1, t2)) ->
          t1.Equals(o1) && t2.Equals(o2) || t1.Equals(o2) && t2.Equals(o1)
      | (StrongOrExpr (o1, o2), StrongOrExpr (t1, t2)) ->
          t1.Equals(o1) && t2.Equals(o2) || t1.Equals(o2) && t2.Equals(o1)
      | (WeakAndExpr (o1, o2), WeakAndExpr (t1, t2)) ->
          t1.Equals(o1) && t2.Equals(o2)
      | (WeakOrExpr (o1, o2), WeakOrExpr (t1, t2)) ->
          t1.Equals(o1) && t2.Equals(o2)
      | (NotExpr o, NotExpr t) ->
          t.Equals(o)
      | (EqualExpr (o1, o2), EqualExpr (t1, t2)) ->
          t1.Equals(o1) && t2.Equals(o2) || t1.Equals(o2) && t2.Equals(o1)
      | (NotEqualExpr (o1, o2), NotEqualExpr (t1, t2)) ->
          t1.Equals(o1) && t2.Equals(o2) || t1.Equals(o2) && t2.Equals(o1)
      | (GreaterExpr (o1, o2), GreaterExpr (t1, t2)) ->
          t1.Equals(o1) && t2.Equals(o2)
      | (GreaterEqualExpr (o1, o2), GreaterEqualExpr (t1, t2)) ->
          t1.Equals(o1) && t2.Equals(o2)
      | (LesserExpr (o1, o2), LesserExpr (t1, t2)) ->
          t1.Equals(o1) && t2.Equals(o2)
      | (LesserEqualExpr (o1, o2), LesserEqualExpr (t1, t2)) ->
          t1.Equals(o1) && t2.Equals(o2)
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