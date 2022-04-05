module FMSufferingVerification

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

let bottomsUp (acts: List<edge>) (predicate: boolExpr) =
  List.fold (fun acc act -> 
    match act with
    | CommandEdge c ->
      match c with
      | Assign(varName, value) -> replaceAssignBool (varName) (value) (acc)
      | AssignArray(arrName, index, value) -> replaceAssignArrayBool (arrName) (index) (value) (acc) 
      | Skip -> acc
    | BoolEdge b -> WeakAndExpr(acc, b)
  ) predicate (List.rev (Seq.toList acts))
