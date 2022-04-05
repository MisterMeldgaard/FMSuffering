module FMSufferingEval

open System.Text.RegularExpressions

type EvalStatus = Running | Stuck | Terminated
let evalStatusToString = function
  | Running -> "running"
  | Stuck -> "stuck"
  | Terminated -> "terminated"

let rec evalArith (arith: arithExpr) (varMem: Dictionary<string, int>) (arrMem: Dictionary<string, List<int>>) : int =
  match arith with
  | Num(x) -> (int) x
  | GetVariable(name) -> varMem[name]
  | GetArrayItem(name, index) -> arrMem[name][evalArith index varMem arrMem]
  | TimesArithExpr(a1, a2) -> (evalArith a1 varMem arrMem) * (evalArith a2 varMem arrMem)
  | DivArithExpr(a1, a2) -> (evalArith a1 varMem arrMem) / (evalArith a2 varMem arrMem)
  | PlusArithExpr(a1, a2) -> (evalArith a1 varMem arrMem) + (evalArith a2 varMem arrMem)
  | MinusArithExpr(a1, a2) -> (evalArith a1 varMem arrMem) - (evalArith a2 varMem arrMem)
  | PowArithExpr(a1, a2) -> pown (evalArith a1 varMem arrMem) (evalArith a2 varMem arrMem)
  | UMinusArithExpr(a) -> -(evalArith a varMem arrMem)

let rec evalBool (bool: boolExpr) (varMem: Dictionary<string, int>) (arrMem: Dictionary<string, List<int>>) =
  match bool with
  | True -> true
  | False -> false
  | StrongAndExpr(b1, b2) -> let b1r = evalBool b1 varMem arrMem
                             let b2r = evalBool b2 varMem arrMem
                             b1r && b2r
  | StrongOrExpr(b1, b2) ->  let b1r = evalBool b1 varMem arrMem
                             let b2r = evalBool b2 varMem arrMem
                             b1r || b2r
  | WeakAndExpr(b1, b2) -> (evalBool b1 varMem arrMem) && (evalBool b2 varMem arrMem)
  | WeakOrExpr(b1, b2) -> (evalBool b1 varMem arrMem) || (evalBool b2 varMem arrMem)
  | NotExpr(b) -> not (evalBool b varMem arrMem)
  | EqualExpr(a1, a2) -> (evalArith a1 varMem arrMem) = (evalArith a2 varMem arrMem)
  | NotEqualExpr(a1, a2) -> (evalArith a1 varMem arrMem) <> (evalArith a2 varMem arrMem)
  | GreaterExpr(a1, a2) -> (evalArith a1 varMem arrMem) > (evalArith a2 varMem arrMem)
  | GreaterEqualExpr(a1, a2) -> (evalArith a1 varMem arrMem) >= (evalArith a2 varMem arrMem)
  | LesserExpr(a1, a2) -> (evalArith a1 varMem arrMem) < (evalArith a2 varMem arrMem)
  | LesserEqualExpr(a1, a2) -> (evalArith a1 varMem arrMem) <= (evalArith a2 varMem arrMem)

let evalCommand (command: command) (varMem: Dictionary<string, int>) (arrMem: Dictionary<string, List<int>>) =
  match command with
  | Assign(varName, value) -> varMem[varName] <- (evalArith value varMem arrMem)
  | AssignArray(arrName, index, value) -> arrMem[arrName][(evalArith index varMem arrMem)] <- (evalArith value varMem arrMem)
  | Skip -> ()

let rec evalConnections (connections: List<edge * node>) (varMem: Dictionary<string, int>) (arrMem: Dictionary<string, List<int>>) =
  match connections[0] with
  | (CommandEdge command, nextNode) ->
    evalCommand command varMem arrMem
    Some(nextNode)
  | (BoolEdge b, nextNode) ->
    if (evalBool b varMem arrMem) then
      Some(nextNode)
    else
      if connections.Count = 1 then
        None
      else
        evalConnections (connections.GetRange(1, connections.Count - 1)) varMem arrMem

let evalStep (node: node) (varMem: Dictionary<string, int>) (arrMem: Dictionary<string, List<int>>) = evalConnections node.connections varMem arrMem

let getNodeName (nodeNames: List<(string * node)>) (node: node) = fst (nodeNames |> Seq.find (fun (_, fNode) -> fNode = node))

let printVarMem (varMem: Dictionary<string, int>) =
  Regex.Replace(
    Seq.fold (fun acc (KeyValue(key, value)) -> $"{acc}{key}={value}, ") "" varMem,
    ", $",
    "")
let printArr (arr: List<int>) = String.Join("; ", arr)
let printArrMem (arrMem: Dictionary<string, List<int>>) =
  Regex.Replace(
    Seq.fold (fun acc (KeyValue(key, value)) -> $"{acc}{key}=[{printArr value}], ") "" arrMem,
    ", $",
    "")

let printState (status: EvalStatus) node nodeNames varMem arrMem =
  $"Status: {evalStatusToString status}\nNode: {getNodeName nodeNames node}\n{printVarMem varMem}\n{printArrMem arrMem}"

let trimAll (strs: string array) = Array.map (fun (el: string) -> el.Trim()) strs

// 1,2,3,4 - [1,2,3,4]
let readArray (values: string) =
  new List<int>(
    values.Split ";" |>
    Array.map (fun el -> int el))

// Memory is expected to be in the same format as the way you type it into the field on FM4Fun
let readMemory (str: string) (varMem: Dictionary<string, int>) (arrMem: Dictionary<string, List<int>>) =
  str.Split "," |>
  Array.map (fun el -> trimAll (el.Trim().Split "=")) |>
  Array.iter (fun el -> if el[1].[0] = '[' then 
                          arrMem.Add(el[0], readArray (Regex.Replace(el[1], "\[|\]", "")))
                        else
                          varMem.Add(el[0], int el[1]))


let rec stepProgram node endNode cap (nodeNames: List<(string * node)>) (varMem: Dictionary<string, int>) (arrMem: Dictionary<string, List<int>>) =
  if cap = 0 then
    printfn $"{printState Running node nodeNames varMem arrMem}"
    printf "Run additional steps: "
    let addSteps = int (Console.ReadLine())
    stepProgram node endNode addSteps nodeNames varMem arrMem
  else
    let nextNode = evalStep node varMem arrMem
    match nextNode with
    | Some(n) ->
      if n = endNode then
        printfn $"{printState Terminated n nodeNames varMem arrMem}"
      else
        stepProgram n endNode (cap - 1) nodeNames varMem arrMem
    | None -> printfn $"{printState Stuck node nodeNames varMem arrMem}"