module FMSufferingProgramGraph

let dotPrefix = "digraph program_graph {rankdir=LR;
node [shape = circle]; q▷;
node [shape = doublecircle]; q◀; 
node [shape = circle]\n"

type node = { connections: List<(edge * node)> }
and edge = CommandEdge of command | BoolEdge of boolExpr

let rec programGraphCommand (startNode: node) (endNode: node) (deterministic: bool) (loopNodes: List<node>) = function
    | Assign(a, b) -> startNode.connections.Add(CommandEdge(Assign(a, b)), endNode)
    | AssignArray(a, b, c) -> startNode.connections.Add(CommandEdge(AssignArray(a, b, c)), endNode)
    | Skip -> startNode.connections.Add(CommandEdge(Skip), endNode)
    | CommandCommand(c1, c2) -> let fresh = { connections = new List<(edge * node)>() }
                                programGraphCommand startNode fresh deterministic loopNodes c1
                                programGraphCommand fresh endNode deterministic loopNodes c2
    | IfStatement(a) -> if deterministic then
                          programGraphGuardedDeterministic startNode endNode False a loopNodes |> ignore
                        else
                          programGraphGuarded startNode endNode loopNodes a
    | DoStatement(a) -> loopNodes.Add(startNode)
                        if deterministic then
                          let d = programGraphGuardedDeterministic startNode startNode False a loopNodes
                          startNode.connections.Add(BoolEdge(NotExpr(d)), endNode)
                        else
                          programGraphGuarded startNode startNode loopNodes a
                          startNode.connections.Add(BoolEdge(pgDone a), endNode)

and programGraphGuarded (startNode: node) (endNode: node) (loopNodes: List<node>) = function
    | Condition(b, c) -> let fresh = { connections = new List<(edge * node)>() }
                         programGraphCommand fresh endNode false loopNodes c
                         startNode.connections.Add(BoolEdge(b), fresh)
    | Choice(gc1, gc2) -> programGraphGuarded startNode endNode loopNodes gc1
                          programGraphGuarded startNode endNode loopNodes gc2

and programGraphGuardedDeterministic (startNode: node) (endNode: node) (d: boolExpr) gc (loopNodes: List<node>) : boolExpr =
  match gc with
  | Condition(b, c) -> let fresh = { connections = new List<(edge * node)>() }
                       programGraphCommand fresh endNode true loopNodes c
                       startNode.connections.Add(BoolEdge(StrongAndExpr(b, NotExpr(d))), fresh)
                       StrongOrExpr(b, d)
  | Choice(gc1, gc2) -> let d1 = programGraphGuardedDeterministic startNode endNode d gc1 loopNodes
                        let d2 = programGraphGuardedDeterministic startNode endNode d1 gc2 loopNodes
                        d2

and pgDone = function
  | Condition(b, c) -> NotExpr b    
  | Choice(gc1, gc2) -> StrongAndExpr (pgDone gc1, pgDone gc2)

let prettyPrintEdge = function
  | CommandEdge(c) -> prettyPrintCommand c
  | BoolEdge(b) -> prettyPrintBool b

let rec visitNode acc (curEdge, curNode) node (visitedNodes: List<(string * node)>) = 
  let startName = fst (visitedNodes |> Seq.find (fun (_, fNode) -> fNode = node))
  match (visitedNodes |> Seq.tryFind (fun (_, fNode) -> fNode = curNode)) with
      | Some (curName, _) -> $"{acc}{startName} -> {curName} [label = \"{prettyPrintEdge curEdge}\"];\n"
      | None -> let curName = $"q{(Seq.length visitedNodes) - 1}"
                visitedNodes.Add(curName, curNode)
                $"{acc}{startName} -> {curName} [label = \"{prettyPrintEdge curEdge}\"];\n{pg2dotRec visitedNodes curNode}"

and pg2dotRec (visitedNodes: 
  List<(string * node)>) node = node.connections |> Seq.fold (fun acc el -> visitNode acc el node visitedNodes) ""

let pg2dot (visitedNodes: List<(string * node)>) node = $"{dotPrefix}{pg2dotRec visitedNodes node}}}"