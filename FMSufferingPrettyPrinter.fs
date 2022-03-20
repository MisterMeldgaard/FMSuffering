module FMSufferingPrettyPrinter

let rec prettyPrintArith = function
    | Num(x) -> string x
    | GetVariable(a) -> a
    | GetArrayItem(a,x) -> $"{a}[{prettyPrintArith x}]"
    | TimesArithExpr(x,y) -> $"({prettyPrintArith x} * {prettyPrintArith y})"
    | DivArithExpr(x,y) -> $"({prettyPrintArith x} / {prettyPrintArith y})"
    | PlusArithExpr(x,y) -> $"({prettyPrintArith x} + {prettyPrintArith y})"
    | MinusArithExpr(x,y) -> $"({prettyPrintArith x} - {prettyPrintArith y})"
    | PowArithExpr(x,y) -> $"({prettyPrintArith x} ^ {prettyPrintArith y})"
    | UMinusArithExpr(x) -> $"(-{prettyPrintArith x})"

and prettyPrintBool = function
    | True -> "true"
    | False -> "false"
    | StrongAndExpr(x,y) -> $"({prettyPrintBool x} & {prettyPrintBool y})"
    | StrongOrExpr(x,y) -> $"({prettyPrintBool x} | {prettyPrintBool y})"
    | WeakAndExpr(x,y) -> $"({prettyPrintBool x} && {prettyPrintBool y})"
    | WeakOrExpr(x,y) -> $"({prettyPrintBool x} || {prettyPrintBool y})"
    | NotExpr(x) -> $"(!{prettyPrintBool x})"
    | EqualExpr(x,y) -> $"({prettyPrintArith x} = {prettyPrintArith y})"
    | NotEqualExpr(x,y) -> $"({prettyPrintArith x} != {prettyPrintArith y})"
    | GreaterExpr(x,y) -> $"({prettyPrintArith x} > {prettyPrintArith y})"
    | GreaterEqualExpr(x,y) -> $"({prettyPrintArith x} >= {prettyPrintArith y})"
    | LesserExpr(x,y) -> $"({prettyPrintArith x} < {prettyPrintArith y})"
    | LesserEqualExpr(x,y) -> $"({prettyPrintArith x} <= {prettyPrintArith y})"

and prettyPrintGuarded = function
    | Condition(b, c) -> $"{prettyPrintBool b} -> {prettyPrintCommand c}"
    | Choice(gc1, gc2) -> $"{prettyPrintGuarded gc1}\n[] {prettyPrintGuarded gc2}"

and prettyPrintCommand = function
    | Assign(a, b) -> $"{a} := {prettyPrintArith b}"
    | AssignArray(a, b, c) -> $"{a}[{prettyPrintArith b}] := {prettyPrintArith c}"
    | Skip -> "skip"
    | CommandCommand(c1, c2) -> $"{prettyPrintCommand c1};\n{prettyPrintCommand c2}"
    | IfStatement(a) -> $"if {prettyPrintGuarded a}\nfi"
    | DoStatement(a) -> $"do {prettyPrintGuarded a}\nod"