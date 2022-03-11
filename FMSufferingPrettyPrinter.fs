module FMSufferingPrettyPrinter

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
    | StrongAndExpr(x,y) -> sprintf "(%s & %s)" (prettyPrintBool x) (prettyPrintBool y)
    | StrongOrExpr(x,y) -> sprintf "(%s | %s)" (prettyPrintBool x) (prettyPrintBool y)
    | WeakAndExpr(x,y) -> sprintf "(%s && %s)" (prettyPrintBool x) (prettyPrintBool y)
    | WeakOrExpr(x,y) -> sprintf "(%s || %s)" (prettyPrintBool x) (prettyPrintBool y)
    | NotExpr(x) -> sprintf "(!%s)" (prettyPrintBool x)
    | EqualExpr(x,y) -> sprintf "(%s = %s)" (prettyPrintArith x) (prettyPrintArith y)
    | NotEqualExpr(x,y) -> sprintf "(%s != %s)" (prettyPrintArith x) (prettyPrintArith y)
    | GreaterExpr(x,y) -> sprintf "(%s > %s)" (prettyPrintArith x) (prettyPrintArith y)
    | GreaterEqualExpr(x,y) -> sprintf "(%s >= %s)" (prettyPrintArith x) (prettyPrintArith y)
    | LesserExpr(x,y) -> sprintf "(%s < %s)" (prettyPrintArith x) (prettyPrintArith y)
    | LesserEqualExpr(x,y) -> sprintf "(%s <= %s)" (prettyPrintArith x) (prettyPrintArith y)

and prettyPrintGuarded = function
    | Condition(b, c) -> sprintf "%s -> %s" (prettyPrintBool b) (prettyPrintCommand c)
    | Choice (gc1, gc2) -> sprintf "%s\n[] %s" (prettyPrintGuarded gc1) (prettyPrintGuarded gc2)

and prettyPrintCommand = function
    | Assign(a, b) -> sprintf "%s := %s" a (prettyPrintArith b)
    | AssignArray(a, b, c) -> sprintf "%s[%s] := %s" a (prettyPrintArith b) (prettyPrintArith c)
    | Skip -> "skip"
    | CommandCommand(c1, c2) -> sprintf "%s;\n%s" (prettyPrintCommand c1) (prettyPrintCommand c2)
    | IfStatement(a) -> sprintf "if %s\nfi" (prettyPrintGuarded a)
    | DoStatement(a) -> sprintf "do %s\nod" (prettyPrintGuarded a)