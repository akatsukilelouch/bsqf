module Bsqf.Parser

open Bsqf.Lexer

// everything that we need to do is to compile a specific set of sexprs to sqf
// all these sepxrs have one specific form

type DeclarationKind =
    | DeclarationPrivate
    | DeclarationParams

type SqfExpression =
    | StringLiteralExpression of literal: string
    | NumberLiteralExpression of literal: string
    | VariableExpression of identifier: string
    | ArrayExpression of items: SqfExpression list
    | GroupingExpression of inner: SqfExpression
    | BinaryOperationExpression of left: SqfExpression * right: SqfExpression * operator: string
    | UnaryOperationExpression of right: SqfExpression * operator: string
    | BlockExpression of statements: SqfStatement list
and SqfStatement =
    | AssignmentStatement of identifier: string * expression: SqfExpression
    | IfStatement of block: SqfExpression * then_: SqfExpression * else_: SqfExpression option
    | WhileStatement of block: SqfExpression * code: SqfExpression
    | ForStatement of identifier: string * from: SqfExpression option * to_: SqfExpression option * block: SqfExpression
    | DeclarationStatement of kind: string * array: SqfExpression
    | ExpressionStatement of expression: SqfExpression
    | BreakStatement of with_: SqfExpression option
    | ContinueStatement of with_: SqfExpression option
    | ExitStatement of with_: SqfExpression option

let escape =
    let map c =
        match c with
            | '"' -> "\\\""
            | c -> string c
        
    String.collect map

let ident level =
    String.replicate level " "

let nextIdent =
    Option.map <| fun level -> level + 1

let appendIdent level value =
    match level with
        | Some level -> sprintf "%s%s" (ident level) value
        | None -> value

let rec minimizeTreeLocalChildren expression =
    match expression with
        | BlockExpression [ExpressionStatement expression] -> expression
        | BlockExpression statements ->
            let unnest statements statement =
                match statement with
                    | ExpressionStatement (BlockExpression inner) -> List.concat [inner; statements]
                    | statement -> statement :: statements

            BlockExpression (statements |> Seq.rev |> Seq.fold unnest [])

        | expression -> expression

let indentifierToSqfIdentifier = sprintf "_%s"

let rec exprToSqf level expression =
    let sublevel = nextIdent level

    match expression with
        | StringLiteralExpression literal -> sprintf "\"%s\"" <| escape literal
        | NumberLiteralExpression literal -> literal
        | VariableExpression identifier -> identifier
        | ArrayExpression items -> sprintf "[%s]" (items |> List.map (exprToSqf sublevel) |> String.concat ",")
        | GroupingExpression expr -> sprintf "(%s)" <| exprToSqf sublevel expr
        | BinaryOperationExpression (left, right, operator) -> sprintf "%s %s %s" (exprToSqf sublevel left) operator (exprToSqf sublevel right)
        | UnaryOperationExpression (right, operator) -> sprintf "%s %s" operator (exprToSqf sublevel right)
        | BlockExpression block -> 
            if block.Length <= 1 then
                match List.tryHead block with
                    | Some head -> sprintf "{ %s }" (statementToSqf None head)
                    | None -> "{ }"
            else
                let print (items: string list) = 
                    sprintf "{\n%s;\n%s}" (items |> String.concat ";\n") <|
                        match level with
                            | Some level -> ident level
                            | None -> ""

                print (block |> List.map (statementToSqf sublevel))

and statementToSqf (level: int option) (statement : SqfStatement) : string =
    let sublevel = nextIdent level

    appendIdent level <|
        match statement with
            | ExpressionStatement expression -> exprToSqf sublevel expression
            | AssignmentStatement (identifier, expression) ->
                sprintf "%s = %s" identifier (exprToSqf sublevel expression)
            | IfStatement (block, then_, else_) ->
                match else_ with
                    | Some else_ ->
                        sprintf "if %s then %s else %s" (exprToSqf sublevel block) (exprToSqf sublevel then_) (exprToSqf sublevel else_)
                    | None ->
                        sprintf "if %s then %s" (exprToSqf sublevel block) (exprToSqf sublevel then_)
            | WhileStatement (block, code) ->
                sprintf "while %s do %s" (exprToSqf sublevel block) (exprToSqf sublevel block)
            | ForStatement (identifier, from_, to_, code) ->
                let orEmpty x =
                    match x with | Some x -> x | None -> ""

                sprintf "for \"%s\" from %s to %s do %s"
                    <| identifier
                    <| (from_ |> Option.map (exprToSqf sublevel) |> orEmpty)
                    <| (to_ |> Option.map (exprToSqf sublevel) |> orEmpty)
                    <| exprToSqf sublevel code
            | DeclarationStatement (kind, array) ->
                sprintf "%s %s" <| kind <| exprToSqf sublevel array
            | BreakStatement with_ ->
                match with_ with
                    | Some block -> sprintf "breakWith %s" <| exprToSqf sublevel block
                    | None -> "break"
            | ContinueStatement with_ ->
                match with_ with
                    | Some block -> sprintf "continueWith %s" <| exprToSqf sublevel block
                    | None -> "continue"
            | ExitStatement with_ ->
                match with_ with
                    | Some block -> sprintf "exitWith %s" <| exprToSqf sublevel block
                    | None -> "exit"

type Context =
    { Parent: Context option;
    Variables: string list ref;
    Functions: string list ref;
    Gensym: int ref; }

    member this.Fork() =
        { this with Variables = ref []; Functions = ref []; Parent = Some this }

    member this.WrapSym(identifier: string) =
        let value = this.Gensym.Value

        this.Gensym.Value <- value + 1

        sprintf "_%s%d" identifier value

    member this.ExistsFunction (name: string) =
        this.Functions.Value |> List.exists name.Equals ||
            match this.Parent with
                | Some parent -> parent.ExistsFunction name
                | None -> false

    member this.ExistsVariableLocal (name: string) =
        this.Variables.Value |> List.exists name.Equals

    member this.ExistsVariableGlobal (name: string) =
        match this.Parent with
            | Some parent -> parent.ExistsFunction name
            | None -> this.ExistsVariableLocal name

let defaultContext () = { Context.Parent = None; Variables = ref []; Functions = ref []; Gensym = ref 0 }
        
exception MalformedQuotingException of string

exception MalformedBuiltinException of string

exception MalformedCodegenException of string

let rec createFunctionCall (context: Context) (expr: SExpr list) param =
    match expr with
        | Atom identifier :: args ->
            BinaryOperationExpression (
                ArrayExpression (args |> List.map (convert context)),
                VariableExpression identifier,
                param
            )
        | _ -> raise (MalformedCodegenException "could not create function")

and convertBuiltin (context: Context) (expr: SExpr list) =
    let convertNestedFunction = convert // preserve the default one
    let convert expr = convert context expr

    match expr with
        | Atom ":set" :: items ->
            match items with
                | Atom identifier :: expr :: [] ->
                    if not (context.ExistsVariableGlobal identifier) then
                        context.Variables.Value <- identifier :: context.Variables.Value

                    ExpressionStatement <| BlockExpression [
                        DeclarationStatement ("private", ArrayExpression [StringLiteralExpression identifier]);
                        AssignmentStatement (identifier, convert expr)
                    ]
                | _ -> raise (MalformedBuiltinException (sprintf "set should be of (!set name value) but is %s" (items.ToString())))
        | Atom ":global" :: Atom identifier :: Literal actualIdentifier :: [] ->
            context.Functions.Value <- identifier :: context.Functions.Value
            
            ExpressionStatement <| BlockExpression [
                DeclarationStatement ("private", ArrayExpression [StringLiteralExpression identifier]);
                AssignmentStatement (identifier, VariableExpression actualIdentifier)
            ]
        | Atom ":define" :: items ->
            match items with
                | Atom identifier :: List args :: body :: [] ->
                    let args = args |> List.fold (fun a x -> match a with | None -> None | Some a -> match x with | Atom identifier -> Some (identifier :: a) | _ -> None) (Some [])

                    match args with
                        | Some args ->
                            let definition = DeclarationStatement ("private", ArrayExpression (args |> List.map StringLiteralExpression))

                            let body =
                                match convertNestedFunction (context.Fork()) body with 
                                    | BlockExpression expression -> definition :: expression
                                    | anything -> [definition; ExpressionStatement anything]

                            context.Functions.Value <- identifier :: context.Functions.Value

                            AssignmentStatement (identifier, BlockExpression body)
                        | None -> raise (MalformedBuiltinException (sprintf "def should be of (!def name var-list code) but is %s" (items.ToString())))
                | _ -> raise (MalformedBuiltinException (sprintf "def should be of (!def name var-list code) but is %s" (items.ToString())))
        | Atom ":if" :: items ->
            match items with
                | code :: then_ :: else_ :: [] -> IfStatement (convert code, convert then_, Some <| convert else_)
                | code :: then_ :: [] -> IfStatement (convert code, convert then_, None)
                | _ -> raise (MalformedBuiltinException "!if should be of (!if condition-code true-code false-code)")
        | Atom ":for" :: items ->
            match items with
                | Atom identifier :: List (a :: a0 :: []) :: List (b :: b0 :: []) :: code :: [] ->
                    let reorder a b =
                        match a, b with | Atom "from", Atom "to" -> a0, b0 | _ -> b0, a0

                    let from_, to_ = reorder a b
                    ForStatement (identifier, Some <| convert from_, Some <| convert to_, convert code)
                | Atom identifier :: List (a :: a0 :: []) :: code :: [] ->
                    let from_, to_ =
                        match a with
                            | Atom "from" -> Some <| convert a0, None
                            | Atom "to" -> None, Some <| convert a0

                    ForStatement (identifier, from_, to_, convert code)

                | _ -> raise (MalformedBuiltinException "!for should be of (!for condition-code (from expr) (to expr) code)")
        | Atom ":while" :: items ->
            match items with
                | code :: then_ :: [] -> WhileStatement (convert code, convert then_)
                | _ -> raise (MalformedBuiltinException "!while should be of (!while condition-code code)")
        | Atom ":break" :: items ->
            match items with
                | code :: [] -> BreakStatement (Some (convert code))
                | [] -> BreakStatement None
                | _ -> raise (MalformedBuiltinException "break should be of (!break optional-code value)")
        | Atom ":continue" :: items ->
            match items with
                | code :: [] -> ContinueStatement (Some (convert code))
                | [] -> ContinueStatement None
                | _ -> raise (MalformedBuiltinException "continue should be of (!continue optional-code value)")
        | Atom ":exit" :: items ->
            match items with
                | code :: [] -> ExitStatement (Some (convert code))
                | [] -> ExitStatement None
                | _ -> raise (MalformedBuiltinException "exit should be of (!exit optional-code value)")
        | Atom ":spawn" :: items -> ExpressionStatement <| createFunctionCall context items "spawn"
        | Atom identifier :: _ -> raise (MalformedBuiltinException (sprintf "invalid builtin %s" identifier))
        | _ -> raise (MalformedBuiltinException "invalid builtin construction")

and convert (context: Context) (expr: SExpr) =
    let convertBuiltin = convertBuiltin context
    let convert = convert context

    minimizeTreeLocalChildren <|
    match expr with
        | List (Atom identifier :: _ as builtin) when identifier.StartsWith ":" -> BlockExpression <| [convertBuiltin builtin]
        | List (Atom identifier :: _ as self) when context.ExistsFunction identifier -> createFunctionCall context self "call"
        // | List list -> BlockExpression <| (list |> List.fold (fun b x -> convert x |> convertBlockExpressionFolder b) [])
        | List [] | Atom "nil" -> VariableExpression "objNull"
        | List list -> BlockExpression <| List.map (fun x -> ExpressionStatement <| convert x) list
        | Quote (List list) -> ArrayExpression (list |> List.map convert)
        | Quote (Atom _) -> raise (MalformedQuotingException "quoting an atom does nothing")
        | Literal literal -> StringLiteralExpression literal
        | Atom atom ->
            if atom.StartsWith '.' &&
                atom.StartsWith '0' &&
                atom.StartsWith '1' &&
                atom.StartsWith '2' &&
                atom.StartsWith '3' &&
                atom.StartsWith '4' &&
                atom.StartsWith '5' &&
                atom.StartsWith '6' &&
                atom.StartsWith '7' &&
                atom.StartsWith '8' &&
                atom.StartsWith '9' then
                    NumberLiteralExpression atom
                else
                    VariableExpression atom

let public compile expr =
    let expression = convert (defaultContext()) expr

    exprToSqf (Some 0) expression
