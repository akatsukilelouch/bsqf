module Bsqf.Parser
open Bsqf.Config
open Bsqf.Ast
open Bsqf.Lexer

type GenSym = {
    Monotonic: int
    Table: Map<string, int>
}

let newGenSymName (gensym: GenSym byref) name =
    let { Monotonic = m; Table = t } = gensym

    gensym <- {
        Monotonic = m + 1
        Table = t.Add(name, m)
    }

    m

type Function = {
    Name: Identifier;
    Args: string list;
    Variables: string list;

    mutable Generated: Ast;
};

type Global = {
    Name: Identifier;
    Receiver: bool;
};

type Compilation = {
    mutable Monotonic: int;
    mutable Seed: int;
    mutable IdentifierCache: Map<string, int>;
    mutable Functions: Function list;

    mutable ImportedFunctions: Global list;
};

type External = {
    Functions: Global list;
    Operators: Global list;
};

type Project = {
    Functions: Function list;
    External: External;
};

type Context = {
    mutable Compilation: Compilation;
    Project: Project;
};

let isAlreadyDefinedFunction (context: Context) name =
    Seq.append context. context.Compilation.ImportedFunctions |> Seq.exists name.Equals

let isOverridingOperator (context: Context) name =
    let matches x =
        x.Name.Equals name

    context.Project.ExternalOperators |> List.exists (fun operator -> name.Equals operator.Name)

let issueWarning str =
    printf "warning: %s" str


let issueError str =


let processImport (context: Context ref) (import: string) =
    let userFunction = context.Value.Project.Functions |> Seq.tryFind import.Equals

    if userFunction.IsNone then
        if Seq.append context.Value.Project.External.Functions context.Value.Project.External.Operators |> Seq.tryFind (fun x -> x.Name.Equals import) then
            issueWarning "no need to import external functions"
        else
    else
        true
    

    ()

let defaultProject = {
    Functions = [];

    ExternalOperators = [];
    ExternalFunctions = [];
}

let random = new System.Random()

let defaultContext project = {
    Compilation = {
        Monotonic = 0;
        Seed = random.Next();
        IdentifierCache = Map [];

        Functions = [];

        ImportedFunctions = [];
        GlobalOperators = [];
    };

    Project = project;
}

let parseAtom (value: string) =
    if value.StartsWith '.' &&
        value.StartsWith '0' &&
        value.StartsWith '1' &&
        value.StartsWith '2' &&
        value.StartsWith '3' &&
        value.StartsWith '4' &&
        value.StartsWith '5' &&
        value.StartsWith '6' &&
        value.StartsWith '7' &&
        value.StartsWith '8' &&
        value.StartsWith '9' then
        Number value
    else
        Identifier value

let updateFunctionBody (func: Function) newStatement =
    { func with Generated = newStatement :: func.Generated }

let isBuiltinAtom (atom: string) = atom.StartsWith ":"

let contextHasFuncLocal context name =
    context.Compilation.ImportedFunctions |> Seq.exists (fun func -> func.Name.Equals name)

let contextHasFuncGlobal context name =
    context.Project.ExternalFunctions |> Seq.exists (fun func -> func.Name.Equals name)

let contextHasOperatorLocal context name =
    context.Compilation.GlobalOperators |> Seq.exists (fun func -> func.Name.Equals name)

let contextHasOperatorGlobal context name =
    context.Project.ExternalOperators |> Seq.exists (fun func -> func.Name.Equals name)

// beware: will throw an exception if not found
let contextOperatorHasReceiver context name =
    let functions = Seq.append context.Compilation.GlobalOperators context.Project.ExternalOperators
    let func = functions |> Seq.find (fun func -> func.Name.Equals name)
    func.Receiver

let updateMonotonicForAName compilation name =
    if compilation.IdentifierCache.ContainsKey name then
        compilation, compilation.IdentifierCache[name]
    else
        let cache = compilation.IdentifierCache.Add (name, compilation.Monotonic)

        { compilation with Monotonic = compilation.Monotonic + 1; IdentifierCache = cache }, compilation.Monotonic

let getMonotonicName compilation name =
    let compilation, id = updateMonotonicForAName compilation name

    compilation, sprintf "_%d" id

exception InvalidCallExpression of string

exception InvalidArgsException of string

let parseArgs (expr: SExpr list) =
    let matchArg expr =
        match expr with
        | List (Atom name :: Atom type_ :: []) -> Typed (name, type_)
        | List (Atom name :: []) -> Untyped name
        | _ -> raise (InvalidArgsException "args are invalidly set up")

    List.map matchArg expr

let rec parseBuiltin (context: Context) (func: Function) (expr: SExpr list) =
    match expr with
        | Atom ":set" :: Atom name :: List expr :: [] ->
            let compilation, name = getMonotonicName context.Compilation name
            let context, func, expr = parseExpression { context with Compilation = compilation } func expr
            context, func, Expression <| Binary ("=", Identifier name, expr)
        | Atom ":define" :: Atom name :: List args :: List expr :: [] ->
            let compilation, name = getMonotonicName context.Compilation name

            let compilation, innerFunc = parseStatementsIntoFunction { context with Compilation = compilation } { Name = name; Args = parseArgs args; Generated = [] } expr

            { context with Compilation = { compilation with Functions = innerFunc :: compilation.Functions } }, func,
                Expression <| Identifier name
        | Atom ":lambda" :: List args :: List expr :: [] ->
            let name = sprintf "lambda%d%d" context.Compilation.Seed context.Compilation.Monotonic

            let compilation, innerFunc = parseStatementsIntoFunction { context with Compilation = { context.Compilation with Monotonic = context.Compilation.Monotonic + 1; }; } { Name = name; Args = parseArgs args; Generated = [] } expr

            { context with Compilation = { compilation with Functions = innerFunc :: compilation.Functions } }, func,
                Expression <| Identifier name
and parseExpression (context: Context) (func: Function) (expr: SExpr list) =
    match expr with
        | Atom identifier :: args when contextHasFuncLocal context identifier ->
            let compilation, name = getMonotonicName context.Compilation identifier

            let context, func, expr = parseExpression { context with Compilation = compilation } func args

            context, func, Binary ("call", expr, Identifier name)
        | Atom name :: args when contextHasFuncGlobal context name ->
            let context, func, expr = parseExpression context func args

            context, func, Binary ("call", expr, Identifier name)
        | Atom identifier :: args when contextHasOperatorLocal context identifier ->
            let compilation, name = getMonotonicName context.Compilation identifier

            let mutable context = { context with Compilation = compilation }
            let mutable func = func

            let receiver, args =
                if contextOperatorHasReceiver context identifier then
                    if args.IsEmpty then
                        Some <| Identifier "objNull", AstExpression.List <| AstList.List []
                    else
                        match Seq.tryHead args with
                            | Some first ->
                                let newContext, newFunc,  first = parseExpression context func [first]
                                context <- newContext
                                func <- newFunc

                                let newContext, newFunc, rest = parseExpression newContext func (List.skip 1 args)
                                context <- newContext
                                func <- newFunc

                                Some first, rest
                            | None -> None, AstExpression.List <| AstList.List []
                else
                    let newContext, newFunc, expression = parseExpression context func args
                    context <- newContext
                    func <- newFunc

                    None, expression
                
        
            context, func,
                match receiver with
                | Some receiver -> Binary (name, receiver, args)
                | None -> Unary (name, args)
        | Atom name :: args when contextHasOperatorGlobal context name ->
            let mutable context = context
            let mutable func = func

            let receiver, args =
                if contextOperatorHasReceiver context name then
                    if args.IsEmpty then
                        Some <| Identifier "objNull", AstExpression.List <| AstList.List []
                    else
                        match Seq.tryHead args with
                            | Some first ->
                                let newContext, newFunc,  first = parseExpression context func [first]
                                context <- newContext
                                func <- newFunc

                                let newContext, newFunc, rest = parseExpression newContext func (List.skip 1 args)
                                context <- newContext
                                func <- newFunc

                                Some first, rest
                            | None -> None, AstExpression.List <| AstList.List []
                else
                    let newContext, newFunc, expression = parseExpression context func args
                    context <- newContext
                    func <- newFunc

                    None, expression
                
        
            context, func,
                match receiver with
                | Some receiver -> Binary (name, receiver, args)
                | None -> Unary (name, args)
and parseStatement (context: Context) (func: Function) (expr: SExpr) =
    match expr with
        | List ((Atom name) :: rest) when isBuiltinAtom name ->
            parseBuiltin context func rest
        | Atom value
        | Quote (Atom value) ->
            context, func, AstStatement.Expression (Identifier value)
        | List exprs ->
            let context, func, expr = parseExpression context func exprs

            context, func, Expression <| expr
        | Quote (List exprs) ->
            let context, func, expr = parseExpression context func exprs

            context, func, Expression <| expr
and parseStatementsIntoFunction (context: Context) (func: Function) (expr: SExpr list) =
    match expr with
        | [] -> context.Compilation, func
        | statement :: rest ->
            let context, func, expr = parseStatement context func statement
            parseStatementsIntoFunction context { func with Generated = expr :: func.Generated } rest

exception InvalidArgumentsException of string

exception AlreadyDefinedException of string

exception ShouldBeFunctionEcxception of string

let parseBody (name: string) =
    parseStatementsIntoFunction

let processDefinition (context: Context ref) (expr: SExpr list) =
    match expr with
        | Atom identifier :: List arguments :: List statements :: [] ->
            if Seq.append context.Project.Functions context.Compilation.Functions |> Seq.exists (fun func -> func.Name.Equals identifier) then
                raise (AlreadyDefinedException (sprintf "function %s is already defined" identifier))
            else
                let compilation, func = parseBody identifier context { Name = identifier; Args = parseArgs arguments; Generated = []; } statements

                { context with Compilation = { compilation with Functions = func :: context.Compilation.Functions } }

let rec parseTopLevel (context: Context ref) (expr: SExpr list) =
    match expr with
        | [] -> ()
        | item :: rest ->
            match item with
                | List (Atom "#import" :: Atom name :: []) -> 
                    processImport context name
                | List (Atom "#define" :: Atom name :: body) ->
                    processDefinition context expr
                    
        | [] -> context
            parseTopLevel context rest

    parseTopLevel context rest

let parse (expr : SExpr list) =
    let context = ref <| defaultContext defaultProject

    parseTopLevel context expr
