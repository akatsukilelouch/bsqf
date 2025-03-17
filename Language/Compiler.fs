module Bsqf.Compiler
open System.IO
open Bsqf.Ast
open FParsec

type Declaration = AstDeclare

type Definition = {
    Name: IdentifierOrRawValue;
    Arguments: IdentifierOrRawValue list;
    Code: AstTree;
}

type ResolvedImport =
    | Definition of Definition
    | Declaration of Declaration

type Callable =
    | CustomFunction
    | UnaryOperator
    | BinaryOperator

type Module =
    {
        Name: string;

        Imports: ResolvedImport list;
        Declarations: Declaration list;
        Definitions: Definition list;
    }

    member this.GetImport x =
        this.Imports |> List.tryFind
            (fun y ->
                match y with 
                    | ResolvedImport.Declaration e -> let (_, name) = e.Name in name.Equals x
                    | ResolvedImport.Definition e -> let (_, name) = e.Name in name.Equals x)

    member this.GetDeclaration x =
        this.Declarations |> List.tryFind
            (fun y -> let (_, name) = y.Name in name.Equals x)
    
    member this.GetDefinition x =
        this.Definitions |> List.tryFind
            (fun y -> let (_, name) = y.Name in name.Equals x)

    member this.GetCallable x =
        this.GetImport x |> Option.map (
            fun import -> 
                match import with
                    | ResolvedImport.Declaration (Declaration.Operator (_, receiver)) ->
                        if receiver then
                            BinaryOperator
                        else
                            UnaryOperator
                    | ResolvedImport.Declaration (Declaration.Function _)
                    | Declaration (Declaration.Function _)
                    | ResolvedImport.Definition _ ->
                        CustomFunction)


exception CompilationError of position: Position * message: string
exception UnsourcedCompilationError of message: string

let fail position message =
    raise <| CompilationError (position, message)

let fail' message =
    raise <| UnsourcedCompilationError message

let chooseAstDeclareToDeclaration ((position, declare): AstTopLevel) : Declaration option =
    match declare with
        | Declare inner -> Some inner
        | _ -> None

let resolveImports (modules: Module list) ((position, import): AstTopLevel): ResolvedImport list option =
    let rec resolveAllImports imports (module_: Module) (items: IdentifierOrRawValue list) =
        match items with
            | [] ->
                imports
            | (position, item) :: rest ->
                let declaration = module_.Declarations |> List.tryFind (fun x -> let _, name = x.Name in name.Equals item)
                
                let import: ResolvedImport option =
                    match declaration with
                        | None -> module_.Definitions |> List.tryFind (fun x -> let _, name = x.Name in name.Equals item) |> Option.map Definition
                        | Some self -> Some <| Declaration self

                match import with
                    | Some import ->
                        resolveAllImports (import :: imports) module_ rest
                    | None ->
                        fail position <| sprintf "no name found in module %s" module_.Name

    match import with
        | Import ((position, name), items) ->
            Some <|
                match modules |> List.tryFind (fun x -> x.Name.Equals name) with
                    | Some module_ ->
                        match items with
                            // resolve all items
                            | [] ->
                                List.concat [
                                    module_.Declarations |> List.map Declaration;
                                    module_.Definitions |> List.map Definition;
                                ]
                            | items ->
                                resolveAllImports [] module_ items
                    | None ->
                        fail position "no module found"
        | _ ->
            None

let chooseModuleDefinitions (modules: Module list) (topLevel: Bsqf.Ast.AstTopLevel) =
    match topLevel with
        | _, Module (position, name) ->
            if modules |> List.exists (fun x -> x.Name.Equals name) then
                fail position "module already exists"
            else
                Some (position, name)
        | _ ->
            None

let chooseDefinitions (modules: Module list) (topLevel: Bsqf.Ast.AstTopLevel) =
    let prepareDefinition (identifier: IdentifierOrRawValue) (args: IdentifierOrRawValue list) (tree: AstTree) =
        {
            Name = identifier;
            Definition.Arguments = args;
            Code = tree;
        }

    match topLevel with
        | _, AstTopLevelValue.Definition (identifier, args, tree) ->
            Some <| prepareDefinition identifier args tree
        | _ ->
            None

let resolveModule (modules: Module list) (topLevel: Bsqf.Ast.AstTopLevel list) =
    let topLevelPosition =
        match topLevel with
            | (position, _) :: _ -> position
            | [] -> fail' "empty module"

    // this is made through list filters to minimize code structure although this is not as fast as manual
    // tree descend with local conditioning

    let (_, name) =
        match topLevel |> List.choose (chooseModuleDefinitions modules) with
            | [] -> fail topLevelPosition "there must be a module definition"
            | identifier :: [] -> identifier
            | _ -> fail topLevelPosition "there must be only one module definition"

    let declarations = topLevel |> List.choose chooseAstDeclareToDeclaration

    let imports = topLevel |> List.choose (resolveImports modules) |> List.concat

    let definitions = topLevel |> List.choose (chooseDefinitions modules)

    {
        Module.Name = name;
        Declarations = declarations;
        Imports = imports;
        Definitions = definitions;
    }

type Context = {
    Variables: IdentifierOrRawValue list;
    GenSymMonotonic: int ref;
};

let rec private util definition module_ into =
    (fun x -> compileStatements x definition module_ into), (fun x -> compileExpression x definition module_ into), fprintf into, fprintf into "%s"
and compileStatements (context: Context) (definition: Definition) (module_: Module) (into: TextWriter) (statements: AstStatement list) =
    let stmts, expr, printf', print' = util definition module_ into

    match statements with
        | [] -> ()
        | (upperPosition, statement) :: rest ->
            // we need to update the context just once if we set a variable, otherwise
            // the context is not changed at all
            let mutable context = context

            match statement with
                | AstStatementValue.Expression e -> expr context e
                | AstStatementValue.AnonymousSet (name, e) ->
                    printf' "%s = " name
                    expr context e
                    print' ";"

                    context <- { context with Variables = (upperPosition, name) :: context.Variables }
                | AstStatementValue.Set ((position, name), e) ->
                    if (module_.GetCallable name).IsSome then
                        fail position "setting the variable shadows an external function"

                    printf' "%s = " name
                    expr context e
                    print' ";"

                    context <- { context with Variables = (position, name) :: context.Variables }
                | AstStatementValue.For ((position, name), from_, to_, step_, code) ->
                    if (module_.GetCallable name).IsSome then
                        fail position "setting the binding shadows an external function"

                    if context.Variables |> List.exists (fun x -> let (_, name) = x in x.Equals name) then
                        fail position "binding shadows the external variable"

                    printf' "for \"%s\"" name
                    let p as_ x = 
                        match x with
                            | Some (_, x) -> fprintf into " %s %s " as_ x
                            | None -> ()

                    p "from" from_
                    p "to" to_
                    p "step" step_
                    print' " { "
                    stmts { context with Variables = (position, name) :: context.Variables } code
                    print' " };"
                | AstStatementValue.While (condition, code) ->
                    print' "while "
                    // TODO: transform blocks
                    expr context condition
                    print' " do {"
                    stmts context code
                    print' " };"
                | AstStatementValue.Exit with_ ->
                    fprintf into "exit"
                    match with_ with
                        | Some x ->
                            fprintf into " with "
                            expr context x
                        | _ -> ()
                    fprintf into ";"
                | AstStatementValue.Continue with_ ->
                    fprintf into "continue"
                    match with_ with
                        | Some x ->
                            fprintf into " with "
                            expr context x
                        | _ -> ()
                    fprintf into ";"
                | AstStatementValue.Break with_ ->
                    fprintf into "break"
                    match with_ with
                        | Some x ->
                            fprintf into " with "
                            expr context x
                        | _ -> ()
                    fprintf into ";"

            compileStatements context definition module_ into rest

and compileExpression (context: Context) (definition: Definition) (module_: Module) (into: TextWriter) ((upperPosition, expression): AstExpression) =
    let stmts, expr, printf', print' = util definition module_ into

    let compileList (args: AstExpression list) =
        print' "["
        for arg in args do    
            expr context arg
            print' ","
        print' "]"

    match expression with
        | AstExpressionValue.Block code ->
            print' "{ "
            stmts context code
            print' " }"

        | AstExpressionValue.Identifier (position, name) ->
            if not (context.Variables |> List.exists (fun x -> let (_, name) = x in x.Equals name)) && (module_.GetCallable name).IsNone then
                fail position "variable or callable not defined"

            print' name

        | AstExpressionValue.AnonymousIdentifier nameOrLiteral
        | AstExpressionValue.Number nameOrLiteral ->
            print' nameOrLiteral

        | AstExpressionValue.If (condition, then_, else_) ->
            // let conditionSet, conditionVariable =
            //     let index = context.GenSymMonotonic.Value
            //     context.GenSymMonotonic.Value <- context.GenSymMonotonic.Value + 1

            //     let temporaryName = sprintf "_%d" index

            //     let temporarySet =
            //         temporaryPosition, AstStatementValue.AnonymousSet (temporaryName, (temporaryPosition, condition))

            //     let temporaryVariable =
            //         temporaryPosition, AstExpressionValue.Identifier (temporaryPosition, temporaryName)

            //     temporarySet, temporaryVariable

            // let resultSet, resultVariable =
            //     let index = context.GenSymMonotonic.Value
            //     context.GenSymMonotonic.Value <- context.GenSymMonotonic.Value + 1

            //     let temporaryName = sprintf "_%d" index

            //     let temporarySet =
            //         temporaryPosition, AstStatementValue.AnonymousSet (temporaryName, (temporaryPosition, condition))

            //     let temporaryVariable =
            //         temporaryPosition, AstExpressionValue.Identifier (temporaryPosition, temporaryName)

            //     temporarySet, temporaryVariable

            print' "if ("
            match condition with
            | _, Identifier (_, identifier) ->
                print' identifier
            | otherwise ->
                expr context otherwise
            print' ") then "

            match then_ with
                | _, Block stmts as self ->
                    expr context self
                | otherwise ->
                    print' "{ "
                    expr context otherwise
                    print' "; }"

            match else_ with
                | Some (_, Block stmts as self) ->
                    print' " else "
                    expr context self
                | Some otherwise ->
                    print' " else { "
                    expr context otherwise
                    print' "; }"
                | None -> ()

        | AstExpressionValue.String literal ->
            printf' "\"%s\"" literal

        | AstExpressionValue.List list ->
            compileList list

        | AstExpressionValue.ForEach ((bindingPosition, name), list, code) ->
            let codeToCompileRest =
                match code with
                    | _, AstExpressionValue.Block stmts -> stmts
                    | position, expr -> [
                        position, AstStatementValue.Expression (position, expr)
                    ]
            
            let variableBinding =
                bindingPosition, AstStatementValue.Set ((bindingPosition, name), (bindingPosition, AstExpressionValue.AnonymousIdentifier "_x"))

            stmts { context with Variables = (bindingPosition, name) :: context.Variables } (variableBinding :: codeToCompileRest)

        | AstExpressionValue.Call ((position, name), args) ->
            if context.Variables |> List.exists (fun x -> let (_, name) = x in x.Equals name) then
                fail position "calling local variables is not yet allowed"
            
            let callable =
                match module_.GetCallable name with
                    | Some callable -> callable
                    | None ->
                        fail position "undefined callable"

            match callable with
                | CustomFunction ->
                    compileList args
                    printf' " call %s;" name
                | BinaryOperator -> 
                    let receiver, args =
                        match args with
                            | receiver :: rest -> receiver, rest
                            | [] ->
                                fail upperPosition "this function is an operation that takes a receiver but none specified"

                    expr context receiver
                    printf' " %s " name
                    compileList args
                    print' ";"
                | UnaryOperator -> 
                    printf' "%s " name
                    compileList args
                    print' ";"

let compileTreeWithParams (definition: Definition) (module_: Module) (into: StreamWriter) (args: IdentifierOrRawValue list) (tree: AstTree) =
    fprintf into "params ["
    for (_, name) in args do
        fprintf into "\"%s\"," name
    fprintf into "];"

    compileStatements { Context.Variables = args; GenSymMonotonic = ref 0; } definition module_ into tree

let compileModule (fileConfig: Config.FileConfig) (module_: Module) =
    let compileDefinition definition =
        let output = Path.Join(fileConfig.Output, sprintf "%s.sqf" module_.Name)
        use writer = new StreamWriter(File.OpenWrite(output))

        compileTreeWithParams definition module_ writer definition.Arguments definition.Code

    module_.Definitions |> List.map compileDefinition
