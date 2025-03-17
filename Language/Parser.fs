module Bsqf.Parser
open Bsqf.Config
open Bsqf.Ast
open Bsqf.Lexer
open FParsec

exception ParseException of Position * string

let fail position reason =
    raise <| ParseException (position, reason)

let rec parseSimpleLiteralAtom ((position, value): SExpr) =
    match value with
        | Atom value -> position, value
        | _ -> raise <| ParseException (position, "expected atom, probably a number")

let isANumberLiteral (str: string) =
    isDigit (str.Chars 0)

let rec parseExpression ((position, value): SExpr) : AstExpression =
    let isANumber (x: string) =
        let mutable _tmp: double = 0

        System.Double.TryParse(x, &_tmp)


    match value with
        | List ((_, Atom ":if") :: defs) ->
            let condition, then_, else_ =
                match defs with
                    | condition :: then_ :: [] -> 
                        condition, then_, None
                    | condition :: then_ :: else_ :: [] -> 
                        condition, then_, Some else_
                    | (position, _) :: _ -> 
                        fail position "expecting a condition clause and a code clause"
                    | [] -> 
                        fail position "unexpected empty if clause"

            position, AstExpressionValue.If (parseExpression condition, parseExpression then_, else_ |> Option.map parseExpression)
        | List ((_, Atom ":for-each") :: defs) -> 
            let identifier, list, body =
                match defs with
                    | (_, List ((position, Atom element) :: list :: [])) :: body :: [] ->
                        (position, element), list, body
                    | (position, List _) :: [] ->
                        fail position "expected code after binding"
                    | (position, _) :: _ ->
                        fail position "for-each binding requires binding a variable from a list in the form of (element list)"
                    | [] ->
                        fail position "for-each binding requires binding a variable from a list in the form of (element list)"

            position, AstExpressionValue.ForEach (identifier, parseExpression list, parseExpression body)
        | Quote (_, List list)  ->
            let rec parseList items defs =
                match defs with
                    | expr :: left ->
                        parseList (parseExpression expr :: items) left
                    | [] ->
                        items |> List.rev
            
            position, AstExpressionValue.List (parseList [] list)
        | Quote _ ->
            fail position "you may only quote lists to create lists"
        | Atom name when not (isANumberLiteral name || name.StartsWith ":")-> 
            position, Identifier (position, name)
        | Atom number when isANumber number ->
            position, Number number
        | Literal literal ->
            position, String literal
        | List ((funcPosition, Atom func) :: args) ->
            position, AstExpressionValue.Call ((funcPosition, func), args |> List.map parseExpression)
        | List inner ->
            position, AstExpressionValue.Block (parseStatements [] inner)
        | _ -> 
            fail position "expected an expression"
and parseStatement ((position, value): SExpr) =
    match value with
        | List ((_, Atom ":for") :: defs) -> 
            let rec getForDefs (from_, to_, step) (defs: SExpr list) =
                match defs with
                    | (_, List ((_, Atom "from") :: from_ :: [])) :: defs ->
                        getForDefs (Some <| parseSimpleLiteralAtom from_, to_, step) defs
                    | (_, List ((_, Atom "to") :: to_ :: [])) :: defs ->
                        getForDefs (from_, Some <| parseSimpleLiteralAtom to_, step) defs
                    | (_, List ((_, Atom "step") :: step :: [])) :: defs ->
                        getForDefs (from_, to_, Some <| parseSimpleLiteralAtom step) defs
                    | (_, List code) :: [] ->
                        from_, to_, step, code
                    | (position, _) :: [] ->
                        fail position "expected code"
                    | (position, _) :: _ ->
                        fail position "expected either a from, to or step binding"
                    | [] ->
                        fail position "unexpected empty for clause"

            match defs with
                | (identifierPosition, Atom identifier) :: defs ->
                    let from_, to_, step, code = getForDefs (None, None, None) defs

                    position, AstStatementValue.For ((identifierPosition, identifier), from_, to_, step, parseStatements [] code)
                | (position, _) :: _ ->
                    fail position "excepted a for binding identifier that is an atom"
                | [] -> 
                    fail position "excepted a for binding identifier"

        | List ((_, Atom ":while") :: defs) ->
            let condition, code =
                match defs with
                    | condition :: (_, List code) :: [] -> 
                        condition, code
                    | (position, _) :: _ -> 
                        fail position "expecting a condition clause and a code clause"
                    | [] -> 
                        fail position "unexpected empty while clause"

            position, AstStatementValue.While (parseExpression condition, parseStatements [] code)
        | List ((_, Atom ":break") :: defs) ->
            let with_ =
                match defs with
                    | code :: [] -> Some code
                    | [] -> None
                    | (position, _) :: _ ->
                        fail position "expected exit-with expression or no clause at all"

            position, AstStatementValue.Break (with_ |> Option.map parseExpression)
        | List ((_, Atom ":continue") :: defs) ->
            let with_ =
                match defs with
                    | code :: [] -> Some code
                    | [] -> None
                    | (position, _) :: _ ->
                        fail position "expected exit-with expression or no clause at all"

            position, AstStatementValue.Continue (with_ |> Option.map parseExpression)
        | List ((_, Atom ":exit") :: defs) ->
            let with_ =
                match defs with
                    | code :: [] -> Some code
                    | [] -> None
                    | (position, _) :: _ ->
                        fail position "expected exit-with expression or no clause at all"

            position, AstStatementValue.Exit (with_ |> Option.map parseExpression)
        | List ((_, Atom ":set") :: defs) ->
            let (identifier, expr) =
                match defs with
                    | (position, Atom name) :: code :: [] -> (position, name), code
                    | (position, _) :: _ ->
                        fail position "expected a name and an expression"
                    | [] ->
                        fail position "expected a name and an expression"

            position, AstStatementValue.Set (identifier, parseExpression expr)
        | otherwise ->
            position, AstStatementValue.Expression (parseExpression (position, otherwise))
and parseStatements (statements : AstStatement list) (expr: SExpr list) =
    match expr with 
        | left :: right ->
            parseStatements (parseStatement left :: statements) right
        | [] ->
            statements |> List.rev
let rec parseTopLevel (tree: AstTopLevel list) (expr: SExpr list) =
    match expr with
        | (position, List item) :: rest ->
            let newTree =
                match item with
                    | (_, Atom "#module") :: defs ->
                        let name =
                            match defs with
                                | (position, Atom name) :: [] -> position, name
                                | (position, _) :: _ ->
                                    fail position "expected module name"
                                | _ ->
                                    fail position "expected module name"

                        position, Module name
                    | (_, Atom "#declare") :: defs ->
                        let declare =
                            match defs with
                                | (_, Atom "function") :: (position, Literal name) :: [] -> Function (position, name)
                                | (_, Atom "operator") :: (position, Literal name) :: [] -> AstDeclare.Operator ((position, name), false)
                                | (_, Atom "method") :: (position, Literal name) :: [] -> AstDeclare.Operator ((position, name), true)
                                | (position, _) :: _ ->
                                    fail position "expected function, operator or method and a name"
                                | _ ->
                                    fail position "expected function, operator or method and a name"

                        position, Declare declare
                    | (_, Atom "#import") :: defs -> 
                        let identifier, items =
                            match defs with
                                | (position, Atom name) :: [] ->
                                    (position, name), []
                                | (position, Atom name) :: (_, List items) :: [] ->
                                    (position, name), items
                                | (position, _) :: _ ->
                                    fail position "expected module name"
                                | [] ->
                                    fail position "expected module name"
                        
                        position, Import (identifier, items |> List.map parseSimpleLiteralAtom)
                    | (_, Atom "#define") :: (namePosition, Atom name) :: item :: [] -> 
                        position, Definition ((namePosition, name), [], [parseStatement item])
                    | (_, Atom "#define") :: (namePosition, Atom name) :: (_, List args) :: body -> 
                        position, Definition ((namePosition, name), args |> List.map parseSimpleLiteralAtom, parseStatements [] body)
                    | _ ->
                        fail position "a top-level entry should start with a built-in function #import or a #define"

            parseTopLevel (newTree :: tree) rest
        | (position, _) :: _ ->
            fail position "a top-level entry should be either an import or a define macro call"
        | [] ->
            tree |> List.rev

type ParseResult =
    | Ok of AstTopLevel list
    | Error of position: Position * message: string

/// The parse routine takes the raw expression and gives out the AST that
/// embodies the actual code to be spewed out later in the pipeline.
/// 
/// At this point we just transform the S-expressions into more manageable AST form
/// which is later analyzed for excess variables, get rid of local variable names (minified),
/// analyzed for module imports and such.
/// 
/// Before this code contains module definitions but I think it is very hard to nagivate it this way.
/// 
/// <h1>Core components</h1>
/// 
/// The language consists of a few superset features such as lambda functions, definitions, imports and exports.
/// (These features are absent in the SQF language.)
///
/// They make room for better code coupling.
/// 
/// <h2>Context</h2>
/// 
/// The context is created locally for the parse run and then returned from the function.
/// This context is later to be analyzed and compiled. At this point it is just better to use
/// mapping table for compilation.
let parse (expr : SExpr list) =
    parseTopLevel [] expr
