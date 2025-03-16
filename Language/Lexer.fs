module Bsqf.Lexer
open System.IO
open FParsec

type SExprValue =
    | Atom of identifier: string
    | Literal of value: string
    | Quote of quoted: SExpr
    | List of list: SExpr list
and SExpr = Position * SExprValue

let positioned constructor (position, value) =
    position, constructor value

let private atom =
    getPosition .>>. (many1Satisfy <|
        fun c ->
            not <| List.contains c ['('; ')'; '`'; '"'; ' '; '\n'; '\r']) |>> positioned Atom

let private literal =
    between <| skipChar '"' <| skipChar '"' <| (getPosition .>>. manyChars (attempt (pchar '\\' >>. pchar '"') <|> noneOf ['"'])) |>> positioned Literal

let private quote parser =
    skipChar '`' >>. (getPosition .>>. parser) |>> positioned Quote

#nowarn 40
let rec private list: CharStream<unit> -> Reply<SExpr> =
    let parenthesized = between (skipChar '(') (skipChar ')')

    let self x = list x

    getPosition .>>. parenthesized (between spaces spaces <| sepEndBy (attempt literal <|> attempt atom <|> attempt (quote self) <|> attempt self) spaces) |>> positioned List

exception ParsingErrorException of string

let public lex buffer : SExpr =
    match run list buffer with 
        | Success (result, _, _) -> result
        | Failure (_, parserError, _) -> raise (ParsingErrorException (parserError.ToString()))

let public lexFile path =
    File.ReadAllText path |> lex
