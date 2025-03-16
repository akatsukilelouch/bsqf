module Bsqf.Lexer
open System.IO
open FParsec

type SExpr =
    | Atom of identifier: string
    | Literal of value: string
    | Quote of quoted: SExpr
    | List of SExpr list

let private atom =
    many1Satisfy <|
        fun c ->
            not <| List.contains c ['('; ')'; '`'; '"'; ' '; '\n'; '\r']
    |>> Atom

let private literal =
    between <| skipChar '"' <| skipChar '"' <| manyChars (attempt (pchar '\\' >>. pchar '"') <|> noneOf ['"']) |>> Literal

let private quote parser =
    skipChar '`' >>. parser |>> Quote <|> parser

#nowarn 40
let rec private list =
    let parenthesized = between (skipChar '(') (skipChar ')')

    let self x = list x

    parenthesized (between spaces spaces <| sepEndBy (attempt literal <|> attempt atom <|> attempt (quote self) <|> attempt self) spaces) |>> List

exception ParsingErrorException of string

let public lex buffer =
    match run list buffer with 
        | Success (result, _, _) -> result
        | Failure (_, parserError, _) -> raise (ParsingErrorException (parserError.ToString()))

let public lexFile path =
    File.ReadAllText path |> lex
