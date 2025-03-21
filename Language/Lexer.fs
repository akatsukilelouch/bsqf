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
            not <| List.contains c ['('; ')'; '['; ']'; '`'; '"'; ' '; '\n'; '\r']) |>> positioned Atom

let private literal =
    between <| skipChar '"' <| skipChar '"' <| (getPosition .>>. manyChars (attempt (pchar '\\' >>. pchar '"') <|> noneOf ['"'])) |>> positioned Literal

let private quote parser =
    skipChar ''' >>. (getPosition .>>. parser) |>> positioned Quote

let rec private list: CharStream<unit> -> Reply<SExpr> =
    let parenthesized = between (skipChar '(' <|> skipChar '[') (skipChar ')' <|> skipChar ']')

    let self x = list x

    getPosition .>>. parenthesized (between spaces spaces <| sepEndBy (attempt (quote self) <|> attempt literal <|> attempt atom <|> attempt self) spaces) |>> positioned List

exception LexerError of ParserError

let public lex buffer =
    match run (sepEndBy list spaces .>> eof) buffer with
        | Success (lexed, _, _) -> lexed
        | Failure (_, error, _) -> raise <| LexerError error

let public lexFile path =
    File.ReadAllText path |> lex
