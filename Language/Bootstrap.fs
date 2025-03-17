module Bsqf.Bootstrap
open Bsqf.Lexer
open System.IO

type BootstrapModule = {
    Path: FileInfo;
    Name: string;
}

exception InvalidFileException of string

let report buffer (position: FParsec.Position) (path: string) (error: string) =
    let rec findLineOffset (buffer: string) (index: int) =
        if index - 1 <= 0 || (buffer.Chars (index - 1)).Equals '\n' then
            index
        else
            findLineOffset buffer (index - 1)

    let findLineEnd (buffer: string) (offset: int) =
        buffer[offset ..].IndexOf('\n') + offset

    let lineStart = findLineOffset buffer (int position.Index)
    let lineEnd = findLineEnd buffer lineStart

    let localOffset = int position.Index - lineStart
    let lineSlice = if not (lineEnd.Equals (lineStart - 1)) then buffer[lineStart..lineEnd] else buffer[lineStart..]
    let errorShower = new string(' ', localOffset) + "^"

    printf "%s:%d:%d: %s\n\t%s\n\t%s\n" path position.Line position.Column error lineSlice errorShower

let resolve (fileConfig: Config.FileConfig) (modules: Compiler.Module list) (entry: BootstrapModule) =
    let buffer = File.ReadAllText entry.Path.FullName

    try
        let lexed = Lexer.lex buffer
        let ast = Parser.parse lexed
        let module_ = Compiler.resolveModule modules ast

        ignore <| Directory.CreateDirectory fileConfig.Output
        
        ignore <| Compiler.compileModule fileConfig module_

        Some module_
    with 
        | Lexer.LexerError error ->
            printf "%s" (error.ToString())

            None
        | Parser.ParseException (position, error) ->
            report buffer position entry.Path.FullName error
            
            None
        | Compiler.CompilationError (position, error) ->
            report buffer position entry.Path.FullName error

            None
    
let public bootstrap (config: Config.FileConfig) =
    let mutable modules = []

    for bootstrapModule in config.BootstrapModules do
        let entry = { Path = FileInfo bootstrapModule.Path; Name = Path.GetFileNameWithoutExtension bootstrapModule.Path }

        let module_ = resolve config modules entry

        match module_ with
            | Some module_ -> 
                modules <- List.append modules [module_]
            | None -> ()
