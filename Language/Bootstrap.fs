module Bsqf.Bootstrap
open Bsqf.Lexer
open System.IO

type Module = {
    Path: FileInfo;
    Name: string;
}

type Context = {
    mutable Modules: Map<string, Module>;
}

exception InvalidFileException of string

let resolve (config: Config.FileConfig) (context: Context) (entry: Module) =
    let buffer = File.ReadAllText entry.Path.FullName

    let lexed = Lexer.lex buffer

    match lexed with
        | Lexed lexed -> 
            let ast =
                match lexed with
                    | _, List inner -> Parser.parse inner
                    | _ -> raise <| InvalidFileException "file should be a list"

            match ast with
                | Parser.ParseResult.Ok tree -> 
                    printf "%s\n" (tree.ToString())
                | Parser.ParseResult.Error (position, error) ->
                    let rec findLineOffset (buffer: string) (index: int) =
                        if (buffer.Chars (index - 1)).Equals '\n' then
                            index
                        else
                            findLineOffset buffer (index - 1)

                    let findLineEnd (buffer: string) (offset: int) =
                        buffer[offset ..].IndexOf('\n') + offset

                    let lineStart = findLineOffset buffer (int position.Index)
                    let lineEnd = findLineEnd buffer lineStart

                    let localOffset = (int position.Index) - lineStart
                    let lineSlice = if not (lineEnd.Equals -1) then buffer[lineStart..lineEnd] else buffer[lineStart..]
                    let errorShower = new string(' ', localOffset) + "^"

                    printf "%s:%d:%d: %s\n\t%s\n\t%s\n" entry.Path.FullName position.Line position.Column error lineSlice errorShower

        | Error error ->
            printf "%s:%d:%d: %s\n" entry.Path.FullName error.Position.Line error.Position.Column (error.ToString())


let public bootstrap (config: Config.FileConfig) =
    let context = {
        Modules = Map [];
    }

    for bootstrapModule in config.BootstrapModules do
        let moduleEntity = { Path = FileInfo bootstrapModule.Path; Name = Path.GetFileNameWithoutExtension bootstrapModule.Path }

        context.Modules <- context.Modules.Add(moduleEntity.Name, moduleEntity)

    for entry in context.Modules do
        resolve config context entry.Value
