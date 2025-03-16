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
    let lexed = Lexer.lexFile entry.Path.FullName

    match lexed with
        | Lexed lexed -> 
            let ast =
                match lexed with
                    | _, List inner -> Parser.parse inner
                    | _ -> raise <| InvalidFileException "file should be a list"

            printf "%s\n" (ast.ToString())
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
