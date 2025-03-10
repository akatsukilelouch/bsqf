module Bsqf.Bootstrap
open System.IO

type Module = {
    Path: FileInfo;
    Name: string;
}

type Context = {
    mutable Modules: Map<string, Module>;
}

let resolve (config: Configuration.FileConfig) (context: Context) (entry: Module) =
    let lexed = Lexer.lexFile entry.Path.FullName

    printf "%s" <| Parser.compile lexed

let public bootstrap (config: Configuration.FileConfig) =
    let context = {
        Modules = Map [];
    }

    for bootstrapModule in config.BootstrapModules do
        let moduleEntity = { Path = FileInfo bootstrapModule.Path; Name = Path.GetFileNameWithoutExtension bootstrapModule.Path }

        context.Modules <- context.Modules.Add(moduleEntity.Name, moduleEntity)

    for entry in context.Modules do
        resolve config context entry.Value
