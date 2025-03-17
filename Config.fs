module Bsqf.Config
open System.IO
open Legivel.Serialization

type Module = {
    Path: string;
}

type FileConfig = {
    Output: string;
    BootstrapModules: Module list;
    UseStdLib: bool;
}

exception InvalidConfigException

let public parse path =
    match File.ReadAllText path |> Deserialize<FileConfig> |> Seq.head with
        | Success config -> config.Data
        | _ -> raise InvalidConfigException
