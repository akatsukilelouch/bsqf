module Bsqf.Configuration
open System.IO
open Legivel.Serialization

type Module = {
    Path: string;
}

type FileConfig = {
    BootstrapModules: Module list;
}

exception InvalidConfigException

let public parse path =
    match File.ReadAllText path |> Deserialize<FileConfig> |> Seq.head with
        | Success config -> config.Data
        | _ -> raise InvalidConfigException
