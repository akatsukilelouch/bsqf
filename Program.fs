open Argu
open System.IO

type Argument =
    | ManifestPath of path: string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | ManifestPath _ -> "project manifest path."

type ManifestPath =
    | ContinuousAscendTil of name: string
    | SpecificPath of path: string

exception NoManifestException

let inline start path =
    let manifest, workingDirectory =
        match path with
            | SpecificPath path -> new FileInfo(path), Directory.GetParent path
            | ContinuousAscendTil name ->
                let rec search (at: DirectoryInfo) =
                    match at.GetFiles() |> Seq.tryFind (fun fileInfo -> fileInfo.Name.Equals name) with
                        | Some item -> Some (item, item.Directory)
                        | None ->
                            let parent = at.Parent

                            if isNull parent then
                                None
                            else
                                search parent

                match search <| new DirectoryInfo(Directory.GetCurrentDirectory()) with
                    | Some (manifest, workingDirectory) ->
                        manifest, workingDirectory
                    | None ->
                        raise NoManifestException

    Bsqf.Bootstrap.bootstrap (Bsqf.Config.parse manifest.FullName)

[<EntryPoint>]
let main args =
    let parser = ArgumentParser.Create<Argument>()

    let args = parser.Parse args

    start <|
        if not (args.Contains ManifestPath) then
            ContinuousAscendTil "bsqf.yaml"
        else
            SpecificPath <| args.GetResult(ManifestPath, "bsqf.yaml")

    0
