open System
open System.IO
open Library.Recorder


type Command =
| InCommand of System.DateTime
| OutCommand of System.DateTime

type ParseStateOption =
| NoneOption
| InOption
| OutOption

type ParseState =
    { 
        stateOption: ParseStateOption
        commands: Command list
    }

let (|DateTime|_|) str =
    match DateTime.TryParse(str:string) with
    | true, dt -> Some(dt)
    | _ -> None

let stateGetCommands datetime state =
    match state with
    | { stateOption = InOption; commands = commands } ->
        commands @ [InCommand (datetime)]
    | { stateOption = OutOption; commands = commands } ->
        commands @ [OutCommand (datetime)]
    | { stateOption = NoneOption; commands = commands } ->
        commands
        
let createState stateOption commands =
    { stateOption = stateOption; commands = commands}
let createNextState stateOption state datetime =
    createState stateOption (stateGetCommands datetime state)

let parseArgs args =
    let now = DateTime.Now
    let folder stateResult arg =
        match stateResult with
        | Ok state ->
            match arg with
            | "in" | "i" -> Ok (createNextState InOption state now)
            | "out" | "o" -> Ok (createNextState OutOption state now)
            | DateTime dt -> Ok (createNextState NoneOption state dt)
            | _ ->  Error $"Invalid argument: {arg}"
        | Error e -> Error e

    let init = Ok (createState NoneOption [])
    let res = args |> Array.fold folder init
    match res with
        | Ok state -> Ok (stateGetCommands now state)
        | Error e -> Error e
        
let runCommands commands =
    use file = File.AppendText("timecard.csv")
    let action command =
        match command with
        | InCommand dt -> Record file InState dt
        | OutCommand dt -> Record file OutState dt
    commands |> List.iter action
    
[<EntryPoint>]
let main args =
    let res = parseArgs args
    match res with
    | Ok commands ->
        runCommands commands
        0
    | Error msg ->
        printfn "%s" msg
        1
