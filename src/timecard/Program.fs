open System
open System.IO
open Library.Recorder


type Command =
| InCommand of System.DateTime
| OutCommand of System.DateTime

type ParseState =
    { 
        commandOption: Command option
        commands: Command list
    }

let createState commandOption commands =
    { commandOption = commandOption; commands = commands}

let stateGetCommands state =
    match state.commandOption with
    | Some command -> state.commands @ [ command ]
    | None -> state.commands

let createNextState state commandOption =
    createState commandOption (stateGetCommands state)

let updateStateDateTime state dt =
    match state.commandOption with
    | Some command -> 
        let newCommand = 
            match command with
            | InCommand _ -> InCommand(dt)
            | OutCommand _ -> OutCommand(dt)
        let commands = state.commands @ [newCommand]
        Ok(createState None commands)
    | None -> Error $"Unexpected datetime"

let parseArgs args =
    let now = DateTime.Now
    let folder stateResult arg =
        match stateResult with
        | Ok state ->
            let nextState command = Ok (createNextState state (Some (command)))
            match arg with
            | "in" | "i" -> nextState (InCommand (now))
            | "out" | "o" -> nextState (OutCommand (now))
            | DateTime dt -> updateStateDateTime state dt
            | _ ->  Error $"Invalid argument: {arg}"
        | Error e -> Error e

    let init = Ok (createState None [])
    let res = args |> Array.fold folder init
    match res with
        | Ok state -> Ok (stateGetCommands state)
        | Error e -> Error e
        
let runCommands commands =
    use file = File.AppendText("timecard.csv")
    let action command =
        match command with
        | InCommand dt -> writeRecord file (InRecord (dt))
        | OutCommand dt -> writeRecord file (OutRecord (dt))
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
