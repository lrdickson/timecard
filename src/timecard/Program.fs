open System
open System.IO
open Library.Recorder


type Command =
| InCommand of System.DateTime
| OutCommand of System.DateTime
| StartCommand of System.DateTime
| EndCommand of System.DateTime

type ParseState =
    { 
        commandOption: Command option
        commands: Command list
    }

let createState commandOption commands =
    { commandOption = commandOption; commands = commands}

// Note this function records the commands in reverse order
let stateGetCommands state =
    match state.commandOption with
    | Some command -> command :: state.commands
    | None -> state.commands

let createNextState state commandOption =
    createState commandOption (stateGetCommands state)

// Note this function records the commands in reverse order
let updateStateDateTime state dt =
    match state.commandOption with
    | Some command -> 
        let newCommand = 
            match command with
            | InCommand _    -> InCommand(dt)
            | OutCommand _   -> OutCommand(dt)
            | StartCommand _ -> StartCommand(dt)
            | EndCommand _   -> EndCommand(dt)
        let commands = newCommand :: state.commands
        Ok(createState None commands)
    | None -> Error $"Unexpected datetime"

let parseArgs now args =
    let folder stateResult arg =
        match stateResult with
        | Ok state ->
            let nextState command = Ok (createNextState state (Some (command)))
            match arg with
            | "in"    | "i" -> nextState (InCommand    (now))
            | "out"   | "o" -> nextState (OutCommand   (now))
            | "start" | "s" -> nextState (StartCommand (now))
            | "end"   | "e" -> nextState (EndCommand   (now))
            | DateTime dt -> updateStateDateTime state dt
            | _ ->  Error $"Invalid argument: {arg}"
        | Error e -> Error e

    let init = Ok (createState None [])
    let res = args |> Array.fold folder init
    match res with
        | Ok state -> Ok (stateGetCommands state)
        | Error e -> Error e

type CliOptions = {
    records: Record list
    startTime: DateTime option
    endTime: DateTime option
}

// Note this function records the records in the reverse order of the commands list
// Since the commands list is in the reverse order of the arguments passed,
// this function corrects the order of the records
let getCliOptions commands =
    let getCliOptionsFolder cliOptions command =
        let addRecord rState dt =
            { cliOptions with
                records = (createRecord rState dt) :: cliOptions.records
            }
        match command with
        | InCommand dt -> addRecord In dt
        | OutCommand dt -> addRecord Out dt
        | StartCommand dt -> {cliOptions with startTime = Some(dt)}
        | EndCommand dt -> {cliOptions with endTime = Some(dt)}
    commands
    |> List.fold getCliOptionsFolder {records = []; startTime = None; endTime = None}
    
        
let writeRecords cliOptions =
    use file = File.AppendText("timecard.csv")
    let action record = writeRecord file record
    cliOptions.records |> List.iter action
    cliOptions

let readRecords now cliOptions =
    use file = File.OpenText("timecard.csv")
    summarize now file cliOptions.startTime cliOptions.endTime

[<EntryPoint>]
let main args =
    let now = DateTime.Now
    let res = 
        parseArgs now args
    match res with
    | Ok commands ->
        let res = 
            commands
            |> getCliOptions
            |> writeRecords
            |> readRecords now
        match res with
        | Ok json ->  printfn "%s" json; 0
        | Error msg -> printfn "%s" msg; 1
    | Error msg -> printfn "%s" msg; 1
