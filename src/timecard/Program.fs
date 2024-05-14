open System
open System.IO
open Library.Recorder


type Command =
| FileCommand of string
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
    // eprintfn "Updating state: %A with datetime: %A" state dt
    match state.commandOption with
    | None -> Error $"Unexpected datetime"
    | Some command ->
    match command with
    | InCommand _    -> InCommand(dt) |> Ok
    | OutCommand _   -> OutCommand(dt) |> Ok
    | StartCommand _ -> StartCommand(dt) |> Ok
    | EndCommand _   -> EndCommand(dt) |> Ok
    | FileCommand _ ->
        Error "File command does not accept datetime"
    |> function
    | Error e -> Error e
    | Ok newCommand ->
    // eprintfn "Adding new command: %A" newCommand
    newCommand :: state.commands
    |> createState None |> Ok

// Note this function records the commands in reverse order
let updateStateFilePath state fp =
    match state.commandOption with
    | None -> Error $"Unexpected filepath"
    | Some command ->
    match command with
    | InCommand _ | OutCommand _ | StartCommand _
    | EndCommand _   ->
        Error ("Invalid comman argument: " + fp)
    | FileCommand _ -> FileCommand(fp) |> Ok
    |> function
    | Error e -> Error e
    | Ok newCommand ->
    newCommand :: state.commands
    |> createState None |> Ok

let (|FilePath|_|) str =
    match Uri.IsWellFormedUriString(str, UriKind.RelativeOrAbsolute) with
    | true -> Some(str)
    | false -> None

let parseArgs now args =
    let folder stateResult arg =
        // eprintfn "Parsing arg: %A for state: %A" arg stateResult
        match stateResult with
        | Error e -> Error e
        | Ok state ->

        let nextState command = Ok (createNextState state (Some (command)))
        match arg with
        | "--in"    | "-i"
        | "in"    | "i" -> nextState (InCommand    (now))
        | "--out"   | "-o"
        | "out"   | "o" -> nextState (OutCommand   (now))
        | "--start" | "-s"
        | "start" | "s" -> nextState (StartCommand (now))
        | "--end"   | "-e"
        | "end"   | "e" -> nextState (EndCommand   (now))
        | "--file"  | "-f"
        | "file"  | "f" -> nextState (FileCommand  (""))
        | DateTime dt -> updateStateDateTime state dt
        | FilePath fp -> updateStateFilePath state fp
        | _ ->  Error $"Invalid argument: {arg}"

    let init = Ok (createState None [])
    args |> Array.fold folder init
    |> function
    | Ok state -> Ok (stateGetCommands state)
    | Error e -> Error e

type CliOptions = {
    records: Record list
    startTime: DateTime option
    endTime: DateTime option
    file: string
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
        | FileCommand file -> {cliOptions with file = file}
    commands
    |> List.fold getCliOptionsFolder
        {records = []; startTime = None; endTime = None; file = "timecard.csv"}

let writeRecords cliOptions =
    use file = File.AppendText(cliOptions.file)
    let action record = writeRecord file record
    cliOptions.records |> List.iter action

let readRecords now cliOptions =
    use file = File.OpenText(cliOptions.file)
    summarize now file cliOptions.startTime cliOptions.endTime

[<EntryPoint>]
let main args =
    let now = DateTime.Now

    // Parse the command line arguments
    parseArgs now args
    |> function
    | Error msg -> eprintfn "%s" msg; 1
    | Ok commands ->
    let cliOptions = commands |> getCliOptions

    // Run the commands
    // eprintfn "CLI options: %A" cliOptions
    cliOptions |> writeRecords
    cliOptions |> readRecords now

    // Print the output
    |> function
    | Error msg -> eprintfn "%s" msg; 1
    | Ok json ->
    printfn "%s" json

    0
