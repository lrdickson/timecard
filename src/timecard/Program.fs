open System
open Library.Say


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
        
[<EntryPoint>]
let main args =
    hello "from F#"
    let res = parseArgs args
    printfn "%A" res

    0
