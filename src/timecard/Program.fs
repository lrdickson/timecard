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
    let folder state arg =
        match arg with
        | "in" | "i" -> createNextState InOption state now
        | "out" | "o" -> createNextState OutOption state now
        | DateTime dt -> createNextState NoneOption state dt 
        | _ -> 
            printfn $"Invalid argument: {arg}"
            state

    args 
        |> List.fold folder {
            stateOption = NoneOption; commands = []}
        |> stateGetCommands now
        

[<EntryPoint>]
let main args =
    hello "from F#"
    let commands = parseArgs (Array.toList args)
    printfn "%A" commands

    0
