﻿namespace Library
open System
open System.Globalization
open System.IO

module Recorder =
    type RecordState =
    | In
    | Out

    type Record =
        {
            state: RecordState
            time: DateTime
        }

    let createRecord rState dt =
        { state = rState; time = dt }

    let getRecordStateString record =
        match record.state with
        | In ->  "in"
        | out -> "out"

    let dateTimeToString (dt:DateTime) = dt.ToString(
        "yyyy-MM-dd HH:mm:ss", CultureInfo.InvariantCulture)
    let logToRow inout dt =
        sprintf "%s,%s" inout (dateTimeToString dt)

    let writeRecord (file:StreamWriter) record =
        file.WriteLine(
            logToRow ( getRecordStateString record ) record.time)

    let readToEnd (file:StreamReader) =
        file.ReadToEnd()

    let (|DateTime|_|) str =
        match DateTime.TryParse(str:string) with
        | true, dt -> Some(dt)
        | _ -> None

    let lineFolder state (line:string) =
        match state with
        | Error e -> Error e
        | Ok records ->
            let cells = line.Split(',') |> List.ofArray
            match cells with
            | inout::dtString::_ | [inout;dtString] ->
                let okRecords rState dt =
                    Ok(records @ [createRecord rState dt])
                match inout, dtString with
                | "in", DateTime dt -> okRecords In dt
                | "out", DateTime dt -> okRecords Out dt
                | _ -> Error(sprintf "Unable to parse %s" line)
            | _ -> Error( sprintf "Not enough cells in %s" line)

    let summarize file =
        let lines = (readToEnd file).Trim().Split('\n')

        let recordsRes =
            lines
            |> Array.fold lineFolder (Ok([]))

        match recordsRes with
        | Ok records ->
            let records =
                records |> List.sortBy (fun record -> record.time)
            printfn "%A" records
        | Error e ->
            printfn "%A" e

        ()
