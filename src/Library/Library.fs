namespace Library
open System
open System.Globalization
open System.IO

module Recorder =
    type Record =
    | InRecord of DateTime
    | OutRecord of DateTime

    let dateTimeToString (dt:DateTime) = dt.ToString("yyyy-MM-dd HH:mm:ss", CultureInfo.InvariantCulture)
    let logToRow inout dt = sprintf "%s,%s" inout (dateTimeToString dt)

    let writeLine (file:StreamWriter) (text:string) =
        file.WriteLine(text)
        
    let writeRecord file record =
        let writeState inout dt = writeLine file (logToRow inout dt)
        match record with
        | InRecord dt -> writeState "in" dt
        | OutRecord dt -> writeState "out" dt

    let readToEnd (file:StreamReader) =
        file.ReadToEnd()

    let (|DateTime|_|) str =
        match DateTime.TryParse(str:string) with
        | true, dt -> Some(dt)
        | _ -> None

    let summarize file =
        let lines = (readToEnd file).Split('\n')
        let folder state (line:string) =
            match state with
            | Error e -> Error e
            | Ok records ->
                let cells = line.Split(',') |> List.ofArray
                match cells with
                | inout::dtString::_ ->
                    match inout, dtString with
                    | "in", DateTime dt -> Ok(InRecord(dt))
                    | "out", DateTime dt -> Ok(OutRecord(dt))
                    | _ -> Error(sprintf "Unable to parse %s" line)
                | _ -> Error (sprintf "Not enough cells in %s" line)

        ()
