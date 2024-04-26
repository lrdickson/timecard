namespace Library
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
                    Ok( (createRecord rState dt) :: records)
                match inout, dtString with
                | "in", DateTime dt -> okRecords In dt
                | "out", DateTime dt -> okRecords Out dt
                | _ -> Error(sprintf "Unable to parse %s" line)
            | _ -> Error( sprintf "Not enough cells in %s" line)

    type DayInTime =
        {
            day: DateTime
            inTime: TimeSpan
        }

    let createDayInTime day inTime =
        {
            day = day
            inTime = inTime
        }

    let addHours dayInTime inTime =
        {
            day = dayInTime.day
            inTime = dayInTime.inTime + inTime
        }

    type InTimeState =
        {
            lastRecord: Record
            currentDay: DayInTime
            dayInTimes: DayInTime list
        }

    let getInTime state lastTime nextTime =
        match state with
        | In -> nextTime - lastTime
        | Out -> TimeSpan(0)

    let getInTimeFromRecord lastRecord nextTime =
        getInTime lastRecord.state lastRecord.time nextTime

    let getInTimeFolder state (record:Record) =
        let inTimeSinceLastRecord = getInTimeFromRecord state.lastRecord
        let getNextState currentDay dayInTimes =
            {
                lastRecord = record
                currentDay = currentDay
                dayInTimes = dayInTimes
            }
        if record.time.Day = state.lastRecord.time.Day then
            let inTime = inTimeSinceLastRecord record.time
            let currentDay = addHours state.currentDay inTime
            getNextState currentDay state.dayInTimes
        else
            // Get the remaining inTime for the previous day
            let midnight = record.time.Date
            let inTime = inTimeSinceLastRecord midnight
            let previousDay = addHours state.currentDay inTime
            let inTime = getInTime record.state midnight record.time
            let currentDay = createDayInTime midnight inTime
            getNextState currentDay (previousDay :: state.dayInTimes)

    let summarize file =
        let lines = (readToEnd file).Trim().Split('\n')

        let recordsRes =
            lines
            |> Array.fold lineFolder (Ok([]))


        match recordsRes with
        | Ok records ->
            let records =
                records
                |> List.rev
                |> List.sortBy (fun record -> record.time)
            match records with
            | firstRecord::rest ->
                let currentDay =
                    createDayInTime firstRecord.time.Date (TimeSpan(0))
                let initialState = {
                    lastRecord = firstRecord
                    currentDay = currentDay
                    dayInTimes = []
                }
                let dayInTimes =
                    List.fold getInTimeFolder initialState rest
                let dayInTimes = dayInTimes.dayInTimes
                printfn "%A" dayInTimes
            | _ ->
                printfn "Not enough records"
        | Error e ->
            printfn "%A" e

        ()
