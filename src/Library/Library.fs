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
    let logToRow dt inout =
        sprintf "%s,%s" (dateTimeToString dt) inout

    let writeRecord (file:StreamWriter) record =
        file.WriteLine(
            logToRow record.time ( getRecordStateString record ))

    let readToEnd (file:StreamReader) =
        file.ReadToEnd()

    let (|DateTime|_|) str =
        match DateTime.TryParse(str:string) with
        | true, dt -> Some(dt)
        | _ -> None

    let getRecordsFolder state (line:string) =
        match state with
        | Error e -> Error e
        | Ok records ->
            let cells = line.Split(',') |> List.ofArray
            match cells with
            | dtString::inout::_ | [dtString;inout] ->
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

    let rec getInTimeFolder state (record:Record) =
        // printfn "State: %A" state
        let inTimeSinceLastRecord = getInTimeFromRecord state.lastRecord
        let getNextState currentDay dayInTimes =
            {
                lastRecord = record
                currentDay = currentDay
                dayInTimes = dayInTimes
            }
        if record.time.Date = state.currentDay.day then
            let inTime = inTimeSinceLastRecord record.time
            let currentDay = addHours state.currentDay inTime
            getNextState currentDay state.dayInTimes
        else
            // Get the remaining inTime for the previous day
            let oneDayTs = TimeSpan(1,0,0,0)
            let nextMidnightDt = (state.lastRecord.time + oneDayTs).Date
            let inTime = inTimeSinceLastRecord nextMidnightDt
            let previousDay = addHours state.currentDay inTime
            let dayInTimes =
                if previousDay.inTime > TimeSpan(0) then
                    (previousDay :: state.dayInTimes)
                else
                    state.dayInTimes
            let nextDay = createDayInTime nextMidnightDt (TimeSpan(0))
            let tempRecord = createRecord state.lastRecord.state nextMidnightDt
            let nextState =
                { 
                    lastRecord = tempRecord
                    currentDay = nextDay
                    dayInTimes = dayInTimes
                }
            getInTimeFolder nextState record 

    let summarize file =
        let lines = (readToEnd file).Trim().Split('\n')

        let recordsRes =
            lines
            |> Array.fold getRecordsFolder (Ok([]))

        match recordsRes with
        | Ok records ->
            let records =
                records
                |> List.rev
                |> List.sortBy (fun record -> record.time)
            // printfn "Records: %A" records
            match records with
            | firstRecord::rest ->
                let currentDay =
                    createDayInTime firstRecord.time.Date (TimeSpan(0))
                let initialState = {
                    lastRecord = firstRecord
                    currentDay = currentDay
                    dayInTimes = []
                }
                // printfn "Records %A rest %A" firstRecord rest
                let inTimesState =
                    List.fold getInTimeFolder initialState rest

                // Calculate any leftover time until now
                let outNowRecord = createRecord Out DateTime.Now
                let inTimesState = getInTimeFolder inTimesState outNowRecord

                // Get the in times from the state
                let dayInTimes = inTimesState.currentDay :: inTimesState.dayInTimes
                printfn "Day In Times: %A" dayInTimes

            | _ ->
                printfn "Not enough records"
        | Error e ->
            printfn "%A" e

        ()
