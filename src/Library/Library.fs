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
        { dayInTime with inTime = dayInTime.inTime + inTime }

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
        let getNextState dayInTimes currentDay =
            {
                lastRecord = record
                currentDay = currentDay
                dayInTimes = dayInTimes
            }
        let getNextDayInTimes dayInTime =
            if dayInTime.inTime > TimeSpan(0) then
                (dayInTime :: state.dayInTimes)
            else
                state.dayInTimes

        if record.time.Date = state.currentDay.day then
            // Record days match
            inTimeSinceLastRecord record.time
            |> addHours state.currentDay
            |> getNextState state.dayInTimes

        else
            if state.lastRecord.state = Out then
                // A new day has started and the last record of the pervious day was an out record
                createDayInTime record.time.Date (TimeSpan(0))
                |> (getNextDayInTimes state.currentDay
                |> getNextState)

            else
                // A new day has started and the last record of the previous day was an in record
                // Calculate the time remaining from the previous day
                let oneDayTs = TimeSpan(1,0,0,0)
                let nextMidnightDt = (state.lastRecord.time + oneDayTs).Date
                let dayInTimes =
                    nextMidnightDt
                    |> inTimeSinceLastRecord
                    |> addHours state.currentDay
                    |> getNextDayInTimes

                // Setup a new day and a temporary record, then handle the rest with a recursive call
                let nextDay = createDayInTime nextMidnightDt (TimeSpan(0))
                let tempRecord =
                    {state.lastRecord with time = nextMidnightDt}
                let nextState =
                    {
                        lastRecord = tempRecord
                        currentDay = nextDay
                        dayInTimes = dayInTimes
                    }
                getInTimeFolder nextState record

    let getDayInTimes lines startTime endTime =
        // Get the records
        let recordsRes =
            lines
            |> Array.fold getRecordsFolder (Ok([]))
        match recordsRes with
        | Error e ->
            Error e
        | Ok records ->
            let records =
                records
                |> List.rev
                |> List.sortBy (fun record -> record.time)

            // Get the records that start after the start time
            let startIndex =
                records |>
                List.findIndex (fun r -> r.time >= startTime)
            let records =
                if startIndex = 0 then
                    records
                else
                    let tempRecords = snd (List.splitAt (startIndex - 1) records)
                    match tempRecords with
                    | first :: rest ->
                        {first with time = startTime} :: rest
                    | _ -> records

            // Get the records that end before the end time
            let revRecords = List.rev records
            let endIndex =
                revRecords |>
                List.findIndex (fun r -> r.time <= endTime)
            let endRecord = createRecord Out endTime
            let records =
                endRecord :: (snd (List.splitAt endIndex revRecords))
                |> List.rev

            // Get the in times from the records
            match records with
            | firstRecord::rest ->
                let currentDay =
                    createDayInTime firstRecord.time.Date (TimeSpan(0))
                let initialState = {
                    lastRecord = firstRecord
                    currentDay = currentDay
                    dayInTimes = []
                }
                let inTimesState =
                    List.fold getInTimeFolder initialState rest

                // Return the in times from the state
                if inTimesState.currentDay.inTime > TimeSpan(0) then
                    inTimesState.currentDay :: inTimesState.dayInTimes
                else
                    inTimesState.dayInTimes
                |> List.rev |> Ok

            | _ ->
                Error "Not enough records"

    let stringJoin separator (list:string list) =
        String.Join(separator, list)

    let jsonDictEntry key value =
        sprintf """"%s": %s""" key value

    let jsonDictEntryStrVal key value =
        sprintf "\"%s\"" value
        |> jsonDictEntry key

    let dateToWeekdayString (dt:DateTime) =
        dt.ToString("D")

    let dayInTimeToStrFolder (dayInTime:DayInTime) =
        sprintf "{%s,\n    %s}"
            (jsonDictEntryStrVal "day" (dateToWeekdayString dayInTime.day))
            (jsonDictEntryStrVal "time" (dayInTime.inTime.ToString()))

    let dayInTimesToJson dayInTimes =
        dayInTimes
        |> List.map dayInTimeToStrFolder
        |> stringJoin ",\n"
        |> sprintf "[\n%s\n]"

    let summarize (now:DateTime) file startOption endOption =
        // Calculate the in time
        let lines = (readToEnd file).Trim().Split('\n')
        let startTime =
            match startOption with
            | Some s -> s
            | None ->
                let daysSinceSaturday =
                    int(now.DayOfWeek) - int(DayOfWeek.Saturday)
                let daysSinceSaturday = (daysSinceSaturday + 7) % 7
                now.Date - TimeSpan(daysSinceSaturday,0,0,0)
        let endTime =
            match endOption with
            | Some s -> s
            | None -> now
        let dayInTimesRes = getDayInTimes lines startTime endTime

        // Display
        match dayInTimesRes with
        | Ok dayInTimes ->

            let dayInTimesJson = dayInTimes |> dayInTimesToJson

            // Display extra information based on whether we are using the default week option
            match (startOption, endOption) with
            | (None, None) ->
                // Get the week total
                let totalFolder sum dayInTime =
                    sum + dayInTime.inTime
                let weekTotal =
                    dayInTimes
                    |> List.fold totalFolder (TimeSpan(0))

                let stuff =
                    (now - DateTime(2024, 4, 20)).Days
                    |> (fun days -> days / 7)
                    |> (fun weeks ->
                        if (weeks % 2) = 0 then
                            (44, DayOfWeek.Friday)
                        else
                            (36, DayOfWeek.Thursday))

                // Print the extended information
                let dictPairs = [
                    (jsonDictEntry "inTimes" dayInTimesJson)
                    (jsonDictEntryStrVal "weekTotal" (weekTotal.ToString()))
                ]
                let json =
                    sprintf "{%s}" (stringJoin ",\n" dictPairs)
                printfn "%s" json
            | _ ->
                // Print just the day in times
                printfn "%s" dayInTimesJson
        | Error e -> printfn "%A" e
