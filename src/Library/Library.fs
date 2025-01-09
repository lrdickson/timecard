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
        // eprintfn "State: %A" state
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

    let getRecords lines startTime endTime =
        // Get the records
        lines
        |> Array.fold getRecordsFolder (Ok([]))
        |> function
        | Error e -> Error e
        | Ok records ->
        let sortedRecords =
            records
            |> List.rev
            |> List.sortBy (fun record -> record.time)

        // Get the records that start after the start time
        // eprintfn "records before filter: %A" sortedRecords
        // eprintfn "start time: %A" startTime
        sortedRecords
        |> List.tryFindIndex (fun r -> r.time >= startTime)
        |> function
        | None -> Error "All records are before the start time"
        | Some startIndex ->
        let records =
            if startIndex = 0 then
                sortedRecords
            else
            let tempRecords = snd (List.splitAt (startIndex - 1) sortedRecords)
            match tempRecords with
            | first :: rest ->
                {first with time = startTime} :: rest
            | _ -> sortedRecords

        // Get the records that end before the end time
        let revRecords = List.rev records
        revRecords
        |> List.tryFindIndex (fun r -> r.time <= endTime)
        |> function
        | None -> Error ""
        | Some endIndex ->
        let endRecord = createRecord Out endTime
        endRecord :: (snd (List.splitAt endIndex revRecords))
        |> List.rev
        |> Ok

    let getDayInTimes = function
        // Get the in times from the records
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

    let dateToString (dt:DateTime) =
        dt.ToString("D")

    let timeSpanToString (ts:TimeSpan) =
        sprintf "%ihr %imin %isec"
            ((ts.Days * 24) + ts.Hours)
            ts.Minutes
            ts.Seconds

    let dayInTimeToStrFolder (dayInTime:DayInTime) =
        sprintf "{%s,\n    %s}"
            (jsonDictEntryStrVal "day" (dateToString dayInTime.day))
            (jsonDictEntryStrVal "time" (timeSpanToString dayInTime.inTime))

    let dayInTimesToJson dayInTimes =
        dayInTimes
        |> List.map dayInTimeToStrFolder
        |> stringJoin ",\n"
        |> sprintf "[\n%s\n]"

    let getExtendedInfoJson (now:DateTime) dayInTimes =
        let dayInTimesJson = dayInTimes |> dayInTimesToJson

        // Get the week total
        let totalFolder sum dayInTime =
            sum + dayInTime.inTime
        let weekTotal =
            dayInTimes
            |> List.fold totalFolder (TimeSpan(0))

        // Get standard time left today
        let hoursToday =
            dayInTimes
            |> List.tryFind (fun d -> d.day = now.Date)
            |> function
                | Some d -> d.inTime
                | None -> TimeSpan(0)
        let getStandardDayHours (day:DateTime) =
            match day.DayOfWeek with
            | DayOfWeek.Saturday | DayOfWeek.Sunday -> 0
            | DayOfWeek.Friday -> 8
            | _ -> 9
        let standardHoursToday = TimeSpan(getStandardDayHours now, 0, 0)
        let standardTimeLeft = standardHoursToday - hoursToday
        let standardTimeEnd = now + standardTimeLeft

        // Determine the overtime so far
        let overtimeFolder sum dayInTime =
            let { day = day; inTime = inTime } = dayInTime
            let getWeekdayOvertime expectedDayHours =
                let ts = TimeSpan(expectedDayHours,0,0)
                if now.Date > day then
                    sum + (inTime - ts)
                else
                    sum
            getStandardDayHours day
            |> getWeekdayOvertime
        let overtime =
            dayInTimes
            |> List.fold overtimeFolder (TimeSpan(0))

        // Get end time - (overtime - days remaining in workweek)
        let overtimeOverDaysRemaining = 
            (now - DateTime(2024, 4, 20)).Days
            |> (fun days -> days / 7)
            |> (fun weeks ->
                if (weeks % 2) = 0 then
                    DayOfWeek.Friday
                else
                    DayOfWeek.Thursday)
            |> (fun lastDay ->
                ((int(lastDay) - int(now.DayOfWeek) + 7) % 7) + 1)
            |> (fun daysRemaining ->
                overtime / float(daysRemaining))
        let endMinusOvertime = standardTimeEnd - overtimeOverDaysRemaining

        // Get the planned time today
        let plannedTimeToday = standardHoursToday - overtimeOverDaysRemaining

        // Return the extended information
        let timeToString (t:DateTime) = t.ToString("T")
        [
            jsonDictEntry       "inTimes"           dayInTimesJson
            jsonDictEntryStrVal "weekTotal"         (timeSpanToString weekTotal)
            jsonDictEntryStrVal "standardTimeLeft"  (timeSpanToString standardTimeLeft)
            jsonDictEntryStrVal "standardTimeEnd"   (timeToString standardTimeEnd)
            jsonDictEntryStrVal "overtime"          (timeSpanToString overtime)
            jsonDictEntryStrVal "endMinusOvertime"  (timeToString endMinusOvertime)
            jsonDictEntryStrVal "plannedTimeToday"  (timeSpanToString plannedTimeToday)
        ]
        |> stringJoin ",\n"
        |> sprintf "{%s}"

    let summarize (now:DateTime) file startOption endOption =
        // Calculate the in time
        let lines = (readToEnd file).Trim().Split('\n')
        let startTime =
            match startOption with
            | Some s -> 
                // eprintfn "startTime received: %A" s
                s
            | None ->
                let daysSinceSaturday =
                    int(now.DayOfWeek) - int(DayOfWeek.Saturday)
                let daysSinceSaturday = (daysSinceSaturday + 7) % 7
                now.Date - TimeSpan(daysSinceSaturday,0,0,0)
        let endTime =
            match endOption with
            | Some s -> s
            | None -> now
        let dayInTimesRes =
            getRecords lines startTime endTime
            |> function
            | Error e -> Error e
            | Ok records -> getDayInTimes records

        // Display
        match dayInTimesRes with
        | Error e -> Error e
        | Ok dayInTimes ->

        // Display extra information based on whether we are using the default week option
        match (startOption, endOption) with
        | (None, None) ->
            getExtendedInfoJson now dayInTimes
            |> Ok
        | _ ->
            // Return just the day in times
            dayInTimes
            |> dayInTimesToJson
            |> jsonDictEntry "inTimes"
            |> sprintf "{%s}"
            |> Ok
