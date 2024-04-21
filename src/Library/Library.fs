namespace Library
open System
open System.Globalization
open System.IO

module Recorder =
    type LogState =
    | InState
    | OutState

    let dateTimeToString (dt:DateTime) = dt.ToString("yyyy-MM-dd HH:mm:ss", CultureInfo.InvariantCulture)
    let logToRow inout dt = sprintf "%s,%s" inout (dateTimeToString dt)

    let write (file:StreamWriter) (text:string) =
        file.WriteLine(text)
        
    let Record file log dt =
        let writeState inout = write file (logToRow inout dt)
        match log with
        | InState -> writeState "in"
        | OutState -> writeState "out"
