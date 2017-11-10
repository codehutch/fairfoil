module AirfoilF.Main

open Expecto
open AirfoilF.Core

[<EntryPoint>]
let main argv =
    Tests.runTestsInAssembly defaultConfig argv
    printfn "%A" myNaca2412
    0
