module AirfoilF.Core

open System
open FsCheck
open Expecto

open AirfoilF.TestData

let a b = 
  b + 1

let naca4Thickness t x = 5.0 * t * (  0.2969 * (x ** 0.5) 
                                    - 0.1260 *  x 
                                    - 0.3516 * (x ** 2.0)
                                    + 0.2843 * (x ** 3.0) 
                                    - 0.1015 * (x ** 4.0)) // 0.1036 for closed trailing edge   

let naca4Camber m p x =                                     
  
  let naca4CamberPre () = (m / (p ** 2.0)) * (2.0 * p * x - (x ** 2.0))
  let naca4CamberPost () = (m / ((1.0 - p) ** 2.0)) * ((1.0 - 2.0 * p) + 2.0 * p * x - (x ** 2.0)) 

  if (x <= p) 
  then naca4CamberPre ()
  else naca4CamberPost ()

let naca4CamberGradient m p x = 

  let naca4GradientPre () = (2.0 * m) / (p ** 2.0) * (p - x)
  let naca4GradientPost () = (2.0 * m) / ((1.0 - p) ** 2.0) * (p - x)  

  if (x <= p)
  then naca4GradientPre ()
  else naca4GradientPost ()

let naca4CamberAngle m p x = Math.Atan <| naca4CamberGradient m p x



let adjustX x =
  match x with
  | 0.0 ->  0.000000000001
  | 1.0 ->  0.999999999999
  | _ -> x

let naca4UpperSurfaceX m p tt x' =
  let x = adjustX x' 
  x - (naca4Thickness tt x) * Math.Sin (naca4CamberAngle m p x)   
let naca4UpperSurfaceY m p tt x' = 
  let x = adjustX x' 
  naca4Camber m p x + (naca4Thickness tt x) * Math.Cos (naca4CamberAngle m p x)

let naca4LowerSurfaceX m p tt x' = 
  let x = adjustX x' 
  x + (naca4Thickness tt x) * Math.Sin (naca4CamberAngle m p x)

let naca4LowerSurfaceY m p tt x' = 
  let x = adjustX x' 
  naca4Camber m p x - (naca4Thickness tt x) * Math.Cos (naca4CamberAngle m p x)

let cosineSpacedXs num = 
  let n = num - 1
  [ 0..n ] 
  |> List.map (fun x -> 
                 let x' = Math.PI * (double x) / (double n) 
                 (1.0 - Math.Cos x') / 2.0)

let cosineSpacedStationsBackFrontBack numStations =
  let ls = cosineSpacedXs numStations |> List.map (fun x -> x, 1.0) 
  List.rev ls @ List.tail (ls |> List.map (fun (x, y) -> x, -1.0))

let flatListToPairs ls = 
  List.chunkBySize 2 ls
  |> List.map (function [a; b] -> a, b)

let naca2412Pairs = flatListToPairs testNaca2412 

let naca2412NumStations = naca2412Pairs |> List.length |> fun x -> x / 2

let myNaca2412 = 
  let m, p, tt = 0.02, 0.4, 0.12
  cosineSpacedStationsBackFrontBack (naca2412NumStations + 1)
  |> List.map (fun (x, y) -> 
                 match y with
                 | 1.0 -> (naca4UpperSurfaceX m p tt x, naca4UpperSurfaceY m p tt x) 
                 | _ -> (naca4LowerSurfaceX m p tt x, naca4LowerSurfaceY m p tt x)) 


let lowAccuracy = Accuracy.low
let lowerAccuracy = {absolute=1e-5; relative=1e-3}

let testSymmetric input m p tt = 
  flatListToPairs input |>
  List.iteri (fun i tst -> 
    let tstx, tsty = tst
    let myy = if tsty >= 0.0
              then naca4UpperSurfaceY m p tt tstx
              else naca4LowerSurfaceY m p tt tstx
    Expect.floatClose lowerAccuracy tsty myy (sprintf "y different at %A (%A)" tsty i)) 

[<Tests>]
let tests =
  testList "samples" [
  
    testCase "" <| fun _ ->
      let subject = true
      Expect.equal (a 1) 2 "a should add 1"

    testCase "Naca2412" <| fun _ ->
      List.iteri2 (fun i my tst -> 
        let myx, myy = my
        let tstx, tsty = tst
        Expect.floatClose lowAccuracy tstx myx (sprintf "x different at %A (%A)" tstx i)
        Expect.floatClose lowAccuracy tsty myy (sprintf "y different at %A (%A)" tsty i) 
      ) myNaca2412 <| flatListToPairs testNaca2412 

    testCase "Naca0006" <| fun _ -> testSymmetric testNaca0006 0.0 0.0 0.06
    testCase "Naca0015" <| fun _ -> testSymmetric testNaca0015 0.0 0.0 0.15

    testProperty "a should add 1 to anything" <| fun b ->
      a b = b + 1

  ]
