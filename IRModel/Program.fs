// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open System
open Deedle
open IRModel
open IRModel
open tenorOps

[<EntryPoint>]
let main argv = 
    let time = System.Diagnostics.Stopwatch.StartNew()

    let ustreasury0 = Frame.ReadCsv(@"C:\Users\venus\code\github\IRModels\data\USTREASURY-YIELD.csv",true,true)                        
                        |> Frame.indexRows "time"                        
                        |> Frame.sortRowsByKey
                        |> Frame.mapValues(fun x -> x/100.0) 
                        |> Frame.fillMissingWith(Direction.Backward)
                        |> Frame.fillMissingWith(Direction.Forward)
                        |> Frame.fillMissingUsing(fun series K -> series |> Stats.mean)
                        
                        
    let orderedtenors = ustreasury0.ColumnKeys 
                        |> Seq.map(fun c-> Volatility.tenor2years(c),c)
                        |> Seq.sortBy(fun (a,b) -> a)
                        |> Seq.map(fun (_,b) -> b)


    let ustreasury = Frame.ofColumns(ustreasury0.Columns.Realign(orderedtenors))
        
    ustreasury.SaveCsv("C:/temp/ustreasury.csv",true)
    let uszero = Ratetransformations.fromContinuousRatetoZeros(ustreasury);
    uszero.SaveCsv("C:/temp/uszero.csv",true)
    let usinstantaneousforward = Ratetransformations.fromZerostoInstantaneousForward(uszero)
    usinstantaneousforward.SaveCsv("C:/temp/usinstantaneousforward.csv",true)
    let forwardframe = Frame.ReadCsv(@"C:\Users\venus\code\github\IRModels\data\hjm_formatted.csv",true,true)
    let frame:Frame<DateTime,string> = forwardframe |> Frame.indexRows("time") |> Frame.sortRowsByKey
    //let hjm = hjmframework(frame,3,1.0,200,252)
    let hjm = hjmframework(usinstantaneousforward,3,1.0,360,12*30)

    let Nsims = 1000
    

    let MCforwards = seq {for i in 0 .. Nsims do yield hjm.runPath()}
    let tenors2compute = [|1.0<month>;3.0<month>;6.0<month>; 5.0<year>* years2months; 15.0<year>* years2months |]
    let tstart = 0.5<year>
    let l3marr=
        MCforwards |> Seq.mapi(fun counter cube ->                                              
                                            if counter%100 = 0 then
                                                    printfn "%A" (float(counter) / float(Nsims) * 100.0)                                                    
                                            tenors2compute 
                                            |> Array.map(fun T -> hjm.computeForward(cube)(tstart,T*months2years))
                                            |> floatArray
                                            )
                   |>Array.ofSeq                   
                   |> MathNet.Numerics.LinearAlgebra.Matrix.Build.DenseOfRowArrays
    let avgcurve =      l3marr.EnumerateColumns() 
                            |> Seq.mapi(fun i c -> c.ToArray() |> Array.average) 
                            |> Array.ofSeq 
                            |> Array.mapi(fun i v -> tenors2compute.[i], v) |> Series.ofObservations
    let stddevcurve =   l3marr.EnumerateColumns() 
                        |> Seq.map(fun c -> MathNet.Numerics.Statistics.Statistics.StandardDeviation(c))
                        |> Array.ofSeq
                        |> Array.mapi(fun i v -> tenors2compute.[i], v) |> Series.ofObservations

    let rateat999 =   l3marr.EnumerateColumns() |> Seq.map(fun c -> 
                                                                    let l = c.ToArray() |> Array.sort
                                                                    l.[int(float(l.Length)*0.999)]
                                                               )|> Array.ofSeq
                                                               |> Array.mapi(fun i v -> tenors2compute.[i], v) |> Series.ofObservations
    let spreadat999 = rateat999 - avgcurve

    printfn "avg rates at %f is %A" tstart avgcurve
    printfn "standard devitatoins at %f is %A" tstart stddevcurve
    printfn "rates at 99.9 at %f is %A" tstart rateat999
    printfn "spread at 99.9 at %f is %A" tstart spreadat999


    time.Stop()
    //printfn "%A" time.Elapsed.TotalSeconds
    0 // return an integer exit code
