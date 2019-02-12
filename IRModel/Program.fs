// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open System
open Deedle
open IRModel

[<EntryPoint>]
let main argv = 
    let time = System.Diagnostics.Stopwatch.StartNew()

    let forwardframe = Frame.ReadCsv(@"C:\Users\venus\code\github\IRModels\data\hjm_formatted.csv",true,true)
    let frame:Frame<DateTime,string> = forwardframe |> Frame.indexRows("time") |> Frame.sortRowsByKey
    let hjm = hjmframework(frame,3,1.0,250,200)

    let Nsims = 100000
    let MCforwards = seq {for i in 0 .. Nsims do yield hjm.runPath()}
    
    let l3marr=
        MCforwards |> Seq.mapi(fun counter cube ->                                             
                                            if counter%100 = 0 then
                                                    printfn "%A" (float(counter) / float(Nsims) * 100.0)                                                    
                                            hjm.computeForward(cube)(1.0,5.0)
                                            )
                   |>Array.ofSeq
                   |> Array.sort

    printfn " Libor 3m maxdiff = %A"  ((l3marr.[l3marr.Length-1]-l3marr.[0])*100.0)
    printfn "stdev Libor3m = %A" (MathNet.Numerics.Statistics.Statistics.StandardDeviation(MathNet.Numerics.LinearAlgebra.Vector.Build.DenseOfArray(l3marr)))

    time.Stop()
    //printfn "%A" time.Elapsed.TotalSeconds
    0 // return an integer exit code
