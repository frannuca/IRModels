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
    let uszero = Ratetransformations.fromCompoundingRatetoDiscount(ustreasury);
    uszero.SaveCsv("C:/temp/discount.csv",true)
    let usinstantaneousforward = Ratetransformations.fromDiscounttoInstantaneousForward(uszero)
    usinstantaneousforward.SaveCsv("C:/temp/usinstantaneousforward.csv",true)
    
    let hjm = hjmframework(usinstantaneousforward,3,1.0,360,12*30)

    let Nsims = 10000
    hjm.GenerateForwardMCSamples(Nsims)
    let forwards = hjm.ComputeForwards([|1.0<month>; 3.0<month>|],0.25<year>)
  
    let avgcurve = forwards |> Stats.mean                            
    let stddevcurve =   forwards |> Stats.stdDev
    forwards.SaveCsv("C:/temp/forwards.csv",true)
    //let spreadat999 = rateat999 - avgcurve

    //printfn "avg rates at %f is %A" tstart avgcurve
    //printfn "standard devitatoins at %f is %A" tstart stddevcurve
    //printfn "rates at 99.9 at %f is %A" tstart rateat999
    //printfn "spread at 99.9 at %f is %A" tstart spreadat999


    time.Stop()
    //printfn "%A" time.Elapsed.TotalSeconds
    0 // return an integer exit code
