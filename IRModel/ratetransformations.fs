namespace IRModel
open System
open Deedle
open tenorOps
module Ratetransformations=
    open System.Collections.Generic

    let fromZerostoInstantaneousForward(frame:Frame<DateTime,string>)=
        frame 
        |> Frame.mapRows(fun date row -> 
                                let x = row.Keys |>  Seq.map(Volatility.tenor2years) |> Array.ofSeq 
                                let y = row.Values |> Array.ofSeq |> Array.map(fun s -> Math.Log(unbox(s)))
                                let interp=MathNet.Numerics.Interpolation.CubicSpline.InterpolateAkimaSorted(floatArray x,y)
                                
                                let df = x |> Array.map(fun z -> -interp.Differentiate(float z))
                                row.Observations |> Seq.mapi(fun i k -> k.Key,df.[i])
                                |>Series.ofObservations
                               )
        |>Frame.ofRows
           

    let fromContinuousRatetoZeros(frame:Frame<DateTime,string>)=
        frame 
        |> Frame.mapRows(fun date row -> 
                                let x = row.Keys |>  Seq.map(Volatility.tenor2years) |> Array.ofSeq 
                                let y = row.GetValues() |> Array.ofSeq 
                                                   |> Array.mapi(fun i s -> 
                                                                            let v:float =  s
                                                                            Math.Exp(-v* float x.[i]))                               
                                row.Observations |> Seq.mapi(fun i k -> k.Key,y.[i])
                                |>Series.ofObservations
                                )
        |>Frame.ofRows
        
    let fromZerostoContinuousRate(frame:Frame<DateTime,string>)=
        frame 
        |> Frame.mapRows(fun date row -> 
                                let x = row.Keys |>  Seq.map(Volatility.tenor2years) |> Array.ofSeq 
                                let y = row.Values |> Array.ofSeq 
                                                   |> Array.mapi(fun i s -> -Math.Log(unbox(s))/x.[i])                               
                                row.Observations |> Seq.mapi(fun i k -> k.Key,y.[i])
                                |>Series.ofObservations
                                )
        |>Frame.ofRows
        