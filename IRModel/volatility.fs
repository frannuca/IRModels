namespace IRModel
open System
open Deedle
open MathNet.Numerics.LinearAlgebra
open tools

//<summary>Volatility PCA calculations and interpolation operations.</summary>
module Volatility=    
    open System.Text.RegularExpressions
    open MathNet.Numerics.Interpolation

    (**<summary> 
    active pattern to detect string notation for tenors, e.g: '1d', '3m', '4w', '9y' to its represenation in years
       <-summary>
       <param name="str"> Input string representing a tenor</param>
       <returns> number of years as a double if the input matches a tenor string, otherwise None</returns>
    **)
    let (|Tenor|_|) str =
        let m = Regex.Match(str,"(^\d*\.?\d*)([dwmy])")
        if m.Success then
            if m.Groups.Count = 3 then
                Some((m.Groups.[1].Value,m.Groups.[2].Value))
            else 
                None
        else
            None


    (**<summary> 
    transforms standard string notation for tenors, e.g: '1d', '3m', '4w', '9y' to its represenation in years
       <-summary>
       <param name="tenor"> Input string representing a tenor</param>
       <returns> number of years as a double if the input matches a tenor string, otherwise None</returns>
       <exception cref="System.Exception">thrown if the input does not match any tenor pattern</exception>
    **)        
    let tenor2years tenor=
        match tenor with
        |Tenor (x,tag) -> match tag with
                            |"d" -> Double.Parse(x)/360.0
                            |"w" -> Double.Parse(x)*7.0/360.0
                            |"m" -> Double.Parse(x)*30.0/360.0
                            |"y" -> Double.Parse(x)
                            |_ -> failwith(sprintf "incorrect format %s" tenor)
        | _ -> failwith(sprintf "incorrect format %s" tenor)
        
    let extractTenorsFromFrame(frame:Frame<DateTime,string>)=
        frame.GetColumns() 
        |> Series.keys
        |> Seq.map(tenor2years)
        |> Array.ofSeq
    (**
    <summary>Given intantaneous forward rates at different tenors it applies PCA to the covariance matrix of
    the 1-day instantaneous forward rates obtaining the n-factor composition of the volatility</summary>
    <param name="history">Deedle frame holding an order ascending date key as order ascending tenor columns</param>
    <param name="nfactor"> number of principal components to be extracted</param>
    <returns> an array of interpolation functions on the n-factors specified in the input </returns>
    **)
    let compute_volatility(history:Frame<DateTime,string>,nfactor:int)=
        let diff_rates = history|> Frame.diff(1)
        let tenorsinyears = extractTenorsFromFrame(history)

        let sigma = diff_rates.cov()
        let eig = sigma.Evd(MathNet.Numerics.LinearAlgebra.Symmetricity.Symmetric)
        let sortedindex = eig.EigenValues.ToArray() 
                            |> Array.mapi(fun i v ->i,v.Real) 
                            |> Array.sortByDescending(fun (_,v) -> v)

        
        let sigmax = sortedindex 
                        |> Array.map(fun (i,v) -> System.Math.Sqrt(v) * eig.EigenVectors.Column(i))
                        |> Matrix.Build.DenseOfColumnVectors
                        
        let sigma_pc = sigmax.SubMatrix(0,eig.EigenVectors.RowCount,0,nfactor)                        
        
        //interpolation
        sigma_pc.EnumerateColumns()
        |> Seq.map(fun c -> 
                    let f = CubicSpline.InterpolateAkimaSorted(tenorsinyears,c.ToArray())
                    fun (t) -> f.Interpolate(t)
                    )
        |>Array.ofSeq

        
    
