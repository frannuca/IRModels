namespace IRModel
open System
open System.Runtime.CompilerServices
open System.Collections.Generic
open Deedle
open MathNet.Numerics.LinearAlgebra

[<Extension>]
type FrameOps () =
        
    [<Extension>]
    static member inline values(self:Frame<DateTime,string>)=
        self |> Frame.toArray2D |> Matrix.Build.DenseOfArray
       
    [<Extension>]
    static member inline cov(self:Frame<DateTime,string>)=
        let M = self.values()
        let mu = M.ColumnSums()/(float)M.RowCount
        let centeredM = (M.EnumerateColumnsIndexed() 
                            |> Seq.map(fun (i,col) -> (col-mu.[i]).ToArray()) 
                            |>Matrix.Build.DenseOfColumnArrays).Transpose()
                                                   
        centeredM.TransposeAndMultiply(centeredM)/Math.Max(1.0,float(M.RowCount-1))

