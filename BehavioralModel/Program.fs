// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open System
open BehavioralModel
open MathNet.Numerics.LinearAlgebra
open Deedle
open BehavioralModel.Solver

[<EntryPoint>]
let main argv = 

    let data = Frame.ReadCsv(@"C:\Users\venus\code\github\IRModels\data\sphere.csv",true,true)
    let datam = data.ToArray2D() |> Matrix<float>.Build.DenseOfArray

    let input =  datam.SubMatrix(0,datam.RowCount,0,2)
    let output = datam.Column(2)

    let data2 = Frame.ReadCsv(@"C:\Users\venus\code\github\IRModels\data\sphere_test.csv",true,true)
    let datam2 = data2.ToArray2D() |> Matrix<float>.Build.DenseOfArray

    let input2 =  datam2.SubMatrix(0,datam2.RowCount,0,2)
    let output2 = datam2.Column(2)

    let rbf = new RBFInterpolator(input,output,input2,output2)
    let c = rbf.SolveExponentialRBFInterpolator(1.0,0.2,5.0)
    let err = rbf.computeInterpolationError(c)
    rbf.printError(c)
    printfn "c=%f" c
    0