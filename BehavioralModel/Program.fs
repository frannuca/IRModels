// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open System
open BehavioralModel
open MathNet.Numerics.LinearAlgebra
open Deedle
open BehavioralModel.Solver

[<EntryPoint>]
let main argv = 

    let grid= Frame.ReadCsv(@"C:\Users\venus\code\github\IRModels\data\grid.csv",true,true) 
                |> Frame.toArray2D 
                |> Matrix<float>.Build.DenseOfArray                

    
    let interpn = LinealInterpn(grid.SubMatrix(0,grid.RowCount,0,grid.ColumnCount-1).ToRowArrays(),grid.Column(grid.ColumnCount-1).ToArray(),8)

    let grid_test= Frame.ReadCsv(@"C:\Users\venus\code\github\IRModels\data\grid_test.csv",true,true) 
                    |> Frame.toArray2D 
                    |> Matrix<float>.Build.DenseOfArray                
    
    
    let gridvals = grid_test.ToRowArrays() |> Array.map(fun r -> interpn.interp(r.[0 .. r.Length-2]),r.[r.Length-1])
    gridvals |> Array.iter(fun (a,b) -> printfn "calculated=%f, expected=%f" a b)

    printfn "///////////////////////////////////////////////////"
    let data = Frame.ReadCsv(@"C:\Users\venus\code\github\IRModels\data\sphere.csv",true,true)
    let datam = data.ToArray2D() |> Matrix<float>.Build.DenseOfArray
    let nparams = data.ColumnCount-1

    let input =  datam.SubMatrix(0,datam.RowCount,0,nparams)
    let output = datam.Column(data.ColumnCount-1)

    let data2 = Frame.ReadCsv(@"C:\Users\venus\code\github\IRModels\data\sphere_test.csv",true,true)
    let datam2 = data2.ToArray2D() |> Matrix<float>.Build.DenseOfArray

    let input2 =  datam2.SubMatrix(0,datam2.RowCount,0,nparams)
    let output2 = datam2.Column(datam2.ColumnCount-1)

    let rbf = new RBFInterpolator(input,output,input2,output2,0.15)
    let c = rbf.SolveExponentialRBFInterpolator(1.0,0.0005,25.0)
    let err = rbf.computeInterpolationError(c)
    rbf.printError(c,1.0)
    printfn "c=%f" c
    0