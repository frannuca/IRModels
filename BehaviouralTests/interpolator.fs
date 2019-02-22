namespace Tests

open NUnit.Framework
open System
open MathNet.Numerics.LinearAlgebra
open BehavioralModel
open System.Linq

[<TestFixture>]
type TestClass () =

    let ndim = 3;
    let rng = new MathNet.Numerics.Random.MersenneTwister()
    let dist = new MathNet.Numerics.Distributions.ContinuousUniform(0.02,0.8,rng)
    
    let beta = [|30.0;15.0;18.0|] |> Vector<float>.Build.DenseOfArray
    [<SetUp>]
    member this.Setup () =
        ()
    
    [<Test>]
    member this.Test_ugrid () =
        
        //create 3d grid
        
        let dx = 0.1
        let dy = 0.1
        let dz = 0.1
        let grid = Matrix<float>.Build.Dense(1000,3)
        let counter = ref 0
        for i in 0 .. 9 do 
             for j in 0 .. 9 do  
                    for k in 0 .. 9 do 
                           let aa=[|float i*dx;float j*dy;float k*dz|] |> Vector<double>.Build.DenseOfArray
                           grid.SetRow(!counter,aa)
                           counter := !counter + 1
                 

        
        let feval(x:Vector<float>) = 
            x*beta

        let fs = [|0 .. grid.RowCount-1|] |> Array.map(fun s -> feval(grid.Row(s))) 
        let interp = new LinealInterpn(grid.ToRowArrays(),fs,5)

        let a = interp.interp([|0.6127997681; 0.585970267; 0.7930092074|])
        let ar = feval([|0.6127997681; 0.585970267; 0.7930092074|] |> Vector<float>.Build.DenseOfArray)

        let a = interp.interp([|0.6127997681; 0.585970267; 0.7830092074|])
        let ar = feval([|0.6127997681; 0.585970267; 0.7830092074|] |> Vector<float>.Build.DenseOfArray)

        let err = 
                    seq {for n in 0 .. 1000 do 
                           let x = Array.create 3 0.0
                           dist.Samples(x)
                           let fv = feval(x |> Vector<float>.Build.DenseOfArray)
                           let fi =interp.interp(x)
                           let r = x,fv,fi,(Math.Abs((fv-fi)/fv)*100.0)
                           
                           yield r
                         }
         
        printfn "Max Error=%A" (err |> Array.ofSeq |> Array.maxBy(fun (_,_,_,a)->a))
        
        Assert.Pass()
