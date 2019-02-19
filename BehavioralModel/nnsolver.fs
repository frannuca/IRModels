namespace BehavioralModel
open System
open AForge.Neuro
open AForge.Neuro.Learning
open Deedle
open MathNet.Numerics.LinearAlgebra
module nnsolver=

    let Solve()=
        let outputNorm = 20.0
        let data = Frame.ReadCsv(@"C:\Users\venus\code\github\IRModels\data\sphere.csv",true,true)
        let datam = data.ToArray2D() |> Matrix<float>.Build.DenseOfArray
        let nparams = data.ColumnCount-1

        let input =  datam.SubMatrix(0,datam.RowCount,0,nparams)
        let output = datam.Column(data.ColumnCount-1)/outputNorm

        let data2 = Frame.ReadCsv(@"C:\Users\venus\code\github\IRModels\data\sphere_test.csv",true,true)
        let datam2 = data2.ToArray2D() |> Matrix<float>.Build.DenseOfArray

        let input2 =  datam2.SubMatrix(0,datam2.RowCount,0,nparams)
        let output2 = datam2.Column(datam2.ColumnCount-1)/outputNorm

        // return an integer exit code
        // initialize input and output values

        // create neural network
        let  network = new ActivationNetwork(
                                                SigmoidFunction( 2.0 ),
                                                nparams, // two inputs in the network
                                                5, // two neurons in the first layer
                                                5, 
                                                5, 
                                                5, 
                                                1 ) // one neuron in the second layer
        // create teacher
        let teacher = new BackPropagationLearning( network )
        // loop
        let err = ref 10000.0
        let mutable counter = 0
        while counter<10000 do
            counter <- counter + 1
            err := teacher.RunEpoch(input.ToRowArrays(),output.ToColumnMatrix().ToRowArrays())
    
        let expected, computed, errtot = input.ToRowArrays() |> Array.mapi(fun i x -> output.[i],network.Compute(x).[0],100.0*(System.Math.Abs(network.Compute(x).[0] - output.[i]))/output.[i]) |> Array.maxBy(fun (_,_,x) -> x)
        printfn "total expected=%f, computed=%f, error = %f" expected computed errtot
        0

