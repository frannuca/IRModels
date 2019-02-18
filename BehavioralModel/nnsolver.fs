namespace BehavioralModel
open System
open AForge.Neuro
open AForge.Neuro.Learning
open Deedle
open MathNet.Numerics.LinearAlgebra
module nnsolver=

    let Solve()=
        let data = Frame.ReadCsv(@"C:\Users\venus\code\github\IRModels\data\sphere.csv",true,true)
        let datam = data.ToArray2D() |> Matrix<float>.Build.DenseOfArray

        let input =  datam.SubMatrix(0,datam.RowCount,0,2).ToRowArrays()
        let output = datam.Column(2).ToArray() |> Array.map(fun x -> [|x|])
    
        let data2 = Frame.ReadCsv(@"C:\Users\venus\code\github\IRModels\data\sphere_test.csv",true,true)
        let datam2 = data2.ToArray2D() |> Matrix<float>.Build.DenseOfArray

        let input2 =  datam2.SubMatrix(0,datam2.RowCount,0,2).ToRowArrays()
        let output2 = datam2.Column(2).ToArray() |> Array.map(fun x -> [|x|])
    

        // return an integer exit code
        // initialize input and output values

        // create neural network
        let  network = new ActivationNetwork(
                                                SigmoidFunction( 2.0 ),
                                                2, // two inputs in the network
                                                5, // two neurons in the first layer
                                                5,
                                                3,
                                                1 ) // one neuron in the second layer
        // create teacher
        let teacher = new BackPropagationLearning( network )
        // loop
        let err = ref 10000.0
        let mutable counter = 0
        while counter<1000000 do
            counter <- counter + 1
            err := teacher.RunEpoch(input,output)
    
        let expected, computed, errtot = input2 |> Array.mapi(fun i x -> output2.[i].[0],network.Compute(x).[0],100.0*(System.Math.Abs(network.Compute(x).[0] - output2.[i].[0]))/output2.[i].[0]) |> Array.maxBy(fun (_,_,x) -> x)
        printfn "total expected=%f, computed=%f, error = %f" expected computed errtot
        0

