namespace BehavioralModel
open System
open AForge.Neuro
open AForge.Neuro.Learning
open Deedle
open MathNet.Numerics.LinearAlgebra
open Accord.Math.Optimization
module Solver=    
    
    type RBFInterpolator(input:Matrix<float>, output:Vector<float>,input2:Matrix<float>, output2:Vector<float>)=
    
        let kernel(c:float)(x:Vector<float>,y:Vector<float>)=
            Math.Exp(c*(x-y).L2Norm())

        let Phi(c) = [| for i in 0 .. input.RowCount-1 do 
                        yield [|for j in 0 .. input.RowCount-1 do
                                yield kernel(c)(input.Row(i),input.Row(j)) |]
                     |] |> Matrix.Build.DenseOfRowArrays
        
        let calibrateKernel(c)=                       
             let W = Phi(c).Inverse()*output.ToColumnMatrix()
             W.Column(0)
 
        let interp (c:float,W:Vector<float>)(a:Vector<float>)=        
                 let phi =[|0 .. input.RowCount-1|] 
                            |> Array.map(fun n -> kernel(c)(input.Row(n),a))
                            |> Vector.Build.DenseOfArray
                 (phi.ToRowMatrix()*W.ToColumnMatrix()).[0,0]

    
        member self.computeInterpolationError(c:float)=
           
            let W = calibrateKernel(c)
            let finterp = interp(c,W)
            let e = [|0 .. input2.RowCount-1|] 
                    |> Array.map(fun n -> 
                                            let computed = finterp(input2.Row(n))
                                            let expected = output2.[n]
                                            Math.Abs(computed-expected)                                   
                                            )
                    |> Array.sum
           
            if e <> e then
                printf "c=%f" c
            e

        member self.printError(c:float)=
            let W = calibrateKernel(c)
            let finterp = interp(c,W)
            [|0 .. input2.RowCount-1|] 
            |> Array.iter(fun n -> 
                                    let computed = finterp(input2.Row(n))
                                    let expected = output2.[n]
                                    printfn "computed=%f, expected=%f, rel err =%f" computed expected ((computed-expected)/computed*100.0)
                                    )
            
        member self.SolveExponentialRBFInterpolator(c0:float,cmin:float,cmax:float)=     
        
            let fiterp = fun (c:float array) -> self.computeInterpolationError(c.[0])
            let f = new NonlinearObjectiveFunction(1,fiterp)
            
            let constraintarr = [|new NonlinearConstraint(f,System.Func<float[],float>(fun x -> x.[0]),ConstraintType.GreaterThanOrEqualTo,cmin,System.Func<float[],float[]>(fun x -> [|1.0|]),1e-4)|]
            let cobyla = new Cobyla(f,constraintarr);

            let sucess = cobyla.Minimize([|c0|])
            cobyla.Solution.[0]