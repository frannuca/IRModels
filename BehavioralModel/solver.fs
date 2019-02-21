namespace BehavioralModel
open System
open AForge.Neuro
open AForge.Neuro.Learning
open Deedle
open MathNet.Numerics.LinearAlgebra
open Accord.Math.Optimization
module Solver=    
    
    type RBFInterpolator(input0:Matrix<float>, output0:Vector<float>,input2:Matrix<float>, output2:Vector<float>,ratio:float)=
    
        let Nsamples = int(float(input0.RowCount)*ratio)
        let input_rbf = input0.SubMatrix(0,Nsamples,0,input0.ColumnCount)
        let input_c =  input0.SubMatrix(Nsamples,input0.RowCount-Nsamples,0,input0.ColumnCount)

        let output_rbf = output0.SubVector(0,Nsamples)
        let output_c =  output0.SubVector(Nsamples,output0.Count-Nsamples)


        let kernelxxxxxxxxxxxx(c:float)(x:Vector<float>,y:Vector<float>)=
            Math.Exp(c*(x-y).L2Norm())

        let kernelxxxxxxx(c:float)(x:Vector<float>,y:Vector<float>)=
            Math.Sqrt(c**2.0 + (x-y).L2Norm())

        let kernelxxxxxxxxxxxxx(c:float)(x:Vector<float>,y:Vector<float>)=
            1.0/Math.Sqrt(c**2.0 + (x-y).L2Norm())
        
        let kernel(c:float)(x:Vector<float>,y:Vector<float>)=
            1.0/Math.Sqrt(1.0 + (c*(x-y).L2Norm())**2.0)

        let kernelxxxxxxxxxxxxxxxx(c:float)(x:Vector<float>,y:Vector<float>)=
            1.0/(1.0+c**2.0*(x-y).L2Norm()**2.0)

        let kernelxxxxxxxxxxxxxxxxx(c:float)(x:Vector<float>,y:Vector<float>)=
            (x-y).L2Norm()*c

        let kernelxxxxxxxxxxxxxxxx(c:float)(x:Vector<float>,y:Vector<float>)=
            (x-y).L2Norm()**3.0
            
        let Phi(c) = [| for i in 0 .. input_rbf.RowCount-1 do 
                        yield [|for j in 0 .. input_rbf.RowCount-1 do
                                yield kernel(c)(input_rbf.Row(i),input_rbf.Row(j)) |]
                     |] |> Matrix.Build.DenseOfRowArrays
        
        let calibrateKernel(c)=                       
             let W = Phi(c).Inverse()*output_rbf.ToColumnMatrix()
             W.Column(0)
 
        let interp (c:float,W:Vector<float>)(a:Vector<float>)=        
                 let phi =[|0 .. input_rbf.RowCount-1|] 
                            |> Array.map(fun n -> kernel(c)(input_rbf.Row(n),a))
                            |> Vector.Build.DenseOfArray
                 (phi.ToRowMatrix()*W.ToColumnMatrix()).[0,0]

    
        member self.computeInterpolationError(c:float)=
           
            let W = calibrateKernel(c)
            let finterp = interp(c,W)
            let e = [|0 .. input_c.RowCount-1|] 
                    |> Array.map(fun n -> 
                                            let computed = finterp(input_c.Row(n))
                                            let expected = output_c.[n]
                                            Math.Abs(computed-expected)                                   
                                            )
                    |> Array.sum
           
            if e <> e then
                printf "c=%f" c
            e 

        member self.printError(c:float,minrelerror_perc)=
            let W = calibrateKernel(c)
            let finterp = interp(c,W)
            let sim =
                [|0 .. input2.RowCount-1|] 
                |> Array.map(fun n -> 
                                        let computed = Math.Exp(finterp(input2.Row(n)))/1e6
                                        let expected = Math.Exp(output2.[n])/1e6    
                                        let relerr = Math.Abs((computed-expected)/(computed+1.0)*100.0)  
                                        
                                        computed, expected, relerr
                                        )
                |>Array.filter(fun (_,_,x) -> x > minrelerror_perc)
                
            
            
            sim |> Array.iter(fun (computed,expected,relerr) -> printfn "computed=%f, expected=%f, rel err =%f" computed expected relerr)    
            printfn "total = %i, faulty=%i, ratio=%f" input2.RowCount sim.Length (100.0*float(sim.Length)/float(input2.RowCount))

        member self.SolveExponentialRBFInterpolator(c0:float,cmin:float,cmax:float)=     
        
            let fiterp = fun (c:float array) -> self.computeInterpolationError(c.[0])
            let f = new NonlinearObjectiveFunction(1,fiterp)
            
            let constraintarr = [|new NonlinearConstraint(f,System.Func<float[],float>(fun x -> x.[0]),ConstraintType.GreaterThanOrEqualTo,cmin,System.Func<float[],float[]>(fun x -> [|1.0|]),1e-4);
                                  new NonlinearConstraint(f,System.Func<float[],float>(fun x -> x.[0]),ConstraintType.LesserThanOrEqualTo,cmax,System.Func<float[],float[]>(fun x -> [|1.0|]),1e-4)|]
            let cobyla = new Cobyla(f,constraintarr);

            let sucess = cobyla.Minimize([|c0|])
            cobyla.Solution.[0]
