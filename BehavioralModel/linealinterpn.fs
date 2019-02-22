namespace BehavioralModel
open System
open MathNet.Numerics.LinearAlgebra

type LinealInterpn(grid:float[][],f:float array,ndecimals:int)=    
   
    let m_grid = grid 
                    |> Array.map(fun row-> row |> Array.map(fun y -> System.Math.Round(y,ndecimals))) 
                    |> Matrix.Build.DenseOfRowArrays

    let m_axis = [|0 .. m_grid.ColumnCount-1|] 
                    |> Array.map(fun n -> m_grid.Column(n).ToArray() |> Array.distinct|> Array.sort)

    let fval = f
    let Ndim = m_grid.ColumnCount
    
    let locateIndex(xarr:float array, tol:double)(x:float)=
        let rec recursivelocate(nlow:int,nhigh:int)=
            if Math.Abs(nlow-nhigh)<=1 then
                (nlow,nhigh)
            else
                let xlow = xarr.[nlow]
                let xhigh = xarr.[nhigh]
                let nmid = int(float(nlow+nhigh)*0.5)
                let xmid = xarr.[nmid]

                if System.Math.Abs(xlow - x) <= tol then
                    (nlow,nlow)
                else if System.Math.Abs(xhigh - x) <= tol then
                    (nhigh,nhigh)
                else if System.Math.Abs(xmid - x) <= tol then
                    (nmid,nmid)
                else if xmid<x then
                    recursivelocate(nmid,nhigh)
                else
                    recursivelocate(nlow,nmid)

        recursivelocate(0,xarr.Length-1)
    
    member self.interp(x0:float array)=
        let x = x0 |> Vector.Build.DenseOfArray
        let subgrid = x.ToArray() 
                        |> Array.mapi(fun n s -> let (nlow,nhigh) = locateIndex(m_axis.[n],1e-10) x.[n]
                                                 [|m_axis.[n].[nlow];m_axis.[n].[nhigh]|])
 
        
        let points= [|0 .. m_grid.RowCount-1|]
                      |> Array.filter(fun i -> let pv = m_grid.Row(i) 
                                               [0 .. pv.Count-1]
                                               |>Seq.forall(fun k -> pv.[k]<=subgrid.[k].[1] && pv.[k]>=subgrid.[k].[0])
                                      )
                      |> Array.map(fun i-> i,(m_grid.Row(i)-x).L2Norm())
                      |> Array.sortBy(fun (_,s) -> s)
                      |> Array.take(int(2.0**float Ndim))
                      |> Array.map(fun (i,_) -> m_grid.Row(i),f.[i])
             
        let gridnorm = subgrid |> Array.map(fun ax -> (ax.[1]-ax.[0])) |> Array.fold(fun acc g -> g*acc) 1.0

        
        points
            |> Array.map(fun (p,fval) ->(p.ToArray() 
                                         |> Array.mapi(fun k u -> 
                                                                    let ss = fval
                                                                    let a = (subgrid.[k].[1]-subgrid.[k].[0])
                                                                    let b  = Math.Abs(p.[k] - x.[k])
                                                                    let r = a-b
                                                                    r
                                                                    )
                                         |> Array.fold(fun state g -> state*g) 1.0)*fval/gridnorm                                        
                        )
            |> Array.sum
        
        
