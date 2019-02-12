namespace IRModel
open System

module tools=
    let linspace(a:float,b:float,n:int)=
        [| 0 .. (n-1) |] 
         |> Array.map(fun i -> a + float(i)/float(n-1) * (b-a))


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