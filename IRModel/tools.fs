namespace IRModel

module tools=
    let linspace(a:float,b:float,n:int)=
        [| 0 .. (n-1) |] 
         |> Array.map(fun i -> a + float(i)/float(n-1) * (b-a))


