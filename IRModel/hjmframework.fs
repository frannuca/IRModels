namespace IRModel

open System
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Interpolation
open MathNet.Numerics.Random
open Deedle
open tools
open tenorOps

type hjmframework(historicalForwards:Frame<DateTime,string>,nfactors:int,maxtimeinyears:float,numberoftimesteps:int,numberoftenors:int)=
    let m_history = historicalForwards
    let mc_time = linspace(0.0,maxtimeinyears,numberoftimesteps)    
    let point_tenors = Volatility.extractTenorsFromFrame(historicalForwards) |> Array.sort
    let randy = new MersenneTwister(42); 
    let rndgen = MathNet.Numerics.Distributions.Normal(randy)
    let mc_tenors = linspace(float point_tenors.[0],float point_tenors.[point_tenors.Length-1],numberoftenors)
    let  mutable mc_cube:MathNet.Numerics.LinearAlgebra.Matrix<float> seq option =  None  
    let volatility_interpolators = Volatility.compute_volatility(m_history,nfactors)

    let _computeDrift(tenorinyears:double)=
        volatility_interpolators
        |>Array.map(fun f -> MathNet.Numerics.Integrate.OnClosedInterval(System.Func<float,float>(f),0.0,tenorinyears))
        |>Array.sum
        
    
    
    let mc_vols = mc_tenors
                  |> Array.map(fun t -> 
                                        volatility_interpolators |>
                                        Array.map(fun f -> f(t))
                               )
                  |> MathNet.Numerics.LinearAlgebra.Matrix.Build.DenseOfRowArrays

    let mc_drift =mc_tenors
                  |> Array.map(fun t -> _computeDrift(t) )
    
    let point_todayForward =
        let lastdate = m_history.RowIndex.Keys.[m_history.RowIndex.Keys.Count - 1]
        m_history.GetRow<double>(lastdate) |> Series.values |> Array.ofSeq

    let mc_forwardcurveToday=                                     
                  let f = CubicSpline.InterpolateAkimaSorted(point_tenors |> Array.map(float),point_todayForward)
                  mc_tenors
                  |> Array.map(f.Interpolate)
    
    
    member self.runPath()=
        let current_forward =  Array.zeroCreate(mc_forwardcurveToday.Length)
        let prev_forward =   Array.zeroCreate(mc_forwardcurveToday.Length)
        
        let montecarlomatrix = Matrix<double>.Build.Dense(mc_time.Length,mc_tenors.Length)               
        
        mc_forwardcurveToday.CopyTo(current_forward,0)
        mc_forwardcurveToday.CopyTo(prev_forward,0)
                
        let time = mc_time |> Array.mapi(fun i x -> i,x)
        let tenors = mc_tenors |> Array.mapi(fun i x -> i,x)

        for i,_ in time  do
            if i = 0 then montecarlomatrix.SetRow(0,mc_forwardcurveToday)
            else
                let dt = mc_time.[i]-mc_time.[i-1]
                let sqrt_dt = Math.Sqrt(dt)
                current_forward.CopyTo(prev_forward,0)
                let dZ = Array.zeroCreate(mc_vols.ColumnCount)
                rndgen.Samples(dZ)
                for j,T in tenors do
                    let drift = prev_forward.[j] + mc_drift.[j]*dt
                    let stochastic = dZ 
                                     |>Array.mapi(fun k dz -> mc_vols.[j,k]*dz*sqrt_dt)
                                     |> Array.sum
        
                    let jp,jm = if j<mc_tenors.Length-1 then j+1,j
                                else j,j-1
                    let df_dT = (prev_forward.[jp]-prev_forward.[jm])/(mc_tenors.[jp]-mc_tenors.[jm])*dt

                    current_forward.[j] <- drift+stochastic+df_dT
                
                montecarlomatrix.SetRow(i,current_forward)

        montecarlomatrix

    member self.getInstantaneousForward(cube:Matrix<float>,t:float<year>)(T:float<year>) =
        let t_tol = (mc_time.[1]-mc_time.[0])*1e-3
        let T_tol = ( mc_tenors.[1]-mc_tenors.[0])*1e-3
        let ntlow,nthigh = locateIndex(mc_time, t_tol)(float t)
        let nTlow,nThigh = locateIndex(mc_tenors,T_tol)(float T)

        let x00 = cube.[ntlow,nTlow]
        let x01 = cube.[ntlow,nThigh]
        let x10 = cube.[nthigh,nTlow]
        let x11 = cube.[nthigh,nThigh]

        let dt = mc_time.[nthigh]-mc_time.[ntlow]
        let dT = mc_tenors.[nThigh]-mc_tenors.[nTlow]
        
        let tnorm=
            if Math.Abs(dt)<1e-6 then
                0.0
            else
                (float t-mc_time.[ntlow])/dt
        
        let Tnorm=
            if Math.Abs(dT)<1e-6 then
                0.0
            else
                (float T-mc_tenors.[nTlow])/dT
        
        x00*(1.0-tnorm)*(1.0-Tnorm)+x10*tnorm*(1.0-Tnorm)+x01*(1.0-tnorm)*Tnorm+x11*tnorm*Tnorm


    member private self.computeForwardContinous(cube:Matrix<float>)(t:float<year>, T:float<year>)=
        let fintegrand = fun (s:float) -> self.getInstantaneousForward(cube,t)(s*1.0<year>)
        let I = MathNet.Numerics.Integration.GaussLegendreRule.Integrate(System.Func<float,float>(fintegrand),0.0,float T,16)
        I/T


    member private self.computeForwardCompounded(cube:Matrix<float>)(t:float<year>, T:float<year>)=
        let fintegrand = fun (s:float) -> self.getInstantaneousForward(cube,t)(s*1.0<year>)
        let I = MathNet.Numerics.Integration.GaussLegendreRule.Integrate(System.Func<float,float>(fintegrand),0.0,float T,16)
        (Math.Pow(Math.Exp(I),1.0/float T)-1.0)


    member self.GenerateForwardMCSamples(nsims:int)=
        mc_cube <- Some(seq {for i in 0 .. nsims do yield self.runPath()})
    
    member self.ComputeForwards(tenors2compute:float<month> array,tstart:float<year>)=                                
        match mc_cube with
        |Some(cubes) ->
                        (cubes |> Seq.mapi(fun counter cube ->                                                                                                                                         
                                                            tenors2compute 
                                                            |> Array.map(fun T -> self.computeForwardContinous(cube)(tstart,T*months2years))
                                                            |> floatArray
                                                            )
                                |> Array.ofSeq                   
                                |> MathNet.Numerics.LinearAlgebra.Matrix.Build.DenseOfRowArrays).ToArray()
                                |> Frame.ofArray2D                                
                                |> Frame.indexColsWith(tenors2compute|>Array.map(fun tenor -> sprintf "%Am" (float tenor)))

        |None -> failwith "No internal mc cubes has been calculated, please run GenerateForwardMCSamples"
                