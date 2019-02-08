namespace IRModel

open System
open MathNet.Numerics.LinearAlgebra
open Deedle
open tools
open MathNet.Numerics
open MathNet.Numerics
open MathNet.Numerics.Interpolation
open MathNet.Numerics.Random
open Deedle
open Deedle
open Deedle
open Deedle
open Deedle
open System.IO

type hjmframework(historicalForwards:Frame<DateTime,string>,nfactors:int,maxtimeinyears:float,numberoftimesteps:int,numberoftenors:int)=
    let m_history = historicalForwards
    let mc_time = linspace(0.0,maxtimeinyears,numberoftimesteps)    
    let point_tenors = Volatility.extractTenorsFromFrame(historicalForwards) |> Array.sort
    
    let mc_tenors = linspace(point_tenors.[0],point_tenors.[point_tenors.Length-1],numberoftenors)

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
                  let f = CubicSpline.InterpolateAkimaSorted(point_tenors,point_todayForward)
                  mc_tenors
                  |> Array.map(f.Interpolate)
    
    
    member self.runPath()=
        let current_forward =  Array.zeroCreate(mc_forwardcurveToday.Length)
        let prev_forward =   Array.zeroCreate(mc_forwardcurveToday.Length)
        
        let montecarlomatrix = Matrix<double>.Build.Dense(mc_time.Length,mc_tenors.Length)
        let randy = new MersenneTwister(42);        
        
        mc_forwardcurveToday.CopyTo(current_forward,0)
        mc_forwardcurveToday.CopyTo(prev_forward,0)

        let rndgen = MathNet.Numerics.Distributions.Normal(randy)
        
        let time = mc_time |> Array.mapi(fun i x -> i,x)
        let tenors = mc_tenors |> Array.mapi(fun i x -> i,x)

        for i,t in time  do
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

                    current_forward.[i] <- drift+stochastic+df_dT
                
                montecarlomatrix.SetRow(i,current_forward)

        