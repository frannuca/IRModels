namespace IRModel
open System

module tenorOps=                

    [<Measure>] type day
    [<Measure>] type month
    [<Measure>] type year

    let days2years:float<year/day> =   1.0<year>/360.0<day>
    let months2years:float<year/month> = 1.0<year>/12.0<month>
    let years2months:float<month/year> = 12.0<month>/1.0<year>
        

    let floatArray(x: 'T array)= x |> Array.map(fun v ->  System.Convert.ToDouble(v))