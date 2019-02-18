namespace IRModel
open tenorOps

module bootstrap=
    type Bond={coupon:float;frequency:float<month>;maturity:float<year>}

