namespace algorithms

module Mathy = 

    let dot (v1: seq<float>) v2 =
      Seq.map2 (*) v1 v2
      |> Seq.reduce (+)

    let length v =
      dot v v
      |> sqrt

    let angleBetween v1 v2 =
      let numerator = dot v1 v2
      let denominator = (length v1) * (length v2)
      numerator / denominator |> acos

