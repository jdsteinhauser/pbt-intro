open System
open FsCheck
open algorithms.Mathy

let unwrap (NonEmptyArray a) = Array.map float a

module VectorLengthProperties = 
    let nonnegative (vn:NonEmptyArray<NormalFloat>) = 
        let v = unwrap vn
        length v >= 0.0

    let geqLargestComponent (vn:NonEmptyArray<NormalFloat>) = 
        let v = unwrap vn in (length v >= Seq.maxBy (fun x -> abs x) v)

module DotProperties =
    let commutative (v1n: NonEmptyArray<NormalFloat>) (v2n: NonEmptyArray<NormalFloat>) =
        let v1 = unwrap v1n
        let v2 = unwrap v2n
        dot v1 v2 = dot v2 v1

    let distributive (v1n: NonEmptyArray<NormalFloat>) (v2n: NonEmptyArray<NormalFloat>) (v3n: NonEmptyArray<NormalFloat>) =
        let v1 = unwrap v1n
        let v2 = unwrap v2n
        let v3 = unwrap v3n
        dot v1 (Seq.map2 (+) v2 v3) = dot v1 v2 + dot v1 v3

module AngleBetweenProperties =
    let boundedTwoPi (v1n: NonEmptyArray<NormalFloat>) (v2n: NonEmptyArray<NormalFloat>) =
        let v1 = unwrap v1n
        let v2 = unwrap v2n
        let value = angleBetween v1 v2
        0.0 <= value && value <= 2.0 * Math.PI

module NotMathyMathProperties =
    let toUpperDist (NonEmptyString s1) (NonEmptyString s2) =
        s1.ToUpper() + s2.ToUpper() = (s1 + s2).ToUpper()
    let trimIdempotent (NonEmptyString s) = s.Trim() = s.Trim().Trim()
    let sortIdempotent (xs: int[]) = xs |> Array.sort |> Array.sort = Array.sort xs

module OtherProperties =
    let bilboList (xs: int list) = xs |> List.rev |> List.rev = xs
    let dontGoChangin (xs: int list) = xs |> List.sort |> List.length = List.length xs
    let properSort (xs: int list) = xs |> List.sort |> List.pairwise |> Seq.forall (fun (x,y) -> x <= y)

[<EntryPoint>]
let main argv =
    printfn "Vector Length is non-negative: "
    Check.Quick VectorLengthProperties.nonnegative
    printfn "Vector Length is >= the largest component: "
    Check.Quick VectorLengthProperties.geqLargestComponent

    Console.ReadKey() |> ignore

    printfn "dot product is commutative: "
    Check.Quick DotProperties.commutative
    printfn "dot product is distributive with addition (this one fails): "
    Check.Quick DotProperties.distributive

    Console.ReadKey() |> ignore

    printfn "Angle between to vectors is always between 0 and 360"
    Check.Quick AngleBetweenProperties.boundedTwoPi

    Console.ReadKey() |> ignore

    printfn "String.ToUpper() is distributive: "
    Check.Quick NotMathyMathProperties.toUpperDist

    printfn "String.Trim() is idempotent: "
    Check.Quick NotMathyMathProperties.trimIdempotent

    printfn "Array.sort is idempotent: "
    Check.Quick NotMathyMathProperties.sortIdempotent

    printfn "Double reverse is original list: "
    Check.Quick OtherProperties.bilboList

    printfn "Sorting doesn't change length: "
    Check.Quick OtherProperties.dontGoChangin

    printfn "Sorting puts things in ascending order: "
    Check.Quick OtherProperties.properSort

    0 // return an integer exit code