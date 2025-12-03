open System
open System.IO

let findHighest (batteryCount: int) (bank: list<int>) : (int * list<int>) =
    ArgumentOutOfRangeException.ThrowIfNegativeOrZero batteryCount
    let rec loop (x, xs) remaining =
        if List.length remaining < batteryCount then
            x, xs
        else
            let y :: ys = remaining
            let acc = if y > x then y, ys else x, xs
            loop acc ys

    if List.length bank < batteryCount then
        failwith "Not enough batteries"
    else
        let x :: xs = bank
        loop (x, xs) xs

let largestPossibleJoltage (batteryCount: int) (bank: list<int>) : int64 =
    let rec loop acc (count: int) list =
        if count = 0 then
            acc
        else
            let rank, remaining = findHighest count list
            let newAcc = acc + int64 rank * pown 10L (count - 1)
            loop newAcc (count - 1) remaining

    loop 0 batteryCount bank

let solve batteryCount banks =
    banks
    |> Seq.sumBy (largestPossibleJoltage batteryCount)

let banks =
    File.ReadLines "inputs/day03.txt"
    |> Seq.map (fun str ->
        str
        |> Seq.map (fun c -> int c - int '0')
        |> Seq.toList)
    |> Seq.toList

printfn "Part One: %d" (solve 2 banks)
printfn "Part Two: %d" (solve 12 banks)