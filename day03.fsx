open System.IO

let findHighest (batteryCount: int) (bank: list<int>) : (int * list<list<int>>) =
    let rec imp ((rank, remaining) as acc) list =
        if List.length list < batteryCount then
            acc
        else
            match list with
            | [] -> acc
            | x :: xs when x > rank -> imp (x, [ xs ]) xs
            | x :: xs when x = rank -> imp (rank, xs :: remaining) xs
            | _ :: xs -> imp acc xs

    if List.length bank < batteryCount then
        failwith "Not enough batteries"
    else
        let x :: xs = bank
        imp (x, [ xs ]) xs

let rec largestPossibleJoltage (batteryCount: int) (bank: list<int>) : int64 =
    if batteryCount = 0 then
        0L
    else
        let rank, remaining = findHighest batteryCount bank
        let acc = int64 rank * int64 (pown 10L (batteryCount - 1))

        acc + (remaining |> Seq.map (largestPossibleJoltage (batteryCount - 1)) |> Seq.max)

let solve batteryCount banks =
    banks
    |> Seq.sumBy (largestPossibleJoltage batteryCount)

do
    let banks =
        File.ReadLines "inputs/day03.txt"
        |> Seq.map (fun str ->
            str
            |> Seq.map (fun c -> int c - int '0')
            |> Seq.toList)

    printfn "Part One: %d" (solve 2 banks)
    printfn "Part Two: %d" (solve 12 banks)