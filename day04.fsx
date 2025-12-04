open System.IO
open System.Collections.Generic

let canAccess (dx, dy) (x, y) (arr: char [,]) =
    let nx, ny = x + dx, y + dy

    if nx < 0
       || ny < 0
       || nx >= Array2D.length2 arr
       || ny >= Array2D.length1 arr then
        true
    else
        match arr.[ny, nx] with
        | '@' -> false
        | '.' -> true
        | 'x' -> true
        | _ -> failwith "Invalid character"

let directions =
    [ (1, 0)
      (0, 1)
      (-1, 0)
      (0, -1)
      (1, 1)
      (-1, 1)
      (1, -1)
      (-1, -1) ]

let isRollAvailable (arr: char [,]) (x, y) =
    let rollCount =
        directions
        |> List.sumBy (fun (dx, dy) ->
            if not <| canAccess (dx, dy) (x, y) arr then
                1
            else
                0)

    rollCount < 4

let scan (arr: char [,]) =
    let set = HashSet<(int * int)>()

    for y in 0 .. Array2D.length1 arr - 1 do
        for x in 0 .. Array2D.length2 arr - 1 do
            if arr.[y, x] = '@' && isRollAvailable arr (x, y) then
                set.Add(x, y) |> ignore

    Set.ofSeq set

let solvePartOne (arr: char [,]) = arr |> scan |> Set.count

let findNextRolls (arr: char [,]) (x, y) =
    directions
    |> List.fold
        (fun acc (dx, dy) ->
            if not <| canAccess (dx, dy) (x, y) arr then
                acc |> Set.add (x + dx, y + dy) 
            else
                acc
        )
        Set.empty

let solvePartTwo (arr: char [,]) =
    let copy = Array2D.copy arr
    let mutable availableRolls = scan copy
    let mutable removed = Set.count availableRolls
    while Set.count availableRolls > 0 do
        for x, y in availableRolls do
            copy[y, x] <- 'x'

        availableRolls <-
            availableRolls
            |> Seq.collect (findNextRolls copy)
            |> Seq.filter (isRollAvailable copy)
            |> Set.ofSeq

        removed <- removed + Set.count availableRolls

    removed

let sample =
    "..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@."

let input = File.ReadAllLines "inputs/day04.txt" |> array2D

input |> solvePartOne
input |> solvePartTwo
