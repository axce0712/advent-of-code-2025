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

let scan (arr: char [,]) =
    let set = HashSet<(int * int)>()

    for y in 0 .. Array2D.length1 arr - 1 do
        for x in 0 .. Array2D.length2 arr - 1 do
            if arr.[y, x] = '@' then
                let rollCount =
                    directions
                    |> List.sumBy (fun (dx, dy) ->
                        if not <| canAccess (dx, dy) (x, y) arr then
                            1
                        else
                            0)

                if rollCount < 4 then
                    set.Add(x, y) |> ignore

    Set.ofSeq set

let solvePartOne (arr: char [,]) = arr |> scan |> Set.count

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
