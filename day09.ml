let parse line =
  let idx = String.index line ',' in
  let x = String.sub line 0 idx |> int_of_string in
  let y =
    String.sub line (idx + 1) (String.length line - idx - 1) |> int_of_string
  in
  (x, y)

let all_pairs_map f lst =
  let rec aux acc = function
    | [] -> acc
    | x :: xs ->
        let pairs = List.map (fun y -> f x y) xs in
        aux (List.rev_append pairs acc) xs
  in
  aux [] lst

let solve_part_one tiles =
  tiles
  |> all_pairs_map (fun (x1, y1) (x2, y2) -> (abs (x2 - x1) + 1) * (abs (y2 - y1) + 1))
  |> List.fold_left max 0

let read_lines filename =
  In_channel.with_open_bin filename In_channel.input_lines

let sample = "7,1\n11,1\n11,7\n9,7\n9,5\n2,5\n2,3\n7,3"
let lines = String.split_on_char '\n' sample

let () =
  let tiles = read_lines "inputs/day09.txt" |> List.map parse in
  Printf.printf "Part One: %d\n" (solve_part_one tiles);