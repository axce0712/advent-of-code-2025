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

let get_area (x1, y1) (x2, y2) = (abs (x2 - x1) + 1) * (abs (y2 - y1) + 1)

let solve_part_one tiles =
  tiles |> all_pairs_map (fun t1 t2 -> get_area t1 t2) |> List.fold_left max 0

type horizontalDirection = Left | Right
type verticalDirection = Up | Down
type line = Horizontal of int * (int * int) | Vertical of int * (int * int)

module Line = struct
  let create (x1, y1) (x2, y2) =
    if y1 = y2 then Horizontal (y1, (min x1 x2, max x1 x2))
    else if x1 = x2 then Vertical (x1, (min y1 y2, max y1 y2))
    else failwith "Only horizontal or vertical lines are supported"
end

let scan_lines tiles =
  let rec aux acc = function
    | x :: y :: ys -> aux (Line.create x y :: acc) (y :: ys)
    | [ x ] -> List.rev (Line.create x (List.hd tiles) :: acc)
    | [] -> failwith "Unexpected empty tiles"
  in
  aux [] tiles

let filter_all_pairs_map f lst =
  let rec aux acc = function
    | [] -> acc
    | x :: xs ->
        let pairs = List.filter_map (fun y -> f x y) xs in
        aux (List.rev_append pairs acc) xs
  in
  aux [] lst

let is_intersecting lines ((x1, y1), (x2, y2)) =
  let min_x, max_x = if x1 < x2 then (x1, x2) else (x2, x1) in
  let min_y, max_y = if y1 < y2 then (y1, y2) else (y2, y1) in

  lines
  |> List.exists (function
    | Horizontal (hy, (hx1, hx2)) ->
        min_y < hy && hy < max_y && hx1 < max_x && hx2 > min_x
    | Vertical (vx, (vy1, vy2)) ->
        min_x < vx && vx < max_x && vy1 < max_y && vy2 > min_y)

let solve_part_two tiles =
  let lines = scan_lines tiles in

  tiles
  |> filter_all_pairs_map (fun t1 t2 ->
      if is_intersecting lines (t1, t2) then None else Some (get_area t1 t2))
  |> List.fold_left max 0

let read_lines filename =
  In_channel.with_open_bin filename In_channel.input_lines

let sample = "7,1\n11,1\n11,7\n9,7\n9,5\n2,5\n2,3\n7,3"
let lines = String.split_on_char '\n' sample

let () =
  let tiles = read_lines "inputs/day09.txt" |> List.map parse in
  Printf.printf "Part One: %d\n" (solve_part_one tiles);
  Printf.printf "Part Two: %d\n" (solve_part_two tiles)
