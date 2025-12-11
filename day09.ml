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

type line =
  | Horizontal of int * (int * int) * horizontalDirection
  | Vertical of int * (int * int) * verticalDirection

type area = { start_pos : int * int; end_pos : int * int }

module Line = struct
  let create (x1, y1) (x2, y2) =
    if y1 = y2 then Horizontal (y1, (min x1 x2, max x1 x2), if x1 < x2 then Right else Left)
    else if x1 = x2 then Vertical (x1, (min y1 y2, max y1 y2), if y1 < y2 then Down else Up)
    else failwith "Only horizontal or vertical lines are supported"
end

module Area = struct
  let evaluate area =
    let x1, y1 = area.start_pos in
    let x2, y2 = area.end_pos in
    get_area (x1, y1) (x2, y2)
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

let area_of_lines_opt l1 l2 =
  match (l1, l2) with
  | Vertical (x, (y1, y2), Up), Horizontal (y, (x1, x2), Right)
  | Horizontal (y, (x1, x2), Right), Vertical (x, (y1, y2), Down)
  | Vertical (x, (y1, y2), Down), Horizontal (y, (x1, x2), Left)
  | Horizontal (y, (x1, x2), Left), Vertical (x, (y1, y2), Up) ->
      if (y = y1 || y = y2) && (x = x1 || x = x2) then
        let min_x, max_x = (min x x1, max x x2) in
        let min_y, max_y = (min y y1, max y y2) in
        Some { start_pos = (min_x, min_y); end_pos = (max_x, max_y) }
      else None
  | _ -> None

let intercepts area line =
  let ax1, ay1 = area.start_pos in
  let ax2, ay2 = area.end_pos in

  match line with
  | Vertical (x, (y1, y2), _) ->
      if y2 < ay1 || y1 > ay2 then false
      else ax1 < x && x < ax2 && (ay1 < y2 || y1 < ay2)
  | Horizontal (y, (x1, x2), _) ->
      if x2 < ax1 || x1 > ax2 then false
      else
        let ax1, ay1 = area.start_pos in
        let ax2, ay2 = area.end_pos in
        ay1 < y && y < ay2 && (ax1 < x2 || x1 < ax2)

let solve_part_two tiles =
  let lines = scan_lines tiles in

  lines
  |> filter_all_pairs_map (fun l1 l2 -> area_of_lines_opt l1 l2)
  |> List.filter (fun area -> List.for_all (fun line -> not (intercepts area line)) lines)
(* |> List.fold_left (fun acc area -> max acc (Area.evaluate area)) 0 *)

let read_lines filename =
  In_channel.with_open_bin filename In_channel.input_lines

let sample = "7,1\n11,1\n11,7\n9,7\n9,5\n2,5\n2,3\n7,3"
let lines = String.split_on_char '\n' sample

let () =
  let tiles = read_lines "inputs/day09.txt" |> List.map parse in
  Printf.printf "Part One: %d\n" (solve_part_one tiles);
  Printf.printf "Part Two: %d\n" (solve_part_two tiles)
  
  
432311381
107733510
129351348
