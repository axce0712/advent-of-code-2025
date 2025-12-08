module Pos = struct
  type t = int * int
  let compare = compare
end

module PosMap = Map.Make(Pos)

type node = {
  left  : (int * int) option; 
  right : (int * int) option;
}

type graph = node PosMap.t

let find_start lines =
  let rec aux y lines =
    match lines with
    | l :: ls -> (
      match String.index_opt l 'S' with
      | Some x -> (x, y)
      | None -> aux (y + 1) ls)
    | [] -> failwith "No start found"
  in
  aux 0 lines

let find_splitter_opt (x, y) lines =
  let rec aux y lines =
    match lines with
    | l :: ls ->
      if l.[x] = '^' then
        Some (x, y)
      else
        aux (y + 1) ls
    | [] -> None
  in
  aux (y + 1) (List.drop y lines)

let scan (lines: string list) : (int * int) * graph =
  let rec aux map positions =
    match positions with
    | [] -> map
    | (x, y) :: ps ->
      match PosMap.find_opt (x, y) map with
      | Some _ -> aux map ps
      | None ->
        let left = find_splitter_opt (x - 1, y) lines in
        let right = find_splitter_opt (x + 1, y) lines in
        let new_map = PosMap.add (x, y) { left = left; right = right } map in
        let new_positions =
          match left, right with
          | Some x, Some y -> x :: y :: ps
          | Some x, None -> x :: ps
          | None, Some y -> y :: ps
          | None, None -> ps
        in
        aux new_map new_positions
  in

  let start = find_start lines in

  match find_splitter_opt start lines with
  | Some (x, y) -> (x, y), aux PosMap.empty [ (x, y) ]
  | None -> failwith "Unexpected map"

let count_timelines graph start =
  let memo = Hashtbl.create 128 in
  let rec aux splitter =
    match Hashtbl.find_opt memo splitter with
    | Some x -> x
    | None ->
      let result =
        match PosMap.find_opt splitter graph with
        | Some { left = Some left; right = Some right } -> aux left + aux right
        | Some { left = Some left; right = None } -> 1 + aux left
        | Some { left = None; right = Some right } -> 1 + aux right
        | Some { left = None; right = None } -> 2
        | None -> 0
      in
      Hashtbl.add memo splitter result;
      result
  in
  aux start

let read_lines filename =
  In_channel.with_open_bin filename In_channel.input_lines

let solve_part_one lines =
  let _, splitters = scan lines in
  PosMap.cardinal splitters

let solve_part_two lines =
  let start, splitters = scan lines in
  count_timelines splitters start

let sample =
  ".......S.......\n\
   ...............\n\
   .......^.......\n\
   ...............\n\
   ......^.^......\n\
   ...............\n\
   .....^.^.^.....\n\
   ...............\n\
   ....^.^...^....\n\
   ...............\n\
   ...^.^...^.^...\n\
   ...............\n\
   ..^...^.....^..\n\
   ...............\n\
   .^.^.^.^.^...^.\n\
   ..............."

let lines = String.split_on_char '\n' sample

let () =
  let lines = read_lines "inputs/day07.txt" in
  Printf.printf "Part One: %d\n" (solve_part_one lines);
  Printf.printf "Part Two: %d\n" (solve_part_two lines)
