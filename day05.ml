let read_lines filename =
  let lines = ref [] in
  let ic = open_in filename in
  try
    while true do
      lines := input_line ic :: !lines
    done;
    !lines
  with End_of_file ->
    close_in ic;
    List.rev !lines

let parse (lines: string list) : (int * int) list * int list =
  let split_index = List.find_index ((=) "") lines |> Option.get in
  let ranges_part = List.take split_index lines in
  let ids_part = List.drop (split_index + 1) lines in
  let ranges : (int * int) list  = List.map (fun s -> 
    let split_index = String.index s '-' in
    let lowId = int_of_string (String.sub s 0 split_index) in
    let highId = int_of_string (String.sub s (split_index + 1) (String.length s - split_index - 1)) in
    (lowId, highId)
  ) ranges_part in

  let ids = List.map int_of_string ids_part in
  (ranges, ids)

let count_digits n =
  let rec aux = function 0 -> 0 | n -> 1 + aux (n / 10) in

  match n with 0 -> 1 | n -> aux (abs n)

let int_pow base exp =
  let rec aux acc b = function
    | 0 -> acc
    | e when e mod 2 = 0 -> aux acc (b * b) (e / 2)
    | e -> aux (acc * b) (b * b) (e / 2)
  in
  aux 1 base exp

module IntMap = Map.Make(Int)

let indexed_ranges (ranges: (int * int) list) =
  let rec aux acc ranges =
    match ranges with
    | [] -> acc
    | (low, high) :: rs ->
      let digit_count = count_digits low in
      let key = digit_count in
      let new_acc =
        match IntMap.find_opt key acc with
        | Some lst -> IntMap.add key ((low, high) :: lst) acc
        | None -> IntMap.add key [ (low, high) ] acc
      in
      aux new_acc rs
  in
  aux IntMap.empty ranges

let solve_part_one (ranges: (int * int) list) (ids: int list) =
  let indexed = indexed_ranges ranges in

  ids
  |> List.fold_left (fun acc id ->
    let digit_count = count_digits id in
    match IntMap.find_opt digit_count indexed with
    | Some lst ->
      if List.exists (fun (low, high) -> id >= low && id <= high) lst then (
        acc + 1
      ) else
        acc
    | None -> acc) 0

let sample = "3-5\n10-14\n16-20\n12-18\n\n1\n5\n8\n11\n17\n32" |> String.split_on_char '\n'

let () =
  sample
  |> parse
  |> fun (ranges, ids) -> Printf.printf "Part one: %d\n" (solve_part_one ranges ids)

let () =
  let (ranges, ids) = read_lines "inputs/day05.txt" |> parse in
  Printf.printf "Part one: %d\n" (solve_part_one ranges ids)
