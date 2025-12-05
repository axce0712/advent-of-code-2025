type range = { low : int; high : int }

module Range = struct
  type t = range

  let create low high = { low; high }
  let contains id r = id >= r.low && id <= r.high
end

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

let parse (lines : string list) : range list * int list =
  let split_index = List.find_index (( = ) "") lines |> Option.get in
  let ranges_part = List.take split_index lines in
  let ids_part = List.drop (split_index + 1) lines in
  let ranges =
    List.map
      (fun s ->
        let split_index = String.index s '-' in
        let lowId = int_of_string (String.sub s 0 split_index) in
        let highId =
          int_of_string
            (String.sub s (split_index + 1) (String.length s - split_index - 1))
        in
        { low = lowId; high = highId })
      ranges_part
  in
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

let solve_part_one (ranges : range list) (ids : int list) : int =
  ids
  |> List.fold_left
       (fun acc id ->
         if List.exists (Range.contains id) ranges then acc + 1 else acc)
       0

let sample =
  "3-5\n10-14\n16-20\n12-18\n\n1\n5\n8\n11\n17\n32" |> String.split_on_char '\n'

let () =
  let ranges, ids = sample |> parse in
  Printf.printf "Part one: %d\n" (solve_part_one ranges ids)

let () =
  let ranges, ids = read_lines "inputs/day05.txt" |> parse in
  Printf.printf "Part one: %d\n" (solve_part_one ranges ids)
