type range = { low : int; high : int }

module Range = struct
  type t = range

  let create low high =
    if low > high then invalid_arg "Invalid range" else { low; high }

  let compare r1 r2 =
    if r1.low <> r2.low then Int.compare r1.low r2.low
    else Int.compare r1.high r2.high

  let contains id r = id >= r.low && id <= r.high
  let is_overlapping r1 r2 = r1.low <= r2.high && r2.low <= r1.high
  let count r = r.high - r.low + 1
end

let read_lines filename =
  In_channel.with_open_bin filename In_channel.input_lines

let parse_range s =
  match String.split_on_char '-' s with
  | [ low; high ] -> Range.create (int_of_string low) (int_of_string high)
  | _ -> invalid_arg ("Invalid range: " ^ s)

let parse lines =
  let rec split acc = function
    | "" :: rest -> (List.rev acc, rest)
    | l :: rest -> split (l :: acc) rest
    | [] -> invalid_arg "parse: missing blank line"
  in
  let ranges_part, ids_part = split [] lines in
  let ranges = List.map parse_range ranges_part in
  let ids = List.map int_of_string ids_part in
  (ranges, ids)

let solve_part_one (ranges : range list) (ids : int list) : int =
  ids
  |> List.fold_left
       (fun acc id ->
         if List.exists (Range.contains id) ranges then acc + 1 else acc)
       0

let solve_part_two ranges =
  let sorted = List.sort Range.compare ranges in

  let merged =
    List.fold_left
      (fun acc r ->
        match acc with
        | [] -> [ r ]
        | last :: rest ->
            if Range.is_overlapping last r then
              let merged_range =
                Range.create (min last.low r.low) (max last.high r.high)
              in
              merged_range :: rest
            else r :: acc)
      [] sorted
  in

  merged |> List.fold_left (fun acc r -> acc + Range.count r) 0

let sample =
  "3-5\n10-14\n16-20\n12-18\n\n1\n5\n8\n11\n17\n32" |> String.split_on_char '\n'

let () =
  let ranges, ids = read_lines "inputs/day05.txt" |> parse in
  Printf.printf "Part one: %d\n" (solve_part_one ranges ids);
  Printf.printf "Part two: %d\n" (solve_part_two ranges)
