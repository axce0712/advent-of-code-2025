type range = { low : int; high : int }

module Range = struct
  type t = range

  let create low high =
    if low > high then failwith "Invalid range" else { low; high }

  let compare r1 r2 =
    if r1.low <> r2.low then Int.compare r1.low r2.low
    else Int.compare r1.high r2.high

  let contains id r = id >= r.low && id <= r.high

  let isOverlapping r1 r2 =
    (r1.low >= r2.low && r1.low <= r2.high + 1)
    || (r2.low >= r1.low && r2.low <= r1.high + 1)
    || (r1.high >= r2.low - 1 && r1.high <= r2.high)
    || (r2.high >= r1.low - 1 && r2.high <= r1.high)

  let merge (r1 : range) (r2 : range) : range =
    if isOverlapping r1 r2 then create (min r1.low r2.low) (max r1.high r2.high)
    else failwith "Ranges do not overlap"

  let count r = r.high - r.low + 1
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

let solve_part_one (ranges : range list) (ids : int list) : int =
  ids
  |> List.fold_left
       (fun acc id ->
         if List.exists (Range.contains id) ranges then acc + 1 else acc)
       0

let solve_part_two (ranges : range list) =
  let rec aux range remaining ranges =
    match ranges with
    | r :: rs ->
        if Range.isOverlapping range r then
          aux (Range.merge range r) [] (remaining @ rs)
        else aux range (r :: remaining) rs
    | [] -> (range, remaining)
  in

  let rec merge acc ranges =
    match ranges with
    | [] -> acc
    | r :: rs ->
        let merged, remaining = aux r [] rs in
        merge (merged :: acc) remaining
  in

  ranges |> merge [] |> List.fold_left (fun acc r -> acc + Range.count r) 0

let sample =
  "3-5\n10-14\n16-20\n12-18\n\n1\n5\n8\n11\n17\n32" |> String.split_on_char '\n'

let () =
  let ranges, ids = read_lines "inputs/day05.txt" |> parse in
  Printf.printf "Part one: %d\n" (solve_part_one ranges ids);
  Printf.printf "Part two: %d\n" (solve_part_two ranges)
