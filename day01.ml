
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

let parse_instruction instruction =
  let turn =
    match String.get instruction 0 with
    | 'L' -> -1
    | 'R' -> 1
    | _ -> failwith "Invalid turn"
  in
  let distance = int_of_string (String.sub instruction 1 (String.length instruction - 1)) in
  turn, distance

let to_circle distance =
  if distance < 0 && distance mod 100 <> 0 then
    100 + (distance mod 100)
  else
    distance mod 100

let compute_rotation pointing (turn, distance) =
  let rotation = turn * distance in
  pointing + rotation

let password_protocol (pointing, count) instruction =
  let new_pointing = compute_rotation pointing instruction in
  let new_count = if to_circle new_pointing = 0 then count + 1 else count in
  (new_pointing, new_count)

let solve f instructions =
  instructions
  |> List.fold_left f (50, 0)
  |> snd

let solvePartOne instructions =
  solve password_protocol instructions

let password_method_0x434C49434B (pointing, count) (turn, distance) =
  let clicks = distance mod 100 * turn in
  let overflows = distance / 100 in
  let new_pointing = pointing + clicks in

  if new_pointing > 0 && new_pointing < 100 then
    new_pointing, count + overflows
  else
    (new_pointing + 100) mod 100, count + overflows + if pointing = 0 then 0 else 1

let solvePartTwo instructions =
  solve password_method_0x434C49434B instructions

let text = "L68\nL30\nR48\nL5\nR60\nL55\nL1\nL99\nR14\nL82"

let samplePartOne =
  text
  |> String.split_on_char '\n'
  |> List.map parse_instruction
  |> solvePartOne

let partOne =
  read_lines "inputs/day01.txt"
  |> List.map parse_instruction
  |> solvePartOne

let samplePartTwo =
  text
  |> String.split_on_char '\n'
  |> List.map parse_instruction
  |> solvePartTwo

let partTwo =
  read_lines "inputs/day01.txt"
  |> List.map parse_instruction
  |> solvePartTwo

let () = Printf.printf "Result: %d\n" partTwo
