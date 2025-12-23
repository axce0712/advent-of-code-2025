module IntSet = Set.Make (Int)

let parse_indicator_lights input =
  let acc = ref 0 in
  String.iteri (fun i c -> if c = '#' then acc := !acc lor (1 lsl i)) input;
  !acc

let parse_buttons input =
  input |> String.split_on_char ','
  |> List.fold_left (fun acc part -> acc lor (1 lsl int_of_string part)) 0

let toggle buttons indicator_lights = indicator_lights lxor buttons

type machine = {
  indicator_lights : int;
  buttons : IntSet.t;
  joltage_requirements : int list;
}

let parse_machine (line : string) =
  let rec aux acc idx =
    match String.index_from_opt line idx '(' with
    | None -> (acc, idx)
    | Some start_idx ->
        let end_idx = String.index_from line start_idx ')' in
        let buttons =
          parse_buttons
            (String.sub line (start_idx + 1) (end_idx - start_idx - 1))
        in
        aux (IntSet.add buttons acc) (end_idx + 2)
  in

  let idx_end_indicator_lights = String.index line ']' in
  let indicator_lights =
    String.sub line 1 (idx_end_indicator_lights - 1) |> parse_indicator_lights
  in

  let buttons, idx_after_buttons =
    aux IntSet.empty (idx_end_indicator_lights + 2)
  in

  let idx_start_joltage = String.index line '{' in
  let idx_end_joltage = String.length line - 1 in
  let joltage_requirements =
    String.sub line (idx_start_joltage + 1)
      (idx_end_joltage - idx_start_joltage - 1)
    |> String.split_on_char ',' |> List.map int_of_string
  in

  { indicator_lights; buttons; joltage_requirements }

let fewest_total_presses (machine : machine) : int =
  let rec aux presses candidates =
    if IntSet.mem machine.indicator_lights candidates then presses
    else
      let new_candidates =
        IntSet.fold
          (fun b1 acc ->
            machine.buttons
            |> IntSet.map (fun b2 -> b1 lxor b2)
            |> IntSet.union acc)
          candidates IntSet.empty
      in

      aux (presses + 1) new_candidates
  in
  aux 0 (IntSet.empty |> IntSet.add 0)

let solve_part_one (machines : machine list) : int =
  List.fold_left
    (fun acc machine -> acc + fewest_total_presses machine)
    0 machines

let read_lines filename =
  In_channel.with_open_bin filename In_channel.input_lines

let sample =
  "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}\n\
   [...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}\n\
   [.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"

let lines = String.split_on_char '\n' sample

let () =
  let machines = read_lines "inputs/day10.txt" |> List.map parse_machine in
  Printf.printf "Part One: %d\n" (solve_part_one machines)
