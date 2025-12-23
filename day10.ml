module IntSet = Set.Make (Int)

let toggle buttons indicator_lights =
  indicator_lights
  |> String.mapi (fun i c ->
      if IntSet.mem i buttons then
        match c with
        | '.' -> '#'
        | '#' -> '.'
        | _ -> failwith "Invalid character in indicator lights"
      else c)

type machine = {
  indicator_lights : string;
  buttons : IntSet.t list;
  joltage : int list;
}

let parse_line (line : string) =
  let rec parse_buttons acc idx =
    let rec aux acc (idx_start, idx_end) =
      match String.index_from_opt line idx_start ',' with
      | Some idx when idx < idx_end ->
          let button =
            String.sub line idx_start (idx - idx_start) |> int_of_string
          in
          aux (IntSet.add button acc) (idx + 1, idx_end)
      | Some _ | None ->
          let button =
            String.sub line idx_start (idx_end - idx_start + 1) |> int_of_string
          in
          IntSet.add button acc
    in

    match String.index_from_opt line idx '(' with
    | None -> (List.rev acc, idx)
    | Some start_idx ->
        let end_idx = String.index_from line start_idx ')' in
        let buttons = aux IntSet.empty (start_idx + 1, end_idx - 1) in
        parse_buttons (buttons :: acc) (end_idx + 2)
  in

  let idx_end_indicator_lights = String.index line ']' in
  let indicator_lights = String.sub line 1 (idx_end_indicator_lights - 1) in

  let buttons, idx_after_buttons =
    parse_buttons [] (idx_end_indicator_lights + 2)
  in

  let idx_start_joltage = String.index line '{' in
  let idx_end_joltage = String.length line - 1 in
  let joltage_str =
    String.sub line (idx_start_joltage + 1)
      (idx_end_joltage - idx_start_joltage - 1)
    |> String.split_on_char ',' |> List.map int_of_string
  in

  { indicator_lights; buttons; joltage = joltage_str }

type press_step = { previous_buttons : IntSet.t; indicator_lights : string }

let next (machine : machine) (step : press_step) : press_step list =
  machine.buttons
  |> List.fold_left
       (fun acc buttons ->
         if IntSet.equal buttons step.previous_buttons then acc
         else
           let new_indicator_lights = toggle buttons step.indicator_lights in
           {
             previous_buttons = buttons;
             indicator_lights = new_indicator_lights;
           }
           :: acc)
       []

let fewest_total_presses (machine : machine) : int =
  let rec aux presses steps =
    let configured, next_steps =
      steps
      |> List.fold_left
           (fun (configured, acc) step ->
             if configured then (true, acc)
             else
               let step_next = next machine step in
               let found =
                 List.exists
                   (fun s ->
                     String.for_all (fun c -> c = '.') s.indicator_lights)
                   step_next
               in

               if found then (true, acc) else (false, step_next :: acc))
           (false, [])
    in

    if configured then presses + 1
    else aux (presses + 1) (List.flatten next_steps)
  in

  let steps =
    [
      {
        previous_buttons = IntSet.empty;
        indicator_lights = machine.indicator_lights;
      };
    ]
  in
  aux 0 steps

let solve (machines: machine list) : int =
  List.map fewest_total_presses machines
  |> List.fold_left (+) 0

let read_lines filename =
  In_channel.with_open_bin filename In_channel.input_lines

let sample =
  "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}\n\
   [...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}\n\
   [.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"

let lines = String.split_on_char '\n' sample

let () =
  let machines = read_lines "inputs/day10.txt" |> List.map parse_line in
  Printf.printf "Part One: %d\n" (solve machines);