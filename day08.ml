let int_pow base exp =
  let rec aux acc b = function
    | 0 -> acc
    | e when e mod 2 = 0 -> aux acc (b * b) (e / 2)
    | e -> aux (acc * b) (b * b) (e / 2)
  in
  aux 1 base exp

let distance (x1, y1, z1) (x2, y2, z2) =
  int_pow (x2 - x1) 2 + int_pow (y2 - y1) 2 + int_pow (z2 - z1) 2

let parse_junction_box line =
  match String.split_on_char ',' line with
  | [ x; y; z ] -> (int_of_string x, int_of_string y, int_of_string z)
  | _ -> invalid_arg ("Invalid coordinate: " ^ line)

let parse (lines : string list) = lines |> List.map parse_junction_box

let all_pairs lst =
  let rec aux acc = function
    | [] -> acc
    | x :: xs ->
        let pairs = List.map (fun y -> (x, y)) xs in
        aux (List.rev_append pairs acc) xs
  in
  aux [] lst

let rank junction_boxes =
  junction_boxes |> all_pairs
  |> List.map (fun (x, y) -> (x, y, distance x y))
  |> List.sort (fun (_, _, d1) (_, _, d2) -> Int.compare d1 d2)

let index_of arr x =
  match Array.find_index (( = ) x) arr with
  | Some i -> i
  | None -> failwith "Element not found"

let counts_of_array arr =
  let tbl = Hashtbl.create 16 in
  Array.iter
    (fun x ->
      Hashtbl.replace tbl x
        (1 + Option.value (Hashtbl.find_opt tbl x) ~default:0))
    arr;
  Hashtbl.to_seq tbl |> List.of_seq

let solve n junction_boxes =
  let ranked = rank junction_boxes in
  let circuits = Array.of_list junction_boxes in
  let connections = Array.init (Array.length circuits) (fun _ -> 0) in
  let rec aux next_idx ranks =
    match ranks with
    | (b1, b2, d) :: rs -> (
        let b1_idx = index_of circuits b1 in
        let b2_idx = index_of circuits b2 in

        match (Array.get connections b1_idx, Array.get connections b2_idx) with
        | 0, 0 ->
            Array.set connections b1_idx next_idx;
            Array.set connections b2_idx next_idx;
            aux (next_idx + 1) rs
        | idx, 0 ->
            Array.set connections b2_idx idx;
            aux next_idx rs
        | 0, idx ->
            Array.set connections b1_idx idx;
            aux next_idx rs
        | idx1, idx2 ->
            Array.iteri
              (fun i x -> if x = idx2 then Array.set connections i idx1)
              connections;
            aux next_idx rs)
    | [] -> ()
  in

  aux 1 (List.take n ranked);

  counts_of_array connections
  |> List.sort (fun (idx1, c1) (idx2, c2) ->
      match (idx1, idx2) with 0, _ -> 1 | _, 0 -> -1 | _ -> compare c2 c1)
  |> List.take 3
  |> List.fold_left (fun acc (_, count) -> acc * count) 1

let solve_part_one = solve 1000

let solve_part_two junction_boxes =
  let ranked = rank junction_boxes in
  let circuits = Array.of_list junction_boxes in
  let connections = Array.init (Array.length circuits) (fun _ -> 0) in
  let rec aux next_idx ranks =
    match ranks with
    | (b1, b2, d) :: rs -> (
        let b1_idx = index_of circuits b1 in
        let b2_idx = index_of circuits b2 in

        match (Array.get connections b1_idx, Array.get connections b2_idx) with
        | 0, 0 ->
            Array.set connections b1_idx next_idx;
            Array.set connections b2_idx next_idx;
            aux (next_idx + 1) rs
        | idx, 0 ->
            Array.set connections b2_idx idx;
            aux next_idx rs
        | 0, idx ->
            Array.set connections b1_idx idx;
            aux next_idx rs
        | idx1, idx2 ->
            Array.iteri
              (fun i x -> if x = idx2 then Array.set connections i idx1)
              connections;

            if Array.for_all (fun x -> x = idx1) connections then
              let x1, _, _ = b1 in
              let x2, _, _ = b2 in
              x1 * x2
            else aux next_idx rs)
    | [] -> failwith "Unreachable"
  in

  aux 1 ranked

let read_lines filename =
  In_channel.with_open_bin filename In_channel.input_lines

let () =
  let junction_boxes = read_lines "inputs/day08.txt" |> parse in
  Printf.printf "Part One: %d\n" (solve_part_one junction_boxes);
  Printf.printf "Part Two: %d\n" (solve_part_two junction_boxes)
