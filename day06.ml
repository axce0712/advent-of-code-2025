type operation = Add | Mulitply
type range = { start_pos : int; end_pos : int }

let read_lines filename =
  In_channel.with_open_bin filename In_channel.input_lines

let rec transpose list =
  match list with
  | [] -> []
  | [] :: xss -> transpose xss
  | (x :: xs) :: xss ->
      (x :: List.map List.hd xss) :: transpose (xs :: List.map List.tl xss)

let parse_homework (parts : string list) =
  match parts with
  | "+" :: xs -> (Add, List.map int_of_string xs)
  | "*" :: xs -> (Mulitply, List.map int_of_string xs)
  | _ -> invalid_arg "Unexpected pattern"

let parse (lines : string list) =
  lines
  |> List.map (fun l ->
      l |> String.split_on_char ' ' |> List.filter (fun p -> p <> ""))
  |> transpose
  |> List.map (fun xs -> xs |> List.rev |> parse_homework)

let evaluate op numbers =
  match op with
  | Add -> List.fold_left ( + ) 0 numbers
  | Mulitply -> List.fold_left ( * ) 1 numbers

let solve_part_one lines =
  let problems = parse lines in
  List.fold_left (fun acc (op, xs) -> acc + evaluate op xs) 0 problems

let last list =
  let rec aux acc list =
    match list with
    | [] -> invalid_arg "List must not be empty"
    | [ x ] -> (x, List.rev acc)
    | x :: xs -> aux (x :: acc) xs
  in

  aux [] list

let rfind_operation_opt (idx : int) (line : string) =
  match
    (String.rindex_from_opt line idx '+', String.rindex_from_opt line idx '*')
  with
  | Some add_idx, Some mul_idx ->
      if add_idx > mul_idx then
        Some (Add, { start_pos = add_idx; end_pos = idx })
      else Some (Mulitply, { start_pos = mul_idx; end_pos = idx })
  | Some add_idx, None -> Some (Add, { start_pos = add_idx; end_pos = idx })
  | None, Some mul_idx -> Some (Mulitply, { start_pos = mul_idx; end_pos = idx })
  | None, None -> None

let parse_operations (line : string) =
  let rec aux acc pos =
    if pos <= 0 then acc
    else
      match rfind_operation_opt pos line with
      | Some (op, pos) ->
          (* Notice that every problem except the last problem has a space in
             between, hence -2 after the last *)
          aux ((op, pos) :: acc) (pos.start_pos - 2)
      | None -> acc
  in
  aux [] (String.length line - 1)

let is_digit c = c >= '0' && c <= '9'

let get_number idx (lines : string list) =
  List.fold_left
    (fun acc line ->
      if is_digit line.[idx] then
        let number = int_of_char line.[idx] - int_of_char '0' in
        (acc * 10) + number
      else acc)
    0 lines

let resolve_operation = function Add -> (0, ( + )) | Mulitply -> (1, ( * ))

let solve_problem op range number_lines =
  let { start_pos = idx_start; end_pos = idx_end } = range in

  let indexes = List.init (idx_end - idx_start + 1) (fun i -> idx_start + i) in
  let seed, f = resolve_operation op in

  List.fold_left
    (fun acc idx ->
      let number = get_number idx number_lines in
      f acc number)
    seed indexes

let solve_part_two (lines : string list) =
  let operation_line, number_lines = last lines in
  let operations = parse_operations operation_line in

  operations
  |> List.fold_left
       (fun acc (op, range) -> acc + solve_problem op range number_lines)
       0

let sample =
  "123 328  51 64 \n 45 64  387 23 \n  6 98  215 314\n*   +   *   +  "

let lines = String.split_on_char '\n' sample

let () =
  let lines = read_lines "inputs/day06.txt" in
  Printf.printf "Part one: %d\n" (solve_part_one lines);
  Printf.printf "Part two %d\n" (solve_part_two lines)
