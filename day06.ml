type operation = Add | Mulitply

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

let sample =
  "123 328  51 64 \n 45 64  387 23 \n  6 98  215 314\n*   +   *   +  "

let lines = String.split_on_char '\n' sample

let solve_part_one problems =
  List.fold_left
    (fun acc (op, xs) ->
      acc
      +
      match op with
      | Add -> List.fold_left ( + ) 0 xs
      | Mulitply -> List.fold_left ( * ) 1 xs)
    0 problems

let () =
  let problems = read_lines "inputs/day06.txt" |> parse in
  Printf.printf "Part one: %d\n" (solve_part_one problems)