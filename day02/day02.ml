let read_file filename =
  let ic = open_in filename in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  content

let count_digits n =
  let rec aux = function 0 -> 0 | n -> 1 + aux (n / 10) in

  match n with 0 -> 1 | n -> aux (abs n)

let is_even n = n mod 2 = 0

let int_pow base exp =
  let rec aux acc b = function
    | 0 -> acc
    | e when e mod 2 = 0 -> aux acc (b * b) (e / 2)
    | e -> aux (acc * b) (b * b) (e / 2)
  in
  aux 1 base exp

let is_invalid id =
  let digit_count = count_digits id in
  match is_even digit_count with
  | true ->
      let divisor = int_pow 10 (digit_count / 2) in
      id / divisor = id mod divisor
  | false -> false

let invalid_ids low high =
  let rec aux high acc n =
    if n <= high then
      let new_acc = if is_invalid n then n :: acc else acc in
      aux high new_acc (n + 1)
    else List.rev acc
  in
  aux high [] low

let get_ranges input =
  input |> String.split_on_char ','
  |> List.map (fun range ->
         match String.split_on_char '-' range with
         | [ low; high ] -> (int_of_string low, int_of_string high)
         | _ -> failwith "Invalid range")

let solve f ranges =
  ranges
  |> List.concat_map (fun (low, high) -> f low high)
  |> List.fold_left ( + ) 0

let solve_part_one ranges = solve invalid_ids ranges

let is_invalid2 digit_count id =
  let rec aux expected n exp =
    let actual = (n / (int_pow 10 exp)) mod (int_pow 10 digit_count) in

    if expected = actual then
      let next = int_pow 10 (exp + digit_count) in
      
      if next > n then
        true
      else
        aux expected n (exp + digit_count)
    else
      false
  in

  if digit_count <= 0 then
    false
  else
    let expected = id mod (int_pow 10 digit_count) in
    aux expected id digit_count

let is_repeating id =
  let rec aux digit_count exp =
    if exp > (digit_count / 2) then
      false
    else
      if digit_count mod exp = 0 then
        if is_invalid2 exp id then
          true
        else
          aux digit_count (exp + 1)
      else
        aux digit_count (exp + 1)
  in

  let digit_count = count_digits id in
  aux digit_count 1

let invalid_ids2 low high =
  let rec aux high acc n =
    if n <= high then
      let new_acc = if is_repeating n then n :: acc else acc in
      aux high new_acc (n + 1)
    else List.rev acc
  in
  aux high [] low

let solve_part_two ranges = solve invalid_ids2 ranges

let sampleData =
  "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"

let () =
  read_file "day02/input.txt"
  |> get_ranges |> solve_part_two
  |> Printf.printf "Part Two: %d\n"
