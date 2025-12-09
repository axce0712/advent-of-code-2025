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
        let pairs = List.map (fun y -> (x, y, distance x y)) xs in
        aux (List.rev_append pairs acc) xs
  in
  aux [] lst

let rank junction_boxes =
  junction_boxes |> all_pairs
  |> List.sort (fun (_, _, d1) (_, _, d2) -> Int.compare d1 d2)

module BoxSet = Set.Make (struct
  type t = int * int * int

  let compare = compare
end)

let connect_opt boxes (b1, b2) =
  match (BoxSet.mem b1 boxes, BoxSet.mem b2 boxes) with
  | true, true -> Some boxes
  | true, false -> Some (BoxSet.add b2 boxes)
  | false, true -> Some (BoxSet.add b1 boxes)
  | false, false -> None

let connect_junction_boxes n boxes =
  let rec aux acc (b1, b2) remaining =
    match remaining with
    | bs :: bss -> (
        match connect_opt bs (b1, b2) with
        | Some new_acc -> List.rev acc @ (new_acc :: bss)
        | None -> aux (bs :: acc) (b1, b2) bss)
    | [] -> List.rev ((BoxSet.empty |> BoxSet.add b1 |> BoxSet.add b2) :: acc)
  in

  let ranked_boxes = rank boxes in

  ranked_boxes |> List.take n
  |> List.fold_left (fun acc (b1, b2, _) -> aux [] (b1, b2) acc) []

let solve_part_one lines =
  let junction_boxes = parse lines in
  let connected_junction_boxes = connect_junction_boxes 10 junction_boxes in

  connected_junction_boxes |> List.map BoxSet.cardinal |> List.sort Int.compare
  |> List.rev
(* |> List.take 3
  |> List.fold_left (fun acc s -> acc * BoxSet.cardinal s) 1 *)

let sample =
  "162,817,812\n\
   57,618,57\n\
   906,360,560\n\
   592,479,940\n\
   352,342,300\n\
   466,668,158\n\
   542,29,236\n\
   431,825,988\n\
   739,650,466\n\
   52,470,668\n\
   216,146,977\n\
   819,987,18\n\
   117,168,530\n\
   805,96,715\n\
   346,949,466\n\
   970,615,88\n\
   941,993,340\n\
   862,61,35\n\
   984,92,344\n\
   425,690,689"

let lines = String.split_on_char '\n' sample solve_part_one lines
