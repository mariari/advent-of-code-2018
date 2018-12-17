open Core

(* a list zipper making O(1) going backwards easy *)
type 'a zipper = Zip of 'a list * 'a list

let zipper_of_list xs = Zip ([], xs)

let is_opposites c1 c2 =
  Char.is_lowercase c1 <> Char.is_lowercase c2
  && Char.uppercase c1 = Char.uppercase c2

let rec remove_pairs = function
  | Zip (p :: ps, n :: ns) when is_opposites p n     -> remove_pairs (Zip (ps, ns))
  | Zip (p :: ps, n :: ns)                           -> remove_pairs (Zip (n :: p :: ps, ns))
  | Zip ([], n1 :: n2 :: ns) when is_opposites n1 n2 -> remove_pairs (Zip ([], ns))
  | Zip ([], n1 :: n2 :: ns)                         -> remove_pairs (Zip ([n2;n1], ns))
  | Zip ([], [x1])                                   -> Zip ([x1],[])
  | Zip (prevs, [])                                  -> Zip (prevs, [])

let polymer_left zipList =
  zipList
  |> remove_pairs
  |> fun (Zip (prevs,nexts)) -> List.length prevs, List.length nexts

let solve_gen file =
  In_channel.read_lines file
  |> List.hd_exn (* we will eat the extra \n that would normally pop up! *)
  |> String.to_list

let solve_p1 file = solve_gen file |> zipper_of_list |> polymer_left

let solve_p2 file =
  let char_range = List.init 26 ~f:(fun i -> Char.of_int_exn (i + 65)) in
  let poly_list  = solve_gen file in
  List.map
    char_range
    ~f:(fun c ->
      List.filter poly_list (fun char -> char <> c && char <> Char.lowercase c)
      |> zipper_of_list
      |> polymer_left)
  |> List.min_elt ~compare
