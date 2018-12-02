open Core

let to_hash xs =
  let hash = Hashtbl.Poly.create ~growth_allowed:false ~size:(String.length xs) () in
  let f    = Option.value_map ~default:1 ~f:succ in
  String.iter ~f:(Hashtbl.update hash ~f) xs;
  hash

let solve_p1 file =
  In_channel.read_lines file
  |> List.fold_left ~init:(0,0) ~f:(fun (num_three, num_two) xs ->
        let freq = xs |> to_hash |> Hashtbl.to_alist in
        let f x  = List.filter freq (fun (_, i) -> x = i) |> List.is_empty |> not |> Bool.to_int in
        let threes = f 3 in
        let twos   = f 2 in
        (threes + num_three, twos + num_two))
  |> fun (three,two) -> two * three

(* the applicative for a list that ignores the diagonals *)
let app_no_repeat fs xs =
  List.fold_right fs ~init:([], 0) ~f:(fun f (acc,index_f) ->
      List.fold_right xs ~init:(acc,0) ~f:(fun x (acc,index_x) ->
          if index_f <> index_x
          then (f x :: acc, succ index_x)
          else (acc, index_x + 1))
      |> fun (acc, _) -> acc, succ index_f)
  |> Tuple2.get1

let rec remove_diff xs ys =
  match (xs,ys) with
  | (x :: xs, y :: ys) when x = y -> x :: remove_diff xs ys
  | (x :: xs, y :: ys)            -> remove_diff xs ys
  | ([], _) | (_,[])              -> []

let solve_p2 file =
  let xs = In_channel.read_lines file in
  app_no_repeat (List.map xs (fun x y ->
                     String.to_list y
                     |> remove_diff (String.to_list x)
                     |> String.of_char_list))
                xs
  |> List.fold_left ~init:(0,"") ~f:(fun (num_diff, acc) xs ->
        let length_xs = String.length xs in
        if length_xs >= num_diff then
          (length_xs, xs)
        else
          (num_diff, acc))
