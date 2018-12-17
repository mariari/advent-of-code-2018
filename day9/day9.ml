open Core

(* 459 players; last marble is worth 71320 points *)

type 'a circular_list = {
    prev : 'a list;
    next : 'a list
  }

(* these semantics are very poor bounds wise, don't use in a general setting!!!! *)
(* See okasaki's purely functional data structures to do this right *)
let move_foward {prev; next} =
  match next with
  | []      -> {prev = [] ; next = List.rev prev} (* naive, better to split in half *)
  | n :: [] -> {prev = [n]; next = List.rev prev}
  | n :: ns -> {prev = n :: prev; next = ns}

let move_back {prev; next} =
  match prev with
  | []      -> {prev = List.rev next; next = []} (* naive, better to split in half *)
  | p :: [] -> {prev = List.rev next; next = [p]}
  | p :: ps -> {prev = ps ; next = p :: next}

let move_n_back n =
  Fn.apply_n_times ~n move_back

let rec remove_next = function
  | {next = []; prev = []} -> {next = []; prev = []}
  | {next = []; prev}      -> remove_next (move_foward {next = []; prev})
  | {next = x :: xs; prev} -> {next = xs; prev}

let rec get_next_exn = function
  | {next = []; prev = []} -> failwith "Can't get an element from an empty cylic_list"
  | {next = []; prev}      -> get_next_exn (move_foward {next = []; prev})
  | {next = x :: xs; prev} -> x

let insert_node {next; prev} ~ele =
  {next = ele :: next; prev}

let play_game ~turns ~players =
  let board = {prev = []; next = [0]} in
  let step_game (board, i, score_map) =
    if i mod 23 = 0 then
      let back_7 = move_n_back 7 board in
      ( back_7 |> remove_next
      , succ i
      , Map.update score_map (i mod players)
                   ~f:(Fn.compose ((+) (i + get_next_exn back_7)) (Option.value ~default:0)) )
    else
      ( board |> move_foward |> move_foward |> insert_node ~ele:i
      , succ i
      , score_map )
  in
  Fn.apply_n_times ~n:turns step_game (board, 1, Map.empty (module Int))
  |> Tuple3.get3
  |> Map.to_alist
  |> List.max_elt ~compare:(fun (_,v1) (_, v2) -> Int.compare v1 v2)
