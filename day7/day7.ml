open Core

type dependency = {
    node       : char;
    depends_on : char
  }

module Parser = struct
  open Angstrom

  type t = dependency

  let parse =
    let f node depends_on = {node; depends_on } in
    f <$> string "Step " *> any_char
      <*> string " must be finished before step " *> any_char

  let eval str =
    match parse_string parse str with
    | Ok v    -> v
    | Error m -> failwith m
end

module G = Graph.Persistent.Digraph.ConcreteBidirectional (Char)

let generate_map =
  List.fold_left ~f:(fun g {node; depends_on} -> G.add_edge g depends_on node)
                 ~init:G.empty

let no_in g =
  G.fold_vertex (fun c xs -> if G.out_degree g c = 0 then c :: xs else xs) g []

let min_ele_no_in g =
  no_in g
  |> List.min_elt ~compare:Char.compare

let dependency_order g =
  let rec loop g ns =
    match min_ele_no_in g with
    | Some n -> loop (G.remove_vertex g n) (n :: ns)
    | None   -> ns
  in
  loop g []

let solve_gen file =
  In_channel.read_lines file
  |> List.map ~f:Parser.eval
  |> generate_map

let solve_p1 file =
  solve_gen file
  |> dependency_order
  |> List.rev
  |> String.of_char_list


type worker = Busy of int * char
            | Free
            [@@deriving compare, sexp]

(* CODE FROM UTILTIIES ---------------------------------------------------------- *)
(* converts a list, xs, to a hashtbl, In reality this is a bag *)
let to_hash xs =
  let hash = Hashtbl.Poly.create ~growth_allowed:false ~size:(List.length xs) () in
  let f x = function
    | Some x -> succ x
    | None   -> 1
  in
  List.iter xs ~f:(fun x -> Hashtbl.update hash x (f x));
  hash

(* fast way of doing list difference, O(n + m) *)
let rec diff xs ys =
  let hash = to_hash ys in
  let rem_or_del x =
    Hashtbl.change hash x ~f:(function
        | None    -> None
        | Some 1  -> None
        | Some x  -> Some (pred x) )
  in
  List.filter xs ~f:(fun x ->
      if Hashtbl.mem hash x then begin
        rem_or_del x;
        false
      end else
        true)
(* Back to Scheduled code ----------------------------------------------------- *)

let complete_n_workers g n base_time =
  let wait_time char = (base_time + Char.to_int char - Char.to_int 'A' + 1) in
  let inital_workers = List.init n (const Free) in
  let alloc_all_workers graph workers =
    let new_free_nodes =
      diff (no_in graph)
           (List.filter_map workers
                            (function
                             | Free       -> None
                             | Busy (j,c) -> Some c))
    in
    let rec solve_time xs ys acc =
      match xs, ys with
      | [], [] | [], _         -> acc
      | xs, []                 -> List.append xs acc
      | Free :: xs, y :: ys    -> solve_time xs ys (Busy (wait_time y, y) :: acc)
      | Busy (w,i) :: xs, ys   -> solve_time xs ys (Busy (w,i) :: acc)
    in
    solve_time workers new_free_nodes []
  in
  let sub_smallest_exn graph worker_list =
    match List.min_elt worker_list ~compare:compare_worker with
    | Some Free         -> failwith "There is a cycle in the dependency graph, no values can be reduced"
    | None              -> failwith "Send in a non_empty_list"
    | Some (Busy (i,_)) -> List.fold_map
                            ~init:graph
                            worker_list ~f:(fun g -> function
                                             | Busy (j,c) when i = j -> G.remove_vertex g c, Free
                                             | Busy (j,c)            -> g, Busy ((j - i), c)
                                             | Free                  -> g, Free)
                        , i
  in
  let rec loop g workers time =
    if G.is_empty g then
      let workers_no_free = List.filter ~f:(fun x -> Free = x) workers in
      match List.max_elt workers_no_free ~compare:compare_worker with
      | Some Free         -> time
      | None              -> time
      | Some (Busy (i,_)) -> time + i
    else
      let ws           = alloc_all_workers g workers in
      let ((g, ws), i) = sub_smallest_exn g ws in
      loop g ws (time + i)
  in
  loop g inital_workers 0

let solve_p2 file =
  let g = solve_gen file in
  complete_n_workers g 5 60

(* things I've learned------------------------------------------------------------ *)
let print_work_list ws =
  printf "%s\n" (Sexp.to_string (List.sexp_of_t sexp_of_worker ws));
