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

let solve_p1 file =
  In_channel.read_lines file
  |> List.map ~f:Parser.eval
  |> generate_map
  |> dependency_order
  |> List.rev
  |> String.of_char_list

let complete_n_workers g n base_time =
  Sequence.of_list
  g
