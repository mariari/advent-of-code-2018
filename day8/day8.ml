open Core

type 'a meta_tree =
  Node of {
      children : 'a meta_tree list;
      meta     : 'a list
    }
| Empty

let meta_tree_of_list xs =
  let rec loop = function
    | [] -> Empty, []
    | [x] -> failwith "bad input list given to meta_tree_of_list"
    | num_children :: num_meta :: xs ->
       let (xs, children) = add_children xs [] num_children in
       Node { children = children ;
              meta = List.take xs num_meta
            }
       , List.drop xs num_meta
  and add_children xs acc = function
    | 0 -> xs, List.rev acc
    | i ->
       let (tree, xs) = loop xs in
       add_children xs (tree :: acc) (pred i)
  in
  loop xs |> Tuple2.get1

let rec fold_tree tree ~init ~f =
  match tree with
  | Empty -> init
  | Node {children; meta} ->
     List.fold_left children
                    ~init:(List.fold_left meta ~f ~init)
                    ~f:(fun acc -> fold_tree ~init:acc ~f)

let solve_gen file =
  In_channel.read_lines file
  |> List.hd_exn
  |> String.split ~on:' '
  |> List.map ~f:int_of_string
  |> meta_tree_of_list

let solve_p1 file =
  solve_gen file |> fold_tree ~init:0 ~f:(+)

let rec node_value = function
  | Empty                      -> 0
  | Node {children = []; meta} -> List.sum (module Int) meta ~f:ident
  | Node {children; meta} ->
     let arr_children = Array.of_list children in
     let arr_length   = Array.length arr_children in
     List.fold_left
       meta ~init:0
       ~f:(fun acc i ->
         if i = 0 || i - 1 >= arr_length then
           acc
         else
           (acc + node_value arr_children.(i - 1)))

let solve_p2 file =
  solve_gen file |> node_value
