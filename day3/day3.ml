open Core

type claim =
  { id     : int
  ; top    : int
  ; left   : int
  ; width  : int
  ; height : int
  }

module Parser = struct
  open Angstrom
  type t = claim

  let integer =
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string

  let parse =
    let f i l t w h = {id = i; left = l; top = t; width = w; height = h} in
    f <$> char '#'     *> integer
      <*> string " @ " *> integer
      <*> char ','     *> integer
      <*> string ": "  *> integer
      <*> char 'x'     *> integer

  let eval str =
    match parse_string parse str with
    | Ok v      -> v
    | Error msg -> failwith msg
end

module Key = struct
  module T = struct
    type t = Int.t * Int.t  [@@deriving compare, hash, sexp]
  end
  include T
  include Hashable.Make (T)
end

let occupy ht {left; top; width; height} =
  let update = Hashtbl.update ht ~f:(Option.value_map ~default:1 ~f:succ) in
  let ranges = List.cartesian_product
                 (List.range ~stop:`inclusive (left + 1) (left + width))
                 (List.range ~stop:`inclusive (top + 1)  (top  + height))
  in
  List.iter ranges update

let num_overlap =
  Hashtbl.fold ~init:0 ~f:(fun ~key ~data acc ->
      if data > 1
      then succ acc
      else acc )

let solve_p1 file =
  let table = Key.Table.create () ~size:1000 in
  In_channel.read_lines file
  |> List.map ~f:Parser.eval
  |> List.iter ~f:(occupy table);
  num_overlap table

let occupy_set ht hs {id; left; top; width; height} =
  let f = function
    | None          -> 1, id
    | Some (n, id') -> Hash_set.remove hs id';
                       Hash_set.remove hs id;
                       (n, id') in
  let update = Hashtbl.update ht ~f in
  let ranges = List.cartesian_product
                 (List.range ~stop:`inclusive (left + 1) (left + width))
                 (List.range ~stop:`inclusive (top + 1)  (top  + height))
  in
  List.iter ranges update


let solve_p2 file =
  let table = Key.Table.create () ~size:1000 in
  let set   = Int.Hash_set.create () ~size:1254 ~growth_allowed:false in
  List.iter (List.range 1 1254) (Hash_set.add set);
  In_channel.read_lines file
  |> List.map ~f:Parser.eval
  |> List.iter ~f:(occupy_set table set);
  Hash_set.to_list set
