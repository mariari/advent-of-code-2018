open Core

module Point = struct
  module T = struct
    type t = {x : int; y : int} [@@deriving compare, hash, sexp]
  end
  include T
  include Hashable.Make (T)
  include Comparator.Make(T)
end

type dom = EqualDistance of Point.t (* if two points conflict, save the closest *)
         | FarFrom of Point.t
         | NoPoints
         [@@deriving sexp]

module Parser = struct
  open Angstrom
  type t = Point.t

  let integer =
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string

  let parse =
    let f x y = Point.{x;y} in
    f <$> integer
      <*> string ", " *> integer

  let eval str =
    match parse_string parse str with
    | Ok v      -> v
    | Error msg -> failwith msg
end

let manhatan_distance Point.{x = x1; y = y1} Point.{x = x2; y = y2} =
  abs (x1 - x2) + abs (y1 - y2)

(* generates a grid 0,0 to x-1,y-1 *)
let generate_grid x y ~f =
  List.init x (fun i -> List.init y (f i))

let make_distance_grid =
  generate_grid ~f:(fun x y -> Point.{x; y}, NoPoints)

let update_cloest_point p1 (cords, point) = match point with
  | NoPoints -> (cords, FarFrom p1)
  | EqualDistance p2 -> (* compare is a lot easier to work with than >= *)
     ( match Int.compare (manhatan_distance p1 cords) (manhatan_distance p2 cords) with
     | 0  -> (cords, EqualDistance p2)
     | 1  -> (cords, EqualDistance p2)
     | _  -> (cords, FarFrom p1) ) (* _ is always -1 *)
  | FarFrom p2 ->
     match Int.compare (manhatan_distance p1 cords) (manhatan_distance p2 cords) with
     | 0  -> (cords, EqualDistance p2)
     | 1  -> (cords, FarFrom p2)
     | _  -> (cords, FarFrom p1)

(* generates the boundary for the grid *)
let boundary xs =
  let max_y =
    List.max_elt xs (fun Point.{y = y_1} Point.{y = y_2} -> compare y_1 y_2)
    |> Option.value_exn in
  let max_x = List.max_elt xs Point.compare |> Option.value_exn in
  (max_x.x, max_y.y)

(* x_dim + 1 and y_dim + 1 are infinite *)
let get_finite_area points_list =
  let (x_dom, y_dom) = boundary points_list in
  let grid           = make_distance_grid (x_dom + 2) (y_dom + 2) in
  let grid           = List.fold_left points_list ~init:grid
                                      ~f:List.(fun g p -> map g (map ~f:(update_cloest_point p))) in
  let init_map       = Map.of_alist_exn (module Point)
                                        (List.map points_list (fun p -> p, 0)) in
  let is_on_edge Point.{x ; y} =
    x = succ x_dom || x = 0 || y = succ y_dom || y = 0 in
  let update_map map (point, dom) =
    match dom with
    | NoPoints                         -> map
    | EqualDistance p2                 -> map
    | FarFrom p1 when is_on_edge point -> Map.remove map p1
    | FarFrom p1                       -> Map.change map p1 (Option.map ~f:succ)
  in
  List.fold_left grid
                 ~init:init_map
                 ~f:(fun map -> List.fold_left ~init:map ~f:update_map)


let solve_p1 file =
  In_channel.read_lines file
  |> List.map ~f:Parser.eval
  |> get_finite_area
  |> Map.to_alist
  |> List.max_elt ~compare:(fun (_, i1) (_, i2) -> Int.compare i1 i2)

let solve_p2 file =
  let points_list    = In_channel.read_lines file |> List.map ~f:Parser.eval in
  let (x_dom, y_dom) = boundary points_list in
  let under_10000 cord =
    List.map points_list ~f:(manhatan_distance cord)
    |> List.fold_left ~init:0 ~f:(+)
    |> (>) 10000 in
  (* lets try some imperative ocaml for fun! *)
  let num_less_than = ref 0 in
  for i = 0 to x_dom do
    for j = 0 to y_dom do
      if under_10000 Point.{x = i; y = j} then
        num_less_than := succ !num_less_than
    done
  done;
  !num_less_than

(* printing stuff is interesting! *)
let print_dom d = sexp_of_dom d |> Sexp.to_string |> printf "dom: %s\n"

let print_point p = Point.sexp_of_t p |> Sexp.to_string |> printf "point: %s\n"
