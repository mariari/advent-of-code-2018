open Core

type pos = {
    x : int;
    y : int
  } [@@deriving compare, sexp, hash]

type vel = pos [@@deriving compare, sexp, hash]

module Pos = struct
  module T = struct
    type t = pos [@@deriving compare, sexp, hash]
  end
  include T
  include Comparator.Make(T)
end

type info = {
    pos : pos;
    vel : vel
  } [@@deriving compare, sexp, hash]

module Parser = struct
  open Angstrom
  type t = info

  let integer =
    take_while1 (function '0' .. '9' | '-' -> true | _ -> false) >>| int_of_string

  let int_space =
    many (char ' ') *> integer

  let parse_point =
    let f x y = {x; y} in
    f <$> char '<' *> int_space
      <*> char ',' *> int_space <* char '>'

  let parse =
    let f pos vel = {pos; vel}
    in
    f <$> string "position="  *> parse_point
      <*> string " velocity=" *> parse_point

  let eval str =
    match parse_string parse str with
    | Ok v      -> v
    | Error msg -> failwith msg
end

let update_vector {pos = {x = x_pos; y = y_pos}; vel = ({x = x_vel; y = y_vel} as vel)} =
  {pos = {x = x_pos + x_vel; y = y_pos + y_vel}; vel}

let update_vectors =
  List.map ~f:update_vector

let num_connected points =
  let neighboring_points {x; y} =
    [{x; y = succ y}; {x; y = pred y};
     {x = succ x; y}; {x = pred x; y}]
  in
  let pos = List.map ~f:(fun p -> p.pos) points in
  let set = Set.of_list (module Pos) pos in
  List.fold_left
    pos ~init:(set,0)
    ~f:(fun (set,i) p ->
      Set.remove set p,
      neighboring_points p
      |> List.sum (module Int) ~f:(Fn.compose Bool.to_int (Set.mem set))
      |> (+) i)
  |> Tuple2.get2

let find_message points ~goal =
  let rec get_close points i =
    let num = num_connected points in
    if num >= goal then
      points, num, i
    else
      get_close (update_vectors points) (succ i)
  in
  (* turns out this section is uneeded with the goal of 180! *)
  let rec potentials points potential_points max_seen i =
    let num = num_connected points in
    if num >= max_seen then
      potentials (update_vectors points)
                 ((List.map points (fun p -> p.pos), i) :: potential_points)
                 num
                 (succ i)
    else
      potential_points
    in
    let (new_points, max, i) = get_close points 0 in
    potentials new_points [] max i

let print_point (xs, i) =
  let gen init g f = List.fold xs ~init ~f:(fun min_value x -> g min_value (f x)) in
  let min_gen      = gen Int.max_value Int.min in
  let max_gen      = gen Int.min_value Int.max in
  let min_x        = min_gen (fun p -> p.x) in
  let min_y        = min_gen (fun p -> p.y) in
  let max_x        = max_gen (fun p -> p.x) in
  let max_y        = max_gen (fun p -> p.y) in
  let board = Array.make_matrix '.' ~dimx:(max_y - min_y + 1) ~dimy:(max_x - min_x + 1) in
  List.iter xs (fun {x;y} -> board.(y - min_y).(x - min_x) <- '#');
  printf "after %i seconds:\n" i;
  Array.iter board ~f:(fun x -> Array.iter x (printf "%c"); printf "\n")

let solve_gen file =
  In_channel.read_lines file
  |> List.map ~f:Parser.eval
  |> find_message ~goal:180
  |> List.iter ~f:print_point

(* let () = solve_gen "input.txt" *)
