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

let step_board points ~dimx ~dimy ~offset =
  let new_vec = List.map points update_vector in
  let board   = Array.make_matrix ~dimx ~dimy '.' in
  List.iter new_vec (fun {pos = {x;y}} -> board.(y + offset).(x + offset) <- '#');
  (board, new_vec)

let rec run_board_n points ~i ~dimx ~dimy ~offset =
  match i with
  | 0 -> step_board points dimx dimy offset
  | i -> let (board, points) = step_board points dimx dimy offset in
         Array.iter board ~f:(fun x -> Array.iter x (printf "%c"); printf "\n");
         run_board_n points (pred i) dimx dimy offset

let num_connected points =
  let neighboring_points {x; y} =
    [{x = succ x; y = succ y}; {x = succ x; y = pred y};
     {x = pred x; y = succ y}; {x = pred x; y = pred y};
     {x;          y = succ y}; {x;          y = pred y};
     {x = succ x; y};          {x = pred x; y}]
  in
  let pos = List.map ~f:(fun p -> p.pos) points in
  let set = Set.of_list (module Pos) pos in
  List.fold_left
    pos ~init:(set,0)
    ~f:(fun (set,i) p ->
      Set.remove set p,
      neighboring_points p
      |> List.sum (module Int) ~f:(Fn.compose Bool.to_int (Set.mem set)))

let solve_p1 file =
  In_channel.read_lines file
  |> List.map ~f:Parser.eval
  |> num_connected
