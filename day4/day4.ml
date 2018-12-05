open Core

type guard_action = Sleep | Wake | Guard of int [@@deriving compare]

type time =
  { year   : int
  ; month  : int
  ; day    : int
  ; hour   : int
  ; minute : int
  } [@@deriving compare]

(* original format of the file *)
type message_info =
  { time   : time
  ; action : guard_action
  } [@@deriving compare]

type minutes = int

(* data will be converted from message_info into this *)
type guad_sleep_time =
  { start  : time
  ; length : minutes
  ; num    : int
  }

module Parser = struct
  open Angstrom
  type t = message_info

  let integer =
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string

  let sleep = string "falls asleep" *> return Sleep
  let wake  = string "wakes up"     *> return Wake
  let guard = (fun x -> Guard x) <$> string "Guard #" *> integer

  let guard_action = guard <|> wake <|> sleep

  let parse =
    let f year month day hour minute action =
      {time = {year; month; day; hour; minute}; action} in
    f <$> char '[' *> integer
      <*> char '-' *> integer
      <*> char '-' *> integer
      <*> char ' ' *> integer
      <*> char ':' *> integer
      <*> string "] " *> guard_action

  let eval str =
    match parse_string parse str with
    | Ok v      -> v
    | Error msg -> failwith msg
end

(* based on the input file I'm not going to account for month or year rollover *)
let time_to_mins {day ; hour ; minute} =
  minute + 60 * hour + 60 * 24 * day

(* takes a sorted list of message_infos and produces a list guad_sleep_times*)
let guard_sleep_times_of_info = function
  | []  -> []
  | {action = Guard num} :: xs ->
     let rec loop num acc = function
       | []                         -> List.rev acc
       | {action = Guard num} :: xs -> loop num acc xs
       | {action = Sleep; time = start } :: {action = Wake; time = end_time} :: xs ->
          loop num
               ({start
                ; num
                ; length = (time_to_mins end_time - time_to_mins start) }
                :: acc)
               xs
       | _ -> failwith "Invalid input format, an awake must come after alseep"
     in
     loop num [] xs
  | {action = Sleep} :: _ | {action = Wake} :: _ -> failwith "A guard should come first"


let gen_minute_list start length =
  List.init length (fun i -> (start + i) mod 60)

let increment_map map =
  List.fold_left
    ~init:map
    ~f:(fun map x -> Map.update map x ~f:(Option.value_map ~default:1 ~f:succ))

let guard_sleep_times =
  let empty_map = Map.empty (module Int) in
  List.fold_left
    ~init:empty_map
    ~f:(fun map {start; length ; num} ->
      let min_list = gen_minute_list start.minute length in
      Map.update map num
        ~f:(Option.value_map
              ~default:(length, increment_map empty_map min_list)
              ~f:(fun (num_slept, map) ->
                num_slept + length, increment_map map min_list)))

let solve_gen file =
  In_channel.read_lines file
  |> List.map ~f:Parser.eval
  |> List.sort ~compare:compare_message_info
  |> guard_sleep_times_of_info
  |> guard_sleep_times

let gratest_slept =
  Map.fold
    ~init:(0,0)
    ~f:(fun ~key ~data (k,greatest) ->
      if data > greatest
      then (key,data)
      else (k, greatest))


let solve_p1 file =
  solve_gen file
  |> Map.fold
      ~init:(0, 0, Map.empty (module Int))
      ~f:(fun ~key ~data:(num_slept, slept_map) ((max_key, max_slept, m) as acc)
          -> if num_slept > max_slept
            then (key, num_slept, slept_map)
            else acc )
  |> fun (guard_num, _, map) ->
    let (key,_) = gratest_slept map in
    guard_num * key


let solve_p2 file =
  solve_gen file
  |> Map.fold
      ~init:(0,0,0)
      ~f:(fun ~key:curr_guard ~data:(_, slept_map) (guard, minute, times_slept) ->
        let (time,curr_times_slept) = gratest_slept slept_map in
        if curr_times_slept > times_slept then
          (curr_guard, time, curr_times_slept)
        else
          (guard, minute, times_slept))
  |> fun (guard, minute, _) -> guard * minute
