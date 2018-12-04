open Core

type guard_action = Sleep | Wake | Guard of int

type time =
  { year   : int
  ; month  : int
  ; day    : int
  ; hour   : int
  ; minute : int
  }

(* original format of the file *)
type message_info =
  { time   : time
  ; action : guard_action
  }

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

let tuple_of_time_info {time = {year; month; day; hour; minute}} =
  (year,month,day,hour,minute)

let compare_info fst snd =
  compare (tuple_of_time_info fst) (tuple_of_time_info snd)

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


let guard_sleep_times =
  List.fold_left
    ~init:(Map.empty (module Int))
    ~f:(fun map {start ; length ; num} ->
      Map.update map num 
    )

let solve_gen file =
  In_channel.read_lines file
  |> List.map ~f:Parser.eval
  |> List.sort ~compare:compare_info
  |> guard_sleep_times_of_info

let solve_p1 file =
  solve_gen file
