open Core

type guard_action = Sleep | Wake | Guard of int

type message_info =
  { year   : int
  ; month  : int
  ; day    : int
  ; hour   : int
  ; minute : int
  ; action : guard_action
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
    let f year month day hour minute action = {year; month; day; hour; minute; action} in
    f <$> char '[' *> integer
      <*> char '-' *> integer
      <*> char '-' *> integer
      <*> char ' ' *> integer
      <*> char ':' *> integer
      <*> string "] " *> guard_action
end
