open Core

let solve_p1 file =
  let file = In_channel.create file in
  let contents = In_channel.input_lines file in
  List.fold_left contents ~init:0 ~f:(fun acc x ->
      Int.of_string x + acc)


let find_repeat xs =
  let rec loop curr_val set = function
    | x :: xs ->
       let new_val = Int.of_string x + curr_val in
       if Set.mem set new_val then
         new_val
       else
         loop new_val (Set.add set new_val) xs
    | [] -> loop curr_val set xs
  in
  loop 0 (Set.empty (module Int)) xs

let solve_p2 file =
  let file = In_channel.create file in
  let contents = In_channel.input_lines file in
  find_repeat contents
