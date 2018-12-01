open Core

let solve_p1 file =
  let contents = In_channel.read_lines file in
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

(* slightly slower than find_repeat, but uses sequences *)
let find_repeat_seq xs =
  Sequence.cycle_list_exn xs
  |> Sequence.fold_until ~init:(0, Set.empty (module Int)) ~finish:Tuple2.get1
      ~f:(fun(acc,set) x ->
        let new_val = Int.of_string x + acc in
        if Set.mem set new_val then
          Stop new_val
        else
          Continue (new_val, Set.add set new_val) )


let solve_p2 file f =
  let contents = In_channel.read_lines file in
  find_repeat contents
