let parse_joltages line =
  line |> String.to_seq
  |> Seq.map (fun c -> Char.(code c - code '0'))
  |> List.of_seq

let highest_in_available_range (lst : int list) (n_size : int) : int * int =
  let end_range = List.length lst - n_size in
  let rec aux max max_i current_i lst =
    if current_i > end_range then (max, max_i)
    else
      match lst with
      | h :: t when h > max -> aux h current_i (current_i + 1) t
      | _ :: t -> aux max max_i (current_i + 1) t
      | [] -> failwith "Empty list state should not be reachable"
  in
  aux (-1) (-1) 0 lst

let construct_highest_number (lst : int list) (n_size : int) : int =
  let rec to_number = function [] -> 0 | h :: t -> h + (10 * to_number t) in
  let rec aux acc lst n_size =
    match n_size with
    | 0 -> to_number acc
    | _ ->
        let max, max_i = highest_in_available_range lst n_size in
        aux (max :: acc) (List.drop (max_i + 1) lst) (n_size - 1)
  in
  aux [] lst n_size

let solve_1 (input : int list list) : int =
  List.fold_left (fun acc x -> acc + construct_highest_number x 2) 0 input

let solve_2 (input : int list list) : int =
  List.fold_left (fun acc x -> acc + construct_highest_number x 12) 0 input
