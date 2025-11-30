(* Read line by line *)

let rec get_processed_lines f ch acc =
  match input_line ch with
  | line -> get_processed_lines f ch (f line :: acc)
  | exception End_of_file ->
      close_in ch;
      List.rev acc

let read_and_process_lines uri f =
  let ch = open_in uri in
  let lines = get_processed_lines f ch [] in
  lines

let read_lines uri = read_and_process_lines uri (fun x -> x)

(* Read entire file *)

let read_file uri =
  let ch = open_in uri in
  let file = really_input_string ch (in_channel_length ch) in
  close_in ch;
  file

let read_split_and_process_file uri seps f =
  let re = Re.(compile (alt (List.map str seps))) in
  read_file uri |> Re.split re |> List.map f

let read_and_split_file uri sep =
  read_split_and_process_file uri sep (fun x -> x)

let read_and_process_file uri f = f (read_file uri)
