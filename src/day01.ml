let parse_rotation (line: string): char * int =
  Scanf.sscanf line "%c%d" (fun c d -> (c, d))

let execute_rotation (pos: int) ((dir, rot): (char * int)) : (int * int) =
  let new_position, on_zero_count = match dir with
    | 'L' -> let raw_rotated = pos - rot in
             let past_zero = raw_rotated <= 0 in
             let new_pos_raw = raw_rotated mod 100 < 0 in
             let new_pos = raw_rotated mod 100 + (if new_pos_raw then 100 else 0) in
             let count_start = if past_zero && pos <> 0 then 1 else 0 in
             let zero_count = count_start + abs ((pos - rot) / 100) in
             new_pos, zero_count
    | 'R' -> (pos + rot) mod 100, (pos + rot) / 100
    | _ -> failwith "invalid instruction"
  in
  print_int pos;
  print_string "\t";
  print_char dir;
  print_string "\t";
  print_int rot;
  print_string "\t";
  print_int new_position;
  print_string "\t";
  print_int on_zero_count;
  print_newline ();
  new_position, on_zero_count
  

let count_0 (position: int) (instructions: (char * int) list) : (int * int) =
  let rec aux acc1 acc2 pos instr =
    match instr with
    | [] -> acc1, acc2
    | h :: t -> match execute_rotation pos h with
                | p, past_zero -> aux
                                    (if p = 0 then acc1 + 1 else acc1)
                                    (acc2 + past_zero)p t
  in aux 0 0 position instructions
