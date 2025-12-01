let parse_rotation (line: string): char * int =
  Scanf.sscanf line "%c%d" (fun c d -> (c, d))

let execute_rotation (pos: int) ((dir, rot): (char * int)) : (int * int) =
  let directional_rotation = if dir = 'L' then (- rot) else rot in
  let turned_position = pos + directional_rotation in
  let times_past_zero = (if turned_position <= 0 then 1 else 0) +
                          (abs ((pos + directional_rotation) / 100)) in
  print_int pos;
  print_string "\t";
  print_int directional_rotation;
  print_string "\t";
  print_int ((turned_position mod  100) + (if (turned_position mod 100) < 0 then 100 else 0));
  print_string "\t";
  print_int times_past_zero; print_newline ();
  (turned_position mod  100) + (if (turned_position mod 100) < 0 then 100 else 0),
  times_past_zero

let count_0 (position: int) (instructions: (char * int) list) : (int * int) =
  let rec aux acc1 acc2 pos instr =
    match instr with
    | [] -> acc1, acc2
    | h :: t -> match execute_rotation pos h with
                | p, past_zero -> aux
                                    (if p = 0 then acc1 + 1 else acc1)
                                    (acc2 + past_zero)p t
  in aux 0 0 position instructions
