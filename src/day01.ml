type dir = L | R

let parse_rotation (line : string) : dir * int =
  Scanf.sscanf line "%c%d" (fun c d -> ((if c = 'L' then L else R), d))

let execute_rotation (pos : int) ((dir, rot) : dir * int) : int * int =
  let cross_0 old nw =
    if (old < 0 && nw >= 0) || (old > 0 && nw <= 0) then 1 else 0
  in
  match dir with
  | L ->
      let new_pos = pos - rot in
      (new_pos mod 100, abs (new_pos / 100) + cross_0 pos new_pos)
  | R ->
      let new_pos = pos + rot in
      (new_pos mod 100, (new_pos / 100) + cross_0 pos new_pos)

let count_0 (position : int) (instructions : (dir * int) list) : int * int =
  let rec aux acc1 acc2 pos instr =
    match instr with
    | [] -> (acc1, acc2)
    | h :: t -> (
        match execute_rotation pos h with
        | p, past_zero ->
            aux (if p = 0 then acc1 + 1 else acc1) (acc2 + past_zero) p t)
  in
  aux 0 0 position instructions
