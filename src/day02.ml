let read_id_range (range: string) : int * int =
  Scanf.sscanf range "%d-%d" (fun start ending -> (start, ending))

let count_id_digits (id: int) =
  let rec count_by_division count n =
    match n with
    | 0 -> count
    | x -> count_by_division (count + 1) (x / 10)
  in
  count_by_division 0 id

let i_pow (n: int) (pw: int): int =
  let rec aux acc n pw =
    if pw = 0 then acc else aux (acc * n) n (pw - 1)
  in
  aux 1 n pw

let next_invalid_id (id: int) : int =
  let size = count_id_digits id in
  let divisor = i_pow 10 (size / 2) in
  let nw_bottom_id = id / divisor in
  let og_top_id = nw_bottom_id * divisor in
  let og_bottom_id = id - og_top_id in
  if (size mod 2) <> 0
  then (i_pow 10 size) + (i_pow 10 (size / 2))
  else
    if og_bottom_id < nw_bottom_id
    then og_top_id + nw_bottom_id
    else
      if (og_bottom_id + 1) = divisor
      then (i_pow 10 (size + 1)) + (i_pow 10 (size / 2))
      else og_top_id + divisor + og_bottom_id + 1

let rec invalid_in_range (acc: int list) (lower: int) (upper: int) =
  let next = next_invalid_id lower in
  if next > upper
  then acc
  else invalid_in_range (next :: acc) next upper
