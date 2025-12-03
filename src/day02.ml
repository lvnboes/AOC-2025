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
  else if og_bottom_id < nw_bottom_id
  then og_top_id + nw_bottom_id
  else if (og_bottom_id + 1) = divisor
  then (i_pow 10 (size + 1)) + (i_pow 10 (size / 2))
  else og_top_id + divisor + nw_bottom_id + 1

let rec invalid_in_range (acc: int list) (lower: int) (upper: int) =
  let next = next_invalid_id lower in
  if next > upper
  then acc
  else invalid_in_range (next :: acc) next upper

let invalid_in_all_ranges (ranges: (int * int) list) : int list =
  let rec aux acc ranges =
    match ranges with
    | [] -> List.rev acc
    | (lower, upper) :: tail ->
       aux (invalid_in_range acc (lower - 1) upper) tail
  in
  aux [] ranges

let solve_1 (input: (int * int) list) : int =
  List.fold_left ( + ) 0 (invalid_in_all_ranges input)



(*repeat a patttern n of a specific size a specific number of times*)
let repeat_pattern n times size = 
  let rec aux acc n times = 
    if times = 0 then acc
    else aux (acc + n) ((i_pow 10 size) * n) (times - 1)
  in
  aux 0 n times

(*get all n times repeated paterns of a specific size between n and max*)
let rec repeat_patterns_of_size acc n times p_size upper =
  (* print_string "N "; print_int n; print_newline (); *)
  (* print_string "T "; print_int times; print_newline (); *)
  (* print_string "S "; print_int p_size; print_newline (); *)
  (* print_string "U "; print_int upper; print_newline (); *)
  if (count_id_digits n) > p_size || n > upper
  then acc
  else let next = repeat_pattern n times p_size in
       if next > upper
       then acc
       else repeat_patterns_of_size (next :: acc) (n + 1) times p_size upper

(*determine lowest pattern of a specific size that when repeated to an integer
 of the same order of magnitude as number n is bigger tnan n*)
let group_digits n size =
  let rec aux acc n n_size g_size =
    let divisor = i_pow 10 (n_size - g_size) in
    let top = n / divisor in
    let rest = n - (top * divisor) in
    if n_size = g_size then List.rev (top :: acc)
    else aux (top :: acc) rest (n_size - g_size) g_size
  in
  aux [] n (count_id_digits n) size

let lowest_pattern n size =
  let patterns = group_digits n size in
  let rec has_higher x lst =
    match lst with
    | [] -> false
    | h :: _ when h > x -> true
    | _ :: t -> has_higher x t
  in
  match patterns with
  | first :: rest -> if has_higher first rest then first + 1 else first
  | _ -> failwith "invalid grouping"

(*all invalid ids between lower and upper limit*)
let invalid_all_pattern_sizes_in_range acc lower upper =
  let rec aux acc n p_size size lower upper =
    if p_size = size / 2
    then repeat_patterns_of_size acc n (size / p_size) p_size upper
    else if size mod p_size = 0
    then (print_string "E1"; aux (repeat_patterns_of_size acc n (size / p_size) p_size upper)
           (lowest_pattern lower (p_size + 1)) (p_size + 1) size lower upper)
    else (print_string "E2"; aux acc (lowest_pattern lower (p_size + 1)) (p_size + 1) size lower upper)
  in
  let size = count_id_digits lower in
  let min_n = lowest_pattern lower 1 in
  aux acc min_n 1 size lower upper

(*split range in chuncks of the same order of magnitude*)
let rec split_range acc a b =
  let size_lower = count_id_digits a in
  let new_top = (i_pow 10 size_lower) in
  if new_top > b then ((a, b) :: acc)
  else  split_range ((a, new_top - 1) :: acc) new_top b

(*split range and get all repeat patterns*)
let all_invalid_in_range acc lower upper =
  let rec aux acc ranges =
    match ranges with
    | [] -> acc
    | (lower, upper) :: rest ->
       print_int lower; print_string " | "; print_int upper; print_newline ();
       aux (invalid_all_pattern_sizes_in_range acc lower upper) rest
  in
  let ranges = split_range [] lower upper in
  aux acc ranges

(* let rec all_invalid_in_all_ranges acc ranges = *)
(*   match ranges with *)
(*   | [] -> acc *)
(*   |  *)
 
