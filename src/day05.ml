let sort_ranges (ranges : (int * int) list) : (int * int) list =
  List.sort (fun (s1, _) (s2, _) -> compare s1 s2) ranges

let overlap (s1, e1) (s2, e2) = not (e1 < s2 || e2 < s1)

let consolidate_sorted_ranges (ranges : (int * int) list) : (int * int) list =
  let rec aux acc ranges =
    match ranges with
    | [] -> acc
    | (sr, er) :: tr -> (
        match acc with
        | [] -> aux [ (sr, er) ] tr
        | (sa, ea) :: ta ->
            if overlap (sr, er) (sa, ea) then
              aux ((min sr sa, max er ea) :: ta) tr
            else aux ((sr, er) :: acc) tr)
  in
  aux [] ranges

let process_ingr_input (input : string) : (int * int) list * int list =
  let fresh, ingredients =
    match Str.(split (regexp "\n\n") input) with
    | [ fresh; ingredients ] -> (fresh, ingredients)
    | _ -> failwith "invalid input"
  in
  let rec process_ranges = function
    | [] -> []
    | h :: t -> Scanf.sscanf h "%d-%d" (fun s e -> (s, e)) :: process_ranges t
  in
  let fresh_ranges =
    process_ranges (String.split_on_char '\n' fresh)
    |> sort_ranges |> consolidate_sorted_ranges
  in
  let ingredients_lst =
    List.map int_of_string String.(split_on_char '\n' (trim ingredients))
  in
  (fresh_ranges, ingredients_lst)

let is_in_range (ingr_id : int) (id_range : int * int) : bool =
  let s, e = id_range in
  ingr_id >= s && ingr_id <= e

let rec is_in_ranges (ingr_id : int) (id_ranges : (int * int) list) : bool =
  match id_ranges with
  | [] -> false
  | h :: _ when is_in_range ingr_id h -> true
  | _ :: t -> is_in_ranges ingr_id t

let count_fresh (ingredients : int list) (id_ranges : (int * int) list) : int =
  let rec aux count ingrs ranges =
    match ingrs with
    | [] -> count
    | h :: t when is_in_ranges h ranges -> aux (count + 1) t ranges
    | _ :: t -> aux count t ranges
  in
  aux 0 ingredients id_ranges

let solve_1 ((id_ranges : (int * int) list), (ingredients : int list)) : int =
  count_fresh ingredients id_ranges

let solve_2 ((id_ranges : (int * int) list), _) : int =
  List.fold_left (fun acc (s, e) -> acc + e - s + 1) 0 id_ranges
