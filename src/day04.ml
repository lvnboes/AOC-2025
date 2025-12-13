let map_warehouse_line (line : string) =
  let map_location = function
    | '.' -> 0
    | '@' -> 1
    | _ -> failwith "invalid character"
  in
  line |> String.to_seq |> Seq.map map_location |> List.of_seq

let padd_with (lst : 'a list) (padding : 'a) (f : 'a -> 'a) : 'a list =
  let rec aux = function [] -> [ padding ] | h :: t -> f h :: aux t in
  padding :: aux lst

let padd_matrix (matrix : int list list) =
  let matrix_width = match matrix with [] -> 0 | h :: _ -> List.length h in
  let matrix_padding = List.init (matrix_width + 2) (fun _ -> 0) in
  padd_with matrix matrix_padding (fun row -> padd_with row 0 (fun x -> x))

let check_row (before : int list) (self : int list) (next : int list)
    (count : int) : int list * int =
  let rec aux acc count before self next =
    match (before, self, next) with
    | ( a :: (b :: c :: _ as t1),
        d :: (e :: f :: _ as t2),
        g :: (h :: i :: _ as t3) ) ->
        if e = 1 && a + b + c + d + f + g + h + i < 4 then
          aux (0 :: acc) (count + 1) t1 t2 t3
        else aux (e :: acc) count t1 t2 t3
    | _ -> (0 :: acc, count)
  in
  aux [ 0 ] count before self next

let clean_magazine (matrix : int list list) (count : int) : int list list * int
    =
  let matrix_width = match matrix with [] -> 0 | h :: _ -> List.length h in
  let matrix_padding = List.init matrix_width (fun _ -> 0) in
  let rec aux acc count = function
    | before :: (self :: next :: _ as tail) ->
        let row, count = check_row before self next count in
        aux (row :: acc) count tail
    | _ -> (matrix_padding :: acc, count)
  in
  aux [ matrix_padding ] count matrix

let rec solve (matrix : int list list) (count : int) (rounds : int) =
  if rounds = 0 then count
  else
    let upd_matrix, upd_count = clean_magazine matrix count in
    if count = upd_count then upd_count
    else solve upd_matrix upd_count (rounds - 1)

let solve_1 (input : int list list) =
  let matrix = padd_matrix input in
  solve matrix 0 1

let solve_2 (input : int list list) =
  let matrix = padd_matrix input in
  solve matrix 0 (-1)
