let map_warehouse_line (line: string) =
  let map_location = function
    | '.' -> 0
    | '@' -> 1
    | _ -> failwith "invalid character"
  in
  line |> String.to_seq |> Seq.map map_location |> List.of_seq

let padd (lst: 'a list) (padding: 'a) (f: 'a -> 'a): 'a list =
  let rec aux = function
    | [] -> [padding]
    | h::t -> (f h)::(aux t)
  in
  padding::(aux lst)

let padd_matrix (matrix: int list list) =
  let matrix_width = match matrix with [] -> 0 | h::_ -> List.length h in
  let matrix_padding = List.init (matrix_width + 2) (fun _ -> 0) in
  padd matrix matrix_padding (fun row -> padd row 0 (fun x -> x))
