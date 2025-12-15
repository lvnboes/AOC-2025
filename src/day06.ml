let parse_line (line: string): string list =
  Str.(split (regexp " +") (String.trim line))

let transpose_homework (matrix: string list list) : (string * int list) list =
  let rec aux acc matrix =
    let to_calc = function
      | h::t -> h, (List.map int_of_string t) | _ -> invalid_arg "to_calc"
    in
    match matrix with
    | [] :: _ ->  acc
    | _ -> List.(aux
                   (to_calc (rev (map hd matrix))::acc)
                   (map tl matrix))
  in
  aux [] matrix

let solve_row = function
  | "*", nums -> List.fold_left ( * ) 1 nums
  | "+", nums -> List.fold_left ( + ) 0 nums
  | _ -> invalid_arg "solve_row"

let solve_1 (input: (string * int list) list) =
  List.fold_left (fun acc row -> acc + (solve_row row)) 0 input
