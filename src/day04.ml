let map_warehouse_line line =
  let map_location = function
    | '.' -> 0
    | '@' -> 1
    | _ -> failwith "invalid character"
  in
  line |> String.to_seq |> Seq.map map_location |> List.of_seq
