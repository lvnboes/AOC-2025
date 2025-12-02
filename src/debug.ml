let pp_int_list lst =
  let rec print_elements = function
    | [] -> ()
    | h :: t -> print_int h; print_string "; "; print_elements t
  in
  print_string "[";
  print_elements lst;
  print_string "]";
