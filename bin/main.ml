open Aoc

let day_01 =
  let read_t, data = Timer.timed_excecution
               (fun _ -> Reader.read_and_process_lines
                           "data/d1.txt"
                           Day01.parse_rotation)
  in
  let p1_t, (res_1, res_2) =
    Timer.timed_excecution (fun _ -> Day01.count_0 50 data) in
    print_float read_t; print_newline ();
    print_float p1_t; print_newline ();
    print_int res_1; print_newline ();
    print_int res_2; print_newline ()

let () = day_01
