open Aoc

let day_01 =
  let read_t, data =
    Timer.timed_excecution (fun _ ->
        Reader.read_and_process_lines "data/d1.txt" Day01.parse_rotation)
  in
  let timed_result =
    Timer.timed_excecution (fun _ -> Day01.solve 50 data)
  in
  Printer.print_day_combined 1 read_t timed_result

let day_02 =
  let read_t, data =
    Timer.timed_excecution (fun _ ->
        Reader.read_split_and_process_file "data/d2.txt" [","]
          Day02.read_id_range)
  in
  let part_1 = Timer.timed_excecution (fun _ -> Day02.solve_1 data) in
  Printer.print_day 2 read_t part_1 (0., 0)

  let () = day_01;
           ignore day_02
