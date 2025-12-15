open Aoc

let day_01 =
  let read_t, data =
    Timer.timed_execution (fun _ ->
        Reader.read_and_process_lines "data/d1.txt" Day01.parse_rotation)
  in
  let timed_result = Timer.timed_execution (fun _ -> Day01.solve 50 data) in
  Printer.print_day_combined 1 read_t timed_result

let day_02 =
  let read_t, data =
    Timer.timed_execution (fun _ ->
        Reader.read_split_and_process_file "data/d2.txt" [","]
          Day02.read_id_range)
  in
  let part_1 = Timer.timed_execution (fun _ -> Day02.solve_1 data) in
  let part_2 = Timer.timed_execution (fun _ -> Day02.solve_2 data) in
  Printer.print_day 2 read_t part_1 part_2

let day_03 =
  let read_t, data =
    Timer.timed_execution (fun _ ->
        Reader.read_and_process_lines "data/d3.txt" Day03.parse_joltages)
  in
  let part_1 = Timer.timed_execution (fun _ -> Day03.solve_1 data) in
  let part_2 = Timer.timed_execution (fun _ -> Day03.solve_2 data) in
  Printer.print_day 3 read_t part_1 part_2

let day_04 =
  let read_t, data =
    Timer.timed_execution (fun _ ->
        Reader.read_and_process_lines "data/d4.txt" Day04.map_warehouse_line)
  in
  let part_1 = Timer.timed_execution (fun _ -> Day04.solve_1 data) in
  let part_2 = Timer.timed_execution (fun _ -> Day04.solve_2 data) in
  Printer.print_day 4 read_t part_1 part_2

let day_05 =
  let read_t, data =
    Timer.timed_execution (fun _ ->
        Reader.read_and_process_file "data/d5.txt" Day05.process_ingr_input)
  in
  let part_1 = Timer.timed_execution (fun _ -> Day05.solve_1 data) in
  let part_2 = Timer.timed_execution (fun _ -> Day05.solve_2 data) in
  Printer.print_day 5 read_t part_1 part_2

let day_06 =
  let read_t, data =
    Timer.timed_execution
      (fun _ ->
        (Reader.read_and_process_lines "data/d6.txt" Day06.parse_line)
        |> Day06.transpose_homework
      )
  in
  let part_1 = Timer.timed_execution (fun _ -> Day06.solve_1 data) in
  let part_2 = 0., 0 in
    (* Timer.timed_execution (fun _ -> Day05.solve_2 data) in *)
  Printer.print_day 5 read_t part_1 part_2

let () =
  day_01;
  day_02;
  day_03;
  day_04;
  day_05;
  day_06
