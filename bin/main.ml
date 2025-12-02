open Aoc

let day_01 =
  let read_t, data =
    Timer.timed_excecution (fun _ ->
        Reader.read_and_process_lines "data/d1.txt" Day01.parse_rotation)
  in
  let timed_result =
    Timer.timed_excecution (fun _ -> Day01.count_0 50 data)
  in
  Printer.print_day_combined 1 read_t timed_result

let () = day_01
