let print_day day read_time (time1, part1) (time2, part2) =
  Printf.printf "Day %d\t| Time\t\t| Result\n" day;
  Printf.printf "%s\n" (String.make 32 '-');
  Printf.printf "Parsing\t| %.3f µs\t|\n" read_time;
  Printf.printf "Part 1\t| %.3f µs\t| %d\n" time1 part1;
  Printf.printf "Part 2\t| %.3f µs\t| %d\n" time2 part2;
  print_newline ()

let print_day_combined day read_time (solve_time, (part1, part2)) =
  print_day day read_time (solve_time, part1) (0., part2)
