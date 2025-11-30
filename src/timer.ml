let timed_excecution f =
  let t_start = Unix.gettimeofday () in
  let result = f() in
  let t_end = Unix.gettimeofday () in
  let time = (t_end -. t_start) * 1000000. in
  (time, result)
