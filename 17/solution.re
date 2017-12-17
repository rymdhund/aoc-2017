/**
  For part two we dont need to construct the buffer, we can just keep track of
  the last value written to pos 1
*/
open Core_kernel;

let step_add = (step, (buffer, pos)) => {
  let newpos = (pos + step) % List.length(buffer) + 1;
  let (a, b) = List.split_n(buffer, newpos);
  let newbuffer = List.concat([a, [List.length(buffer)], b]);
  (newbuffer, newpos)
};

let rec do_n_times = (f, inp, n) => {
  if (n == 0) {
    inp
  } else {
    do_n_times(f, f(inp), n-1)
  }
};

let solve1 = (stepsize) => {
  let (buf, pos) = do_n_times(step_add(stepsize), ([0], 0), 2017);
  List.nth_exn(buf, pos+1);
};

let step_add2 = (step, (pos, n, value)) => {
  let newpos = (pos + step) % n + 1;
  let value = if (newpos == 1) {
    n
  } else {
    value
  };
  (newpos % (n + 1), (n + 1), value);
};

let solve2 = (stepsize) => {
  let (_, _, value) = do_n_times(step_add2(stepsize), (0, 1, 0), 50000000);
  value
};

let () = {
  let stepsize = 324;
  solve1(stepsize) |> Int.to_string |> print_endline;
  solve2(stepsize) |> Int.to_string |> print_endline;
};
