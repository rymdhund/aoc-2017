open Core;

type state =
  | Normal
  | Garbage
  | GarbageIgnore
;

let step = (state, c) => {
  let (depth, st, points, gc) = state;
  switch (st, c) {
    | (GarbageIgnore, _)  => (depth,   Garbage,       points,       gc)
    | (Garbage, '!')      => (depth,   GarbageIgnore, points,       gc)
    | (Garbage, '>')      => (depth,   Normal,        points,       gc)
    | (Garbage,  _ )      => (depth,   Garbage,       points,       gc + 1)
    | (Normal,  '<')      => (depth,   Garbage,       points,       gc)
    | (Normal,  '{')      => (depth+1, Normal,        points+depth, gc)
    | (Normal,  '}')      => (depth-1, Normal,        points,       gc)
    | (Normal,   _ )      => (depth,   Normal,        points,       gc)
  };
};

let solve1 = (inp) => {
  let init = (1, Normal, 0, 0);
  let (_, _, points, _) = List.fold(~f=step, ~init, inp);
  points
};

let solve2 = (inp) => {
  let init = (1, Normal, 0, 0);
  let (_, _, _, gc) = List.fold(~f=step, ~init, inp);
  gc
};

let () = {
  let inp = In_channel.stdin |> In_channel.input_line |> (Option.value_exn: (option(string) => string)) |> String.to_list;
  solve1(inp) |> Int.to_string |> print_endline;
  solve2(inp) |> Int.to_string |> print_endline;
};
