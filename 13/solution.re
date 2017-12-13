open Core_kernel;

let parse_line = (line) => {
  let parts = String.split(~on=' ', line);
  let d = List.hd_exn(parts) |> String.rstrip(~drop=(==)(':')) |> Int.of_string;
  let r = List.nth_exn(parts, 1) |> Int.of_string;
  (d, r)
};

let caught = (d, r) => d % (2 * r - 2) == 0;

let severity = (d, r) => {
  if (caught(d, r)) {
    r * d;
  } else {
    0
  };
};

let sum = List.fold(~f=(+), ~init=0);

let solve1 = (inp) => {
  List.map(inp, ~f=((d, r)) => severity(d, r))
    |> sum;
};

let rec solve2 = (~delay=0, inp) => {
  let caught = List.exists(inp, ((d, r)) => caught(delay + d, r));
  if (caught) {
    solve2(~delay=delay+1, inp);
  } else {
    delay;
  };
};

let () = {
  let inp = In_channel.stdin |> In_channel.input_lines |> List.map(~f=parse_line);
  solve1(inp) |> Int.to_string |> print_endline;
  solve2(inp) |> Int.to_string |> print_endline;
};
