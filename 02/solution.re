open Core;

let sum = List.fold(~f=(+), ~init=0);

let solve1 = (rows) => {
  let max_list = List.fold_left(~f=(a, b) => max(a, b), ~init=0);
  let min_list = List.fold_left(~f=(a, b) => min(a, b), ~init=Int.max_value);
  let row_diff = (xs) => max_list(xs) - min_list(xs);
  let diffs = List.map(~f=row_diff, rows);
  sum(diffs)
};

let parse_line = (line) => {
  let lines = String.split(line, ~on='\t');
  List.map(~f=Int.of_string, lines)
};

let rec find_divisor = (x, ys) =>
  switch ys {
  | [b, ...bs] =>
    if (b % x == 0) {
      Some(b / x)
    } else if (x % b == 0) {
      Some(x / b)
    } else {
      find_divisor(x, bs)
    }
  | [] => None
  };

let rec div = (lst) => {
  let a = List.hd(lst) |> (Option.value_exn: Option.t(int) => int);
  let b = List.tl(lst) |> (Option.value_exn: Option.t(List.t(int)) => List.t(int));
  switch (find_divisor(a, b)) {
  | Some(d) => d
  | None => div(b)
  }
};

let () = {
  assert (parse_line("1\t2\t3") == [1, 2, 3]);
  assert (div([2, 8]) == 4);
  assert (div([9, 4, 7, 3]) == 3);
  assert (div([3, 8, 6, 5]) == 2)
};

let solve2 = (rows) => {
  List.map(~f=div, rows)
  |> sum
};

let () = {
  let inp = In_channel.stdin |> In_channel.input_lines |> List.map(~f=parse_line);
  print_endline(solve1(inp) |> Int.to_string);
  print_endline(solve2(inp) |> Int.to_string)
};
