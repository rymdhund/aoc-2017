open Core;

let pairs = (offset, xs) => {
  let (a, b) = List.split_n(xs, offset);
  let ys = List.append(b, a);
  Option.value_exn(List.zip(xs, ys))
};

let sum = List.fold(~f=(+), ~init=0);

let solve = (pairing, input) =>
  input
  |> String.to_list
  |> List.map(~f=(c) => Char.to_int(c) - 48)
  |> pairing
  |> List.filter(~f=((a, b)) => a == b)
  |> List.map(~f=((a, b)) => a)
  |> sum;

let solve1 = solve(pairs(1));

let () = {
  assert (solve1("1122") == 3);
  assert (solve1("1111") == 4);
  assert (solve1("1234") == 0);
  assert (solve1("91212129") == 9)
};

let solve2 = solve((xs) => pairs(List.length(xs) / 2, xs));

let () = {
  assert (solve2("1212") == 6);
  assert (solve2("1221") == 0);
  assert (solve2("123425") == 4);
  assert (solve2("123123") == 12);
  assert (solve2("12131415") == 4)
};

let () = {
  let input = Option.value_exn(In_channel.stdin |> In_channel.input_line);
  Printf.printf("First is %d\n", solve1(input));
  Printf.printf("Second is %d\n", solve2(input))
};
