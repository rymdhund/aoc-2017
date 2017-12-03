open Core;

let sum = List.fold(~f=(+), ~init=0);

let rec layer = (d, n) =>
  if ((2 * n - 1) * (2 * n - 1) < d && d <= (2 * n + 1) * (2 * n + 1)) {
    n
  } else {
    layer(d, n + 1)
  };

let closests = (n) => {
  let k = (2 * n - 1) * (2 * n - 1) + n;
  [k, k + 2 * n, k + 4 * n, k + 6 * n]
};

let solve1 = (d) => {
  let n = layer(d, 1);
  let a =
    closests(n)
    |> List.map(~f=(x) => abs(d - x))
    |> List.fold_left(~f=(x, y) => min(x, y), ~init=Int.max_value);
  n + a
};

let next = ((x, y), (dx, dy)) =>
  if (abs(x) == abs(y)) {
    switch (x > 0, y > 0) {
    | (true, true) => ((x - 1, y), ((-1), 0))
    | (false, true) => ((x, y - 1), (0, (-1)))
    | (false, false) => ((x + 1, y), (1, 0))
    | (true, false) => ((x + 1, y), (0, 1))
    }
  } else {
    ((x + dx, y + dy), (dx, dy))
  };

let () = {
  assert (next((1, 0), (0, 1)) == ((1, 1), (0, 1)));
  assert (next((1, 1), (0, 1)) == ((0, 1), ((-1), 0)))
};

let get_num = (map, (x, y)) => {
  let cmp = ((a, b), (c, d)) => a == c && b == d;
  [
    (x + 1, y + 1),
    (x, y + 1),
    (x - 1, y + 1),
    (x - 1, y),
    (x - 1, y - 1),
    (x, y - 1),
    (x + 1, y - 1),
    (x + 1, y)
  ]
  |> List.filter_map(~f=List.Assoc.find(map, ~equal=cmp))
  |> sum
};

let rec solve2_rec = (map, pos, dir, stop) => {
  let num = get_num(map, pos);
  if (num > stop) {
    num
  } else {
    let (npos, ndir) = next(pos, dir);
    solve2_rec([(pos, num), ...map], npos, ndir, stop)
  }
};

let solve2 = (d) => solve2_rec([((0, 0), 1)], (1, 0), (0, 1), d);

let () = {
  assert (solve1(12) == 3);
  assert (solve1(23) == 2);
  assert (solve1(1024) == 31)
};

let () = {
  solve1(361527) |> Int.to_string |> print_endline;
  solve2(361527) |> Int.to_string |> print_endline
};
