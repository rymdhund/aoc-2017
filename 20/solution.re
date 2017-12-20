open Core_kernel;

let parse_part = (part) => {
  let [p, v, a] = String.strip(~drop=(c) => !(Char.is_digit(c) || c == '-') , part)
    |> String.split(~on=',');
  (Int.of_string(p), Int.of_string(v), Int.of_string(a));
};

let parse_line = (str) => switch(String.split(~on=' ', str)) {
  | [p, v, a] => (parse_part(p), parse_part(v), parse_part(a))
  | _ => assert(false)
};

let cabs = ((x, y, z)) => abs(x) + abs(y) + abs(z);

let acc_value = ((p, v, a)) => cabs(a);

let solve1 = (parts) => {
  let f = (i: int, (mini: int, minv: int), p) => if (acc_value(p) < minv) {
    (i, acc_value(p))
  } else {
    (mini, minv)
  };
  let (mini, _) = List.foldi(~f, ~init=(0, Int.max_value), parts);
  mini
};

let step_part = (p) => {
  let ((x, y, z), (vx, vy, vz), (ax, ay, az)) = p;
  let (vx, vy, vz) = (vx + ax, vy + ay, vz + az);
  ((x + vx, y + vy, z + vz), (vx, vy, vz), (ax, ay, az))
};

let equal_pos = (part1, part2) => {
  let (p1, _, _) = part1;
  let (p2, _, _) = part2;
  p1 == p2;
};

let rec rm_dups = (eq, xs) => switch (xs) {
  | []
  | [_] => xs
  | [x, ...xs] => if (List.exists(xs, eq(x))) {
    rm_dups(eq, List.filter(~f=(y) => !eq(x, y), xs))
  } else {
    [x, ...rm_dups(eq, xs)]
  }
};

let step = (particles) => particles
  |> rm_dups(equal_pos)
  |> List.map(~f=step_part);

let solve2 = (particles) => {
  let x = ref(particles);
  /* This is a bit of a hack :) */
  for (i in 1 to 100) {
    x := step(x^);
  };
  List.length(x^)
};

let () = {
  let inp = In_channel.stdin
    |> In_channel.input_lines
    |> List.map(~f=parse_line);
  solve1(inp) |> Int.to_string |> print_endline;
  solve2(inp) |> Int.to_string |> print_endline;
};
