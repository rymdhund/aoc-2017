open Core;

let (n, ne, se, s, sw, nw) = (0, 1, 2, 3, 4, 5);

let parse_dir = (x) => {
  switch x {
    | "n" => n
    | "ne" => ne
    | "se" => se
    | "s" => s
    | "sw" => sw
    | "nw" => nw
    | _ => assert(false)
  }
};

let plus = (x, y) => (x + y) % 6;
let rev = plus(3);

let add = (pos, dir) => {
  if (pos[rev(dir)] > 0) {
    pos[rev(dir)] = pos[rev(dir)] - 1;
  } else if (pos[plus(dir, 2)] > 0) {
    pos[plus(dir, 2)] = pos[plus(dir, 2)] - 1;
    pos[plus(dir, 1)] = pos[plus(dir, 1)] + 1;
  } else if (pos[plus(dir, 4)] > 0) {
    pos[plus(dir, 4)] = pos[plus(dir, 4)] - 1;
    pos[plus(dir, 5)] = pos[plus(dir, 5)] + 1;
  } else {
    pos[dir] = pos[dir] + 1;
  };
  pos
};

let sum = Array.fold(~f=(+), ~init=0);

let solve1 = (inp) => {
  let res = List.fold(~f=add, ~init=[|0, 0, 0, 0, 0, 0|], inp);
  sum(res)
};

let solve2 = (inp) => {
  let f = ((pos, mx), dir) => {
    let npos = add(pos, dir);
    (npos, max(mx, sum(npos)))
  };
  let (_, mx) = List.fold(~f, ~init=([|0, 0, 0, 0, 0, 0|], 0), inp);
  mx
};

let () = {
  let inp = In_channel.stdin
    |> In_channel.input_line
    |> (Option.value_exn: (option(string) => string))
    |> String.split(~on=',') 
    |> List.map(~f=parse_dir);
  solve1(inp) |> Int.to_string |> print_endline;
  solve2(inp) |> Int.to_string |> print_endline;
};
