open Core_kernel;

let size = 128;

let rec count_ones = (n) => {
  if (n == 0) {
    0;
  } else if (n land 1 == 1) {
    1 + count_ones(n lsr 1);
  } else {
    count_ones(n lsr 1);
  };
};

let is_one = (graph, (i, j)) => {
  if (i < 0 || i >= size || j < 0 || j >= size) {
    false
  } else {
    let row = List.nth_exn(graph, j);
    let n = List.nth_exn(row, i / 8);
    (1 lsl (7 - (i % 8))) land n != 0
  }
};

let cmp = ((x1, y1), (x2, y2)) => {
  switch (Int.compare(x1, x2)) {
    | 0 => Int.compare(y1, y2)
    | d => d
  };
};

let contains_coord = (cs, coord) => List.exists(~f=(p) => cmp(p, coord) == 0, cs);

let get_neighbour_ones = (graph, (i, j), exclude) =>
    [(i-1, j), (i+1, j), (i, j-1), (i,j+1)]
      |> List.filter(~f=((x, y)) => !contains_coord(exclude, (x, y)))
      |> List.filter(~f=is_one(graph));

let rec label = (graph, new_points, old_points) => {
  switch (new_points) {
    | [] => old_points
    | [p, ...ps] => {
      let neighbours = get_neighbour_ones(graph, p, List.append(new_points, old_points));
      label(graph, List.append(ps, neighbours), [p, ...old_points])
    }
  };
};

let sum = List.fold(~f=(+), ~init=0);

let solve1 = (graph) => {
  let count_row = (r) => r |> List.map(~f=count_ones) |> sum;
  graph |> List.map(~f=count_row) |> sum
};

let solve2 = (graph) => {
  /*
     We solve this by keeping track of all coordinates in regions we have found
  */
  let f = ((regions, cnt), p) => {
    if (contains_coord(regions, p) || !is_one(graph, p)) {
      (regions, cnt);
    } else {
      let reg = label(graph, [p], []);
      (List.append(reg, regions), cnt+1)
    };
  };
  /* There must be a better way to generate all coords (0, 0) to (127, 127) ?? */
  let coords = List.range(0, 128) |> List.map(~f=(i) => List.range(0, 128) |> List.map(~f=(j) => (i, j))) |> List.concat;
  let (_, cnt) = List.fold(~f, ~init=([], 0), coords);
  cnt
};


let () = {
  let inp_string = "wenycdww";
  let graph = List.range(0, 128) |> List.map(~f=(x) => Knot.knot(inp_string ++ "-" ++ Int.to_string(x)));
  solve1(graph) |> Int.to_string |> print_endline;
  solve2(graph) |> Int.to_string |> print_endline;
};
