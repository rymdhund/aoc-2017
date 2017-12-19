open Core_kernel;

let dirs = [|(0, -1), (-1, 0), (0, 1), (1, 0)|];

let move = (pos, dir) => {
  let (x, y) = pos;
  let (dx, dy) = dirs[dir];
  (x + dx, y + dy)
};

let change_dir = (maze, pos, dir) => {
  /* look to the left and right */
  let dir_left = (dir + 3) % 4;
  let dir_right = (dir + 1) % 4;
  let (x, y) = move(pos, dir_left);
  if (maze[y][x] != ' ') {
    dir_left
  } else {
    dir_right
  };
};

let rec step = (maze, pos, dir, acc, steps) => {
  let (x, y) = pos;
  let c = maze[y][x];
  switch (c) {
    | ' ' => (acc, steps)  /* we're done */
    | '+' =>  {
      let ndir = change_dir(maze, pos, dir);
      step(maze, move(pos, ndir), ndir, acc, steps+1)
    }
    | '-'
    | '|' => step(maze, move(pos, dir), dir, acc, steps+1)
    | c => step(maze, move(pos, dir), dir, [c, ...acc], steps+1)
  };
};

let solve = (maze) => {
  let (start_x, _) = Array.findi_exn(maze[0], ~f=(i, x) => x =='|');
  step(maze, (start_x, 0), 2, [], 0);
};

let solve1 = (maze) => {
  let (letters, steps) = solve(maze);
  String.of_char_list(List.rev(letters));
};

let solve2 = (maze) => {
  let (letters, steps) = solve(maze);
  steps
};

let () = {
  let inp = In_channel.stdin
    |> In_channel.input_lines
    |> List.map(~f=(x) => Array.of_list(String.to_list(x)))
    |> Array.of_list;
  solve1(inp) |> print_endline;
  solve2(inp) |> Int.to_string |> print_endline;
};
