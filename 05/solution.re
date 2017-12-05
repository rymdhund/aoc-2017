open Core;

let solve = (inp, change) => {
  let arr = Array.of_list(inp);
  let pos = ref(0);
  let i = ref(0);
  while (pos^ >= 0 && pos^ < Array.length(arr)) {
    let jmp = arr[pos^];
    arr[pos^] = change(jmp);
    pos := pos^ + jmp;
    i := i^ + 1;
  };
  i^;
};

let solve1 = (inp) => solve(inp, (x) => x + 1);

let solve2 = (inp) => solve(inp, (x) => if (x < 3) { x + 1 } else { x - 1 });

let () = {
  let inp = In_channel.stdin |> In_channel.input_lines |> List.map(~f=Int.of_string);
  solve1(inp) |> Int.to_string |> print_endline;
  solve2(inp) |> Int.to_string |> print_endline;
};
