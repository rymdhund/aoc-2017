open Core;

let next = (state) => {
  let (idx, mx) =
    List.foldi(~f=(i, (idx, mx), x) => if (x > mx) { (i, x) } else { (idx, mx) }, ~init=(0, 0), state);
  let len = List.length(state);
  let dist = (i, j) => (i + 2 * len - j - 1) % len;
  let new_value = (j, d) => {
    let to_add = (mx - dist(j, idx) - 1 + len) / len;
    if (j != idx) {
      to_add + d;
    } else {
      to_add;
    };
  };
  List.mapi(~f=new_value, state);
};

let rec find_first_dup = (history, i, state) =>
  if (List.exists(~f=List.equal(state, ~equal=(==)), history)) {
    (i, state);
  } else {
    find_first_dup([state, ...history], i + 1, next(state));
  };

let solve1 = (inp) => {
  let (i, _) = inp |> find_first_dup([], 0);
  i;
};

let solve2 = (inp) => {
  let (i, state) = inp |> find_first_dup([], 0);
  let (j, _) = state |> find_first_dup([], 0);
  j;
};

let () = {
  let inp =
    In_channel.stdin
    |> In_channel.input_line_exn
    |> String.split(~on='\t')
    |> List.map(~f=Int.of_string);
  inp |> solve1 |> Int.to_string |> print_endline;
  inp |> solve2 |> Int.to_string |> print_endline;
};
