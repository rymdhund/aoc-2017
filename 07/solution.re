open Core;

let sum = List.fold(~f=(+), ~init=0);

let parse_line = (line) => {
  let parts = String.split(~on=' ', line);
  let name = List.hd_exn(parts);
  let weight = List.nth_exn(parts,1) |> String.strip(~drop=(c) => c == '(' || c == ')') |> Int.of_string;
  let children = List.drop(parts, 3) |> List.map(~f=String.rstrip(~drop=(c) => c == ','));
  (name, (weight, children));
};

let solve1 = (inp) => {
  let all_names = List.map(inp, ((name, _)) => name);
  let children = List.map(inp, ((_, (_, children))) => children) |> List.concat;
  List.hd_exn(List.filter(all_names, (name) => !List.exists(children, ~f=(x) => x == name)))
};

type entry_type = (string, (int, list(string)));

let find_entry = (entries: list(entry_type) , name) => List.Assoc.find_exn(~equal=(==), entries, name);

let get_weight = (entries, name) => {
  let (weight,_) = find_entry(entries, name);
  weight
};

let rec tot_weight = (entries, name) => {
  let cmp = (n1, n2) => n1 == n2;
  let (weight, children) = List.Assoc.find_exn(~equal=cmp, entries, name);
  let c_weight = List.map(~f=(x) => tot_weight(entries, x), children) |> sum;
  weight + c_weight;
};

let rec balance = (entries: list(entry_type), xs: list(string)) => {
  let ans = xs |> List.map(~f=find_entry(entries)) |> List.find_map(~f=((w, c)) => balance(entries, c));
  if (Option.is_some(ans) || List.is_empty(xs)) {
    ans;
  } else {
    let name_weights = List.map(xs, ~f=(n) => (n, tot_weight(entries, n)));
    let (_, w1) = List.hd_exn(name_weights);
    switch (List.partition_tf(name_weights, ((n,w)) => w == w1)) {
      | (_, []) => None /* same weights */
      | ([(xn, xw)], [(yn, yw), ...ys])
      | ([(yn, yw), ...ys], [(xn, xw)]) => Some(get_weight(entries, xn) + yw-xw) /* single diff in x */
      | _ => assert(false)
    };
  };
};

let solve2 = (inp) => {
  let (_, cs) = find_entry(inp, solve1(inp));
  Option.value_exn(balance(inp, cs));
};

let () = {
  let inp = In_channel.stdin |> In_channel.input_lines |> List.map(~f=parse_line);
  solve1(inp) |> print_endline;
  solve2(inp) |> Int.to_string |> print_endline;
};
