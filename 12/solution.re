open Core;

let sum = List.fold(~f=(+), ~init=0);

let parse_line = (line) => {
  let parts = String.split(~on=' ', line);
  let name = List.hd_exn(parts) |> Int.of_string;
  let children = List.drop(parts, 2) |> List.map(~f=String.rstrip(~drop=(==)(',')))
  |> List.map(~f=Int.of_string);
  (name, children);
};

let get_neighbours = (graph, nodes) => {
  List.map(~f=List.Assoc.find_exn(graph, ~equal=(==)), nodes)
    |> List.concat
    |> List.dedup;
};

let subtract_list = (a, b) => {
  List.filter(a, ~f=(x) => !List.exists(b, ~f=(==)(x)))
};

let get_new_neighbours = (graph, seen, nodes): (list(int), list(int)) => {
  let ns = get_neighbours(graph, nodes);
  let unseen = subtract_list(ns, seen);
  (List.append(seen, unseen), unseen)
};

let rec apply_while = (f, acc, v) => {
  switch (f(acc, v)) {
    | None => acc
    | Some((acc', v')) => apply_while(f, acc', v')
  }
};

let get_cluster = (graph, start_node) => {
  let f = (seen, nodes) => {
    switch nodes {
      | [] => None
      | _ => Some(get_new_neighbours(graph, seen, nodes))
    };
  };
  apply_while(f, [], [start_node]);
};

let solve1 = (inp) => {
  List.length(get_cluster(inp, 0));
};

let rec num_groups = (inp, subset) => {
  switch subset {
    | [] => 0
    | [node, ...xs] => 
    let nodes = get_cluster(inp, node);
    let subset' = subtract_list(subset, nodes);
    num_groups(inp, subset') + 1;
  }
};

let solve2 = (graph) => {
  let all_nodes = List.map(graph, ~f=((k, _)) => k);
  num_groups(graph, all_nodes);
};

let () = {
  let inp = In_channel.stdin |> In_channel.input_lines |> List.map(~f=parse_line);
  solve1(inp) |> Int.to_string |> print_endline;
  solve2(inp) |> Int.to_string |> print_endline;
};
