open Core;

let rot_left = (steps, lst) => {
  let (a, b) = List.split_n(lst, steps);
  List.append(b,a);
};

let rot_right = (steps, lst) => rot_left(List.length(lst) - steps, lst);

let rev = (lst, pos, length) => {
  let lst' = rot_left(pos, lst);
  let (a, b) = List.split_n(lst', length);
  List.append(List.rev(a), b) |> rot_right(pos);
};

let step = ((lst, pos, skip), length) => {
  let next_lst = rev(lst, pos, length);
  let next_pos = (pos + length + skip) % List.length(lst);
  let next_skip = skip + 1;
  (next_lst, next_pos, next_skip)
};

let round = (inp, state) => List.fold(~f=step, ~init=state, inp);

let solve1 = (inp) => {
  let init = (List.range(0, 256), 0, 0);
  let (lst, _, _) = round(inp, init);
  List.nth_exn(lst, 0) * List.nth_exn(lst, 1);
};

let densify = List.fold(~f=(lxor), ~init=0);

assert(densify([65 , 27 , 9 , 1 , 4 , 3 , 40 , 50 , 91 , 7 , 6 , 0 , 2 , 5 , 68 , 22]) == 64);

let rec foldi = (f, i, acc) => {
  if (i <= 0) { acc } else { foldi(f, i - 1, f(acc)) };
};

let solve2 = (inp_str) => {
  let inp = inp_str |> String.to_list |> List.map(~f=Char.to_int);
  let inp = List.append(inp, [17, 31, 73, 47, 23]);
  let state = (List.range(0, 256), 0, 0);
  let (lst, _, _) = foldi(round(inp), 64, state);
  List.groupi(~break=(i, _, _) => i % 16 == 0, lst)
    |> List.map(~f=densify)
    |> List.map(~f=Printf.sprintf("%02x"))
    |> String.concat;
};

let () = {
  let inp_string = In_channel.stdin
    |> In_channel.input_line
    |> (Option.value_exn: (option(string) => string));
  let inp1 = inp_string |> String.split(~on=',') |> List.map(~f=Int.of_string);
  solve1(inp1) |> Int.to_string |> print_endline;
  solve2(inp_string) |> print_endline;
};
