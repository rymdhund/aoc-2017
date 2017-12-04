open Core;

let parse_line = String.split(~on=' ');

let valid = (pw) => List.is_empty(List.find_all_dups(~compare=String.compare, pw));

let () = {
  assert (! valid(["aa", "bb", "aa"]));
  assert (valid(["aa", "bb", "ab"]));
};

let solve1 = (pws) => pws |> List.count(~f=valid);

let sort_pw_words = (pw) =>
  pw
  |> List.map(~f=(w) => w |> String.to_list |> List.sort(~cmp=Char.compare) |> String.of_char_list);

let solve2 = (pws) => pws |> List.map(~f=sort_pw_words) |> List.count(~f=valid);

let () = {
  let inp = In_channel.stdin |> In_channel.input_lines |> List.map(~f=parse_line);
  print_endline(solve1(inp) |> Int.to_string);
  print_endline(solve2(inp) |> Int.to_string);
};
