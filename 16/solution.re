/**
  For part 2 we use the fact that for an even number of dances we can ignore the
  Partner operations since they cancel each other out. This gives us a set of simple
  permutations. And we optimize using the associative property of permutations so we
  only have to apply log(n) permutations to get the result
*/
open Core_kernel;

let puzzle_size = 16;

type move =
  | Spin(int)
  | Exchange(int, int)
  | Partner(char, char);

let parse_move = (str) => {
  let hd = String.get(str, 0);
  let rest = String.slice(str, 1, String.length(str));
  switch (hd) {
    | 's' => Spin(Int.of_string(rest))
    | 'x' => {
      let (a, b) = String.lsplit2_exn(~on='/', rest);
      Exchange(Int.of_string(a), Int.of_string(b))
    }
    | 'p' => Partner(String.get(str, 1), String.get(str, 3))
    | _ => assert(false)
  };
};

let dance = (moves, ps1) => {
  let ps = ref(Array.of_list(ps1));
  List.iter(moves, ~f=(m) => {
    /*Array.to_list(ps^) |> String.of_char_list |> print_endline;*/
    switch m {
      | Spin(s) => {
        let (a, b) = Array.partitioni_tf(ps^, (i, _) => (i < puzzle_size-s));
        ps := Array.append(b, a);
      }
      | Exchange(a, b) => Array.swap(ps^, a, b);
      | Partner(x, y) => {
        let (a, _) = Array.findi_exn(ps^, (_, z) => z ==(x));
        let (b, _) = Array.findi_exn(ps^, (_, z) => z ==(y));
        Array.swap(ps^, a, b);
      }
    };
  });
  Array.to_list(ps^);
};

let solve1 = (moves) => {
  let ps = List.range(97, 97+puzzle_size) |> List.map(~f=Char.of_int_exn);
  let res = dance(moves, ps);
  res |> String.of_char_list
};

let permute1 = (perm, xs) => List.map(perm, ~f=(idx) => List.nth_exn(xs, idx));

let rec permute = (perm, n, xs) => {
  /* Use the fact that (x * perm) * perm == x * (perm * perm) to get logarithmic time */
  if (n == 0) {
    xs
  } else if (n == 1) {
    permute1(perm, xs);
  } else {
    let perm2 = permute1(perm, perm);
    let res = permute(perm2, n/2, xs);
    permute(perm, n%2, res)
  }
};

let solve2 = (moves) => {
  /*
     If we apply the dance an even number of times, the partner shifts cancel each other out.
     So we can ignore them
   */
  let moves_wo_partner = List.filter(~f=(m) => switch m {
    | Partner(_, _) => false
    | _ => true
  }, moves);
  /* make a permutation of the moves as an array(int) */
  let permutation = List.range(0, puzzle_size)
    |> List.map(~f=Char.of_int_exn)
    |> dance(moves_wo_partner)
    |> List.map(~f=Char.to_int);

  let start_ints = List.range(0, puzzle_size);
  let ps = permute(permutation, 1000000000, start_ints); 

  ps |> List.map(~f=(i) => Char.of_int_exn(i+97)) |> String.of_char_list
};

let () = {
  let inp = In_channel.stdin
    |> In_channel.input_line
    |> (Option.value_exn: (option(string) => string))
    |> String.split(~on=',') 
    |> List.map(~f=parse_move);
  solve1(inp) |> print_endline;
  solve2(inp) |> print_endline;
};
