open Core_kernel;

let generator = (modulo, mult, n) => (n * mult) % modulo;

let matches = (a, b) => (a land 0xffff) == (b land 0xffff);

let rec do_times = (f, acc, n) => {
  if (n == 0) {
    acc
  } else {
    do_times(f, f(acc), n-1)
  }
};


let solve1 = (a, b) => {
  let ga = generator(2147483647, 16807);
  let gb = generator(2147483647, 48271);

  let f = ((a, b, times_matched)) => {
    let next_a = ga(a);
    let next_b = gb(b);
    if (matches(next_a, next_b)) {
      (next_a, next_b, times_matched + 1)
    } else {
      (next_a, next_b, times_matched)
    };
  };
  let (_, _, t) = do_times(f, (a, b, 0), 40000000);
  t
};

let rec xgenerator = (modulo, mult, x, n) => {
  let tmp = generator(modulo, mult, n);
  if (tmp % x == 0) {
    tmp
  } else {
    xgenerator(modulo, mult, x, tmp)
  }
};

let solve2 = (a, b) => {
  let ga = xgenerator(2147483647, 16807, 4);
  let gb = xgenerator(2147483647, 48271, 8);

  let f = ((a, b, times_matched)) => {
    let next_a = ga(a);
    let next_b = gb(b);
    if (matches(next_a, next_b)) {
      (next_a, next_b, times_matched + 1)
    } else {
      (next_a, next_b, times_matched)
    };
  };
  let (_, _, t) = do_times(f, (a, b, 0), 5000000);
  t
};


let () = {
  let starta = 512;
  let startb = 191;
  solve1(starta, startb) |> Int.to_string |> print_endline;
  solve2(starta, startb) |> Int.to_string |> print_endline;
};
