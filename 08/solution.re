open Core;

type op = {
  reg: string,
  inc: int,
  cond_reg: string,
  cond_op: (int) => bool,
};

let sum = List.fold(~f=(+), ~init=0);

let parse_line = (line) => {
  let [reg, act, val_str, _, cond_reg, cmp, rhs_str] = String.split(~on=' ', line);
  let valu = Int.of_string(val_str);
  let inc_val = switch(act) {
    | "inc" => valu
    | "dec" => -valu
    | _ => assert(false)
  };
  let rhs = Int.of_string(rhs_str);
  let op = switch(cmp) {
    | "==" => (x) => x == rhs 
    | "!=" => (x) => x != rhs
    | ">" => (x) => x > rhs
    | "<" => (x) => x < rhs
    | ">=" => (x) => x >= rhs
    | "<=" => (x) => x <= rhs
    | _ => assert(false)
  };
  {
    reg: reg,
    inc: inc_val,
    cond_reg: cond_reg,
    cond_op: op
  };
};

let eval = (state, op) => {
  let get_val = (key) => Option.value(~default=0, List.Assoc.find(state, key, ~equal=(==)));
  let set_val = List.Assoc.add(state, ~equal=(==));
  let x = get_val(op.cond_reg);
  if (op.cond_op(x)) {
    set_val(op.reg, get_val(op.reg) + op.inc)
  } else {
    state
  };
};

let max_reg = (state) => List.map(state, ((k,v)) => v) |> List.fold(~f=max, ~init=Int.min_value);

let solve1 = (ops) => {
  let end_state = ops |> List.fold(~f=eval, ~init=[]);
  max_reg(end_state)
};

let solve2 = (ops) => {
  let f = ((state, acc_max), op) => {
    let new_state = eval(state, op);
    let new_max = max(acc_max, max_reg(new_state));
    (new_state, new_max)
  };
  let (_, acc_max) = ops |> List.fold(~f=f, ~init=([], 0));
  acc_max
};

let () = {
  let inp = In_channel.stdin |> In_channel.input_lines |> List.map(~f=parse_line);
  solve1(inp) |> Int.to_string |> print_endline;
  solve2(inp) |> Int.to_string |> print_endline;
};
