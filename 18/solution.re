/**
  Implementing a virtual machine with asynchronous message passing
  */
open Core;

type value =
  | Val(int)
  | Reg(char);

type op = 
  | Snd(value)
  | Set(char, value)
  | Add(char, value)
  | Mul(char, value)
  | Mod(char, value)
  | Rcv(char)
  | Jgz(value, value);

let parse_val = (params, n) => {
  let str = List.nth_exn(params, n);
  if (String.get(str, 0) >= 'a' && String.get(str, 0) <= 'z') {
    Reg(String.get(str, 0))
  } else {
    Val(Int.of_string(str))
  };
};

let parse_reg = (params, n) => {
  let str = List.nth_exn(params, n);
  String.get(str, 0);
};

let parse_line = (line) => {
  let [opstr, ...params] = String.split(~on=' ', line);
  let valu = parse_val(params);
  let reg = parse_reg(params);
  switch(opstr) {
    | "snd" => Snd(valu(0));
    | "set" => Set(reg(0), valu(1));
    | "add" => Add(reg(0), valu(1));
    | "mul" => Mul(reg(0), valu(1));
    | "mod" => Mod(reg(0), valu(1));
    | "rcv" => Rcv(reg(0));
    | "jgz" => Jgz(valu(0), valu(1));
    | _ => assert(false);
  };
};

let rec eval = (code, state) => {
  let (regs, pos, sound) = state;
  if (pos == -1) {
    sound
  } else {
    let op = List.nth_exn(code, pos);
    let get_val = (v) => {
      switch(v) {
        | Reg(r) => Option.value(~default=0, List.Assoc.find(regs, r, ~equal=(==)));
        | Val(n) => n;
      };
    };
    let set_val = List.Assoc.add(regs, ~equal=(==));
    let new_state = switch(op) {
      | Snd(v)    => (regs, pos+1, get_val(v))
      | Set(r, v) => (set_val(r, get_val(v)), pos+1, sound)
      | Add(r, v) => (set_val(r, get_val(Reg(r)) + get_val(v)), pos+1, sound)
      | Mul(r, v) => (set_val(r, get_val(Reg(r)) * get_val(v)), pos+1, sound)
      | Mod(r, v) => (set_val(r, get_val(Reg(r)) % get_val(v)), pos+1, sound)
      | Rcv(r) => if (get_val(Reg(r)) != 0) { (regs, -1, sound) } else { (regs, pos+1, sound) }
      | Jgz(v1, v2) => if (get_val(v1) > 0) {
        (regs, pos + get_val(v2), sound)
      } else {
        (regs, pos + 1, sound)
      }
    };
    eval(code, new_state);
  }
};

let solve1 = (ops) => {
  eval(ops, ([], 0, 0));
};

let rec eval_proc = (code, state) => {
  let (regs, pos, in_q) = state;
  let op = List.nth_exn(code, pos);
  let get_val = (v) => {
    switch(v) {
      | Reg(r) => Option.value(~default=0, List.Assoc.find(regs, r, ~equal=(==)));
      | Val(n) => n;
    };
  };
  let set_val = List.Assoc.add(regs, ~equal=(==));
  let new_state = switch(op) {
    | Snd(v)    => (regs, pos+1, in_q, [get_val(v)], false)
    | Set(r, v) => (set_val(r, get_val(v)), pos+1, in_q, [], false)
    | Add(r, v) => (set_val(r, get_val(Reg(r)) + get_val(v)), pos+1, in_q, [], false)
    | Mul(r, v) => (set_val(r, get_val(Reg(r)) * get_val(v)), pos+1, in_q, [], false)
    | Mod(r, v) => (set_val(r, get_val(Reg(r)) % get_val(v)), pos+1, in_q, [], false)
    | Rcv(r) => switch(in_q) {
      | [] => (regs, pos, in_q, [], true)
      | [x, ...xs] => (set_val(r, x), pos+1, xs, [], false)
    }
    | Jgz(v1, v2) => if (get_val(v1) > 0) {
      (regs, pos + get_val(v2), in_q, [], false)
    } else {
      (regs, pos + 1, in_q, [], false)
    }
  };
  new_state
};

let rec eval2 = (code, state1, state2, cnt) => {
  let (regs1, pos1, q1, wait1) = state1;
  let (regs2, pos2, q2, wait2) = state2;
  switch(q1, wait1, q2, wait2) {
    | ([], true, [], true) => cnt  /* deadlock */
    | ([], true, _, _) => {
      let (regs2, pos2, q2, out2, wait2) = eval_proc(code, (regs2, pos2, q2));
      let cnt = if (List.is_empty(out2)) { cnt } else { cnt + 1 };
      let q1 = List.append(q1, out2);
      eval2(code, (regs1, pos1, q1, wait1), (regs2, pos2, q2, wait2), cnt);
    }
    | (_, _, _, _) => {
      let (regs1, pos1, q1, out1, wait1) = eval_proc(code, (regs1, pos1, q1));
      let q2 = List.append(q2, out1);
      eval2(code, (regs1, pos1, q1, wait1), (regs2, pos2, q2, wait2), cnt);
    }
  }
};

let solve2 = (ops) => {
  eval2(ops, ([('p', 0)], 0, [], false), ([('p', 1)], 0, [], false), 0);
};

let () = {
  let inp = In_channel.stdin |> In_channel.input_lines |> List.map(~f=parse_line);
  solve1(inp) |> Int.to_string |> print_endline;
  solve2(inp) |> Int.to_string |> print_endline;
};
