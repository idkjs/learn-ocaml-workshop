open Base;

module Sweeper_state = {
  type t =
    | Unmarked
    | To_sweep
    | Swept;

  let equal = (t1, t2) =>
    switch (t1, t2) {
    | (Unmarked, Unmarked)
    | (To_sweep, To_sweep)
    | (Swept, Swept) => true
    | _ => false
    };
};

type t = {
  color: Color.t,
  mutable sweeper_state: Sweeper_state.t,
};

let create = color => {color, sweeper_state: Unmarked};
let unmark = t => t.sweeper_state = Unmarked;
let to_sweep = t => t.sweeper_state = To_sweep;

let sweep = t =>
  switch (t.sweeper_state) {
  | To_sweep =>
    t.sweeper_state = Swept;
    true;
  | Unmarked
  | Swept => false
  };

let equal = (t1, t2) =>
  Color.equal(t1.color, t2.color)
  && Sweeper_state.equal(t1.sweeper_state, t2.sweeper_state);
