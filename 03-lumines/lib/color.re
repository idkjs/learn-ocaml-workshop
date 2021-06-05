open Base;

type t =
  | Orange
  | White;

let compare = (t1, t2) =>
  switch (t1, t2) {
  | (White, White)
  | (Orange, Orange) => 0
  | (White, Orange) => 1
  | (Orange, White) => (-1)
  };

let random = () =>
  if (Random.int(2) == 0) {
    Orange;
  } else {
    White;
  };
let equal = (t1, t2) => compare(t1, t2) == 0;
