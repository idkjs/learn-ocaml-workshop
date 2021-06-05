open Base;

/* We have a simple color scheme with two colors: Orange and White. */
type t =
  | Orange
  | White;

/* [compare] compares two colors, returning 0 if they are the same. */
let compare: (t, t) => int;

/* [random] returns a random color. */
let random: unit => t;
let equal: (t, t) => bool;
