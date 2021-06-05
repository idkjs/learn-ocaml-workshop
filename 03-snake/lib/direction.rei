open! Base;

[@deriving sexp_of]
type t =
  | Left
  | Up
  | Right
  | Down;

/** [next_position] takes a direction and a starting position and returns the
    next position after taking one step in the specified direction. */

let next_position: (t, Position.t) => Position.t;
