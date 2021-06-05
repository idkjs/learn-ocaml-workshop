open! Base;

[@deriving sexp_of]
type t =
  | Left
  | Up
  | Right
  | Down;

/* TODO: Implement [next_position].

   Make sure to take a look at the signature of this function to understand what it does.
   Recall that the origin of the board is in the lower left hand corner. */
let next_position = (t, position) => position;
