open! Base;

/** A [t] represents the current state of the game. */ /* The string is the reason the game ended. */

[@deriving sexp_of]
type t =
  | In_progress
  | Game_over(string)
  | Win;

/** [to_string] pretty-prints the current game state into a string. */

let to_string: t => string;
