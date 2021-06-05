open! Base;

[@deriving sexp_of]
type t =
  | In_progress
  | Game_over(string)
  | Win;

let to_string = t =>
  switch (t) {
  | In_progress => ""
  | Game_over(x) => "Game over: " ++ x
  | Win => "WIN!"
  };
