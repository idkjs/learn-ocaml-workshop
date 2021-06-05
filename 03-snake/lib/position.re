open! Base;

[@deriving (compare, sexp)]
type t = {
  col: int,
  row: int,
};
