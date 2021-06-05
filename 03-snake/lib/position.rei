open! Base;

/** A [t] represents a square on the playing area, identified by its row and
    column. */

[@deriving (compare, sexp)]
type t = {
  col: int,
  row: int,
};
