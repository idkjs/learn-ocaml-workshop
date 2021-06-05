open Base;

/* The board is a 2-dimensional array of [filled_square option]s. If the square
   is empty, we represent it with [None].  If it is filled, we represent it with
   [Some Filled_Square].  We have provided getter and setter functions to get
   and set values of the array */
type t = {
  board: array(array(option(Filled_square.t))),
  height: int,
  width: int,
};

/* [create ~height ~width] creates a board of given height and width */
let create: (~height: int, ~width: int) => t;

/* [get] returns the value of the board at a given row and col */
let get: (t, Point.t) => option(Filled_square.t);

/* [set] sets the value of the board at a given row and col */
let set: (t, Point.t, option(Filled_square.t)) => unit;

/* [remove_squares] will be called by the sweeper. It should delete any squares
   marked as [Swept] from the board and leave the board in a valid state */
let remove_squares: t => unit;

/* [add_piece_and_apply_gravity] takes a piece and the column number of the left
   side of the piece and inserts it into the board. Returns: true if it was able
   to add the piece to the board false otherwise */
let add_piece_and_apply_gravity:
  (t, ~moving_piece: Moving_piece.t, ~col: int) => bool;

/* [is_empty] takes a row and a col and returns: true if that square is empty
   false if that square is filled */
let is_empty: (t, Point.t) => bool;
