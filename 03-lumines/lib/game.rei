open Base;

/* This module holds the entire game state. */
type t = {
  board: Board.t,
  height: int,
  width: int,
  mutable moving_piece: Moving_piece.t, /* We represent the location of the moving piece by the bottom left corner
       of the piece. Note that the origin of [board] is the lower left
       corner. */
  mutable moving_piece_col: int,
  mutable moving_piece_row: int,
  game_over: ref(bool),
  sweeper: Sweeper.t,
};

let create: (~height: int, ~width: int, ~seconds_per_sweep: float) => t;

/* [new_moving_piece] puts a random new block at the top of the board */
let new_moving_piece: t => unit;

/* Functions to move the piece on the board */
let move_left: t => unit;
let move_right: t => unit;
let rotate_left: t => unit;
let rotate_right: t => unit;
let drop: t => unit;

/* [tick] handles everything that needs to happen when the clock ticks once */
let tick: t => unit;
