open Base;

/* A moving piece is made up of 4 squares. */
type t = {
  top_left: Filled_square.t,
  top_right: Filled_square.t,
  bottom_left: Filled_square.t,
  bottom_right: Filled_square.t,
};

/* [create] creates a new random piece */
let create: unit => t;

/* [rotate_left] returns a new moving piece where the colors have been rotated
   left (counterclockwise). */
let rotate_left: t => t;

/* [rotate_right] returns a new moving piece where the colors have been rotated
   right (clockwise). */
let rotate_right: t => t;

/* given the column and row of the bottom left block of the pice, [coords]
   return a list of the coordinates of all four blocks in the piece */
let coords: (~bottom_left: Point.t) => list(Point.t);
