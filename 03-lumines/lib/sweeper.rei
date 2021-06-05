open Base;

/* We have implemented the sweeper for you.
   Feel free to look at this code for reference, but you shouldn't need to change
   anything within this module unless you are making a different variant of the game. */
type t;

let create: (Board.t, ~seconds_per_sweep: float) => t;

/* [cur_pos] returns the current position (column) of the sweeper. */
let cur_pos: t => int;

/* [seconds_per_step] returns how quickly the step function should be called to
   make it sweep the board in the time given by seconds per sweep. */
let seconds_per_step: t => float;

/* step advances the sweeper one square and potentially removes squares from the board */
let step: t => unit;
