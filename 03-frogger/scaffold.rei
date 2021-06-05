/** The grid system:

    0. The positions of all objects are snapped onto a coarse grid.
    1. The frog  is 1x1
    2. Every car is 1x1
    3. Every log is 1x1
*/;

/** The playable area of the screen will be referred to as the [board]. */

module Board: {
  /** Every row of the game board is one of these three kinds. */

  module Row: {
    type t =
      | Safe_strip
      | Road
      | River;
  };

  let num_cols: int;

  /** The first and last rows are guaranteed to be [Safe_strip]s. */

  let rows: list(Row.t);
};

/** This is a position in the grid system of the game, not in screen pixels. */

module Position: {
  [@deriving (fields, sexp)]
  type t = {
    x: int,
    y: int,
  };

  let create: (~x: int, ~y: int) => t;
};

/** All these images are 1x1. */

module Image: {
  [@deriving (sexp, variants)]
  type t =
    | Frog_up
    | Frog_down
    | Frog_left
    | Frog_right
    | Car1_left
    | Car1_right
    | Car2_left
    | Car2_right
    | Car3_left
    | Car3_right
    | Log1
    | Log2
    | Log3
    | Confetti
    | Skull_and_crossbones;
};

module Display_list: {
  /** The [Display_command] [(image, pos)] represents a command to draw [image]
      with its leftmost grid point at [pos].
  */

  module Display_command: {
    [@deriving sexp]
    type nonrec t = (Image.t, Position.t);
  };

  [@deriving sexp]
  type t = list(Display_command.t);
};

module Key: {
  type t =
    | Arrow_up
    | Arrow_down
    | Arrow_left
    | Arrow_right;
};

module Event: {
  type t =
    | Tick
    | Keypress(Key.t);
};

let console_log: string => unit;
