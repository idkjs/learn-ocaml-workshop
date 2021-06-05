open Base;

module Position = {
  [@deriving (fields, sexp)]
  type t = {
    x: int,
    y: int,
  };

  let create = Fields.create;
};

module Image = {
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

module Display_list = {
  module Display_command = {
    [@deriving sexp]
    type nonrec t = (Image.t, Position.t);
  };

  [@deriving sexp]
  type t = list(Display_command.t);
};

module Key = {
  type t =
    | Arrow_up
    | Arrow_down
    | Arrow_left
    | Arrow_right;
};

module Event = {
  type t =
    | Tick
    | Keypress(Key.t);
};

module Board = {
  /** Every row of the game board is one of these three kinds. */
  module Row = {
    type t =
      | Safe_strip
      | Road
      | River;
  };

  let num_cols = 10;

  /** The first and last rows are guaranteed to be [Safe_strip]s. */

  let rows =
    Row.(
      [
        Safe_strip,
        River,
        River,
        River,
        River,
        River,
        Safe_strip,
        Road,
        Road,
        Road,
        Road,
        Safe_strip,
      ]
      |> List.rev
    );
};

let console_log = s => Js_of_ocaml.Firebug.console##log(s);
