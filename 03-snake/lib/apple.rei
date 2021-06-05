open! Base;

[@deriving sexp_of]
type t;

/** [create] takes in the [height] and [width] of the area in which an apple can be
    generated, as well as a list of [Position.t]s representing the locations on the board
    that the apple cannot be placed, and creates an [Apple.t].

    [create] returns [None] if there are no valid positions for the apple. */

let create:
  (~height: int, ~width: int, ~invalid_locations: list(Position.t)) =>
  option(t);

/** [location] returns the location of the apple on the board. */

let location: t => Position.t;
