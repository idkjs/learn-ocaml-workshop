open Scaffold;

module Direction: {type t;};

module Frog: {
  type t;

  let facing: t => Direction.t;
  let position: t => Position.t;
};

module Non_frog_character: {
  module Kind: {
    type t =
      | Car
      | Log;
  };

  type t;

  let kind: t => Kind.t;
  let position: t => Position.t;

  /** In units of grid-points/tick. Positive values indicate rightward motion,
     negative values leftward motion. */

  let horizontal_speed: t => int;
};

module Game_state: {
  type t =
    | Playing
    | Won
    | Dead;
};

module World: {
  type t;

  let frog: t => Frog.t;
  let nfcs: t => list(Non_frog_character.t);
  let state: t => Game_state.t;
};

let create: unit => World.t;
let tick: World.t => World.t;
let handle_input: (World.t, Key.t) => World.t;
let handle_event: (World.t, Event.t) => World.t;
let draw: World.t => Display_list.t;
let finished: World.t => bool;
