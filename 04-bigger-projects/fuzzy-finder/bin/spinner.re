open Core;
open Async;

module Spin_state = {
  type t =
    | Vert_bar
    | Lower_left_to_top_right
    | Dash
    | Lower_right_to_top_left;

  let advance =
    fun
    | Vert_bar => Lower_left_to_top_right
    | Lower_left_to_top_right => Dash
    | Dash => Lower_right_to_top_left
    | Lower_right_to_top_left => Vert_bar;

  let to_char =
    fun
    | Vert_bar => '|'
    | Lower_left_to_top_right => '/'
    | Dash => '-'
    | Lower_right_to_top_left => '\\';
};

type t = ref(option(Spin_state.t));

let finish = t => t := None;

let to_char = t => Option.map(~f=Spin_state.to_char, t^);

let advance = t => t := Option.map(~f=Spin_state.advance, t^);

let create = (~spin_every) => {
  let t = ref(Some(Spin_state.Vert_bar));
  Clock.every(spin_every, () => advance(t));
  t;
};
