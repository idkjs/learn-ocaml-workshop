open Scaffold;

module World: {type t;};

let create: unit => World.t;
let tick: World.t => World.t;
let handle_input: (World.t, Key.t) => World.t;
let handle_event: (World.t, Event.t) => World.t;
let draw: World.t => Display_list.t;
let finished: World.t => bool;
