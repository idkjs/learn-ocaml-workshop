open! Base;

/** This module handles the graphics for the game. We have implemented this for
    you so you don't need to change anything here, but feel free to look around
    and once you have the game, feel free to alter this file to make things
    fancier! */;

/** [init_exn] fails if called twice. */

let init_exn: unit => Game.t;

/** [render] renders the entire playing area along with snakes and apples. */

let render: Game.t => unit;

/** [read_key] returns a keyboard input, if it's available. */

let read_key: unit => option(char);
