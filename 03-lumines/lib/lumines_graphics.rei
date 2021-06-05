/* This module handles the graphics for the game.  We have implemented this
   for you so you don't need to change anything here, but feel free to look around
   and once you have the game working, feel free to alter this to make things fancier */

/* Fails if called twice */
let init_exn: Game.t => unit;

/* redraw the board */
let draw: Game.t => unit;

/* return for keyboard input if it's available */
let read_key: unit => option(char);
