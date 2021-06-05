open! Base;

type color;
type stoplight;

let set_color: (stoplight, color) => unit;
let advance_color: stoplight => unit;
