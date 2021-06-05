open Core;

type t;

let create: (~spin_every: Time.Span.t) => t;
let finish: t => unit;
let to_char: t => option(char);
let advance: t => unit;
