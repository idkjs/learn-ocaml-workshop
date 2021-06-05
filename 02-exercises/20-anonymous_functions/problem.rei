open! Base;

let map_option: ('a => 'b, option('a)) => option('b);
let apply_if_nonzero: (int => int, int) => int;
