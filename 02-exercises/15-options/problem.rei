open! Base;

type option('a) =
  | None
  | Some('a);

let safe_divide: (~dividend: int, ~divisor: int) => option(int);
let option_concatenate: (option(string), option(string)) => option(string);
let concatenate: (~separator: string=?, string, string) => string;
