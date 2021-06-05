open Core;

module Prefix: {
  module Host: {
    [@deriving (variants, sexp)]
    type t =
      | Hostname(string)
      | Inet_addr(Unix.Inet_addr.Blocking_sexp.t);
  };

  module User: {
    [@deriving sexp]
    type t = {
      host: Host.t,
      user: option(string),
    };
  };

  [@deriving sexp]
  type t =
    | Server(string)
    | User({
        nickname: string,
        user: option(User.t),
      });
};
module Command: {
  [@deriving sexp]
  type t = string;

  let of_string: string => t;
};

module Params: {
  [@deriving sexp]
  type t = list(string);
};

[@deriving (sexp, fields)]
type t = {
  prefix: option(Prefix.t),
  command: Command.t,
  params: Params.t,
};

let create:
  (~prefix: Prefix.t=?, ~command: Command.t, ~params: Params.t, unit) => t;

let to_string: t => string;
