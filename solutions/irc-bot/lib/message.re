open Core;

module Prefix = {
  module Host = {
    [@deriving (variants, sexp)]
    type t =
      | Hostname(string)
      | Inet_addr(Unix.Inet_addr.Blocking_sexp.t);

    let to_string =
      fun
      | Hostname(s) => s
      | Inet_addr(addr) => Unix.Inet_addr.to_string(addr);
  };

  module User = {
    [@deriving sexp]
    type t = {
      host: Host.t,
      user: option(string),
    };

    let to_string = ({host, user}) => {
      let user =
        switch (user) {
        | Some(u) => "!" ++ u
        | None => ""
        };

      user ++ "@" ++ Host.to_string(host);
    };
  };

  [@deriving sexp]
  type t =
    | Server(string)
    | User({
        nickname: string,
        user: option(User.t),
      });

  let to_string = t =>
    ":"
    ++ (
      switch (t) {
      | Server(s) => s
      | User({nickname, user: maybe_user}) =>
        switch (maybe_user) {
        | None => nickname
        | Some(user) => nickname ++ User.to_string(user)
        }
      }
    );
};

module Command = {
  [@deriving sexp]
  type t = string;

  /* TODO: Actually validate the commands here. */
  let of_string = s => s;
};

module Params = {
  [@deriving sexp]
  type t = list(string);

  let to_string = t => {
    let rec loop = (acc, rest) =>
      switch (rest) {
      | [] => acc
      | [last] => acc ++ " :" ++ last
      | [elem, ...rest] => loop(acc ++ " " ++ elem, rest)
      };

    loop("", t);
  };
};

[@deriving (sexp, fields)]
type t = {
  prefix: option(Prefix.t),
  command: Command.t,
  params: Params.t,
};

let create = (~prefix=?, ~command, ~params, ()) =>
  Fields.create(~prefix, ~command, ~params);

let to_string = t =>
  (
    switch (t.prefix) {
    | Some(prefix) => Prefix.to_string(prefix) ++ " "
    | None => ""
    }
  )
  |> (prefix => prefix ++ t.command ++ Params.to_string(t.params));
