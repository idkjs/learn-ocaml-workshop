open Core;

module Angstrom = {
  include Angstrom;
  include Base.Monad.Make({
    type nonrec t('a) = t('a);
    let bind = (t, ~f) => t >>= f;
    let map = `Define_using_bind;
    let return = return;
  });
};

open Angstrom;
open Angstrom.Let_syntax;

module P = {
  let is_space =
    fun
    | ' ' => true
    | _ => false;

  let is_letter = Char.is_alpha;
  let is_digit = Char.is_digit;
  let is_letter_or_digit = Char.is_alphanum;

  let is_special =
    fun
    | '['
    | ']'
    | '\\'
    | '`'
    | '_'
    | '^'
    | '{'
    | '|'
    | '}' => true
    | _ => false;

  let is_nospcrlfcl =
    fun
    | '\000'
    | '\r'
    | '\n'
    | ' '
    | ':' => false
    | _ => true;
};

let letters = take_while1(P.is_letter);
let digits = take_while1(P.is_digit);

let (<||>) = (a, b) => take_while1(char => a(char) || b(char));

let space = string(" ");
let crlf = string("\r\n");

let rec at_most = (m, p) =>
  if (m == 0) {
    return([]);
  } else {
    lift2((x, xs) => [x, ...xs], p, at_most(m - 1, p)) <|> return([]);
  };

let between = (~lower, ~upper, p) =>
  lift2((xs, ys) => xs @ ys, count(lower, p), at_most(upper, p));

let user =
  take_while1(
    fun
    | '\000'
    | '\r'
    | '\n'
    | ' '
    | '@' => false
    | _ => true,
  )
  <?> "user";

let hostname = {
  let shortname =
    lift2(
      (hd, tl) => Char.to_string(hd) ++ tl,
      satisfy(P.is_letter_or_digit),
      peek_char
      >>= (
        fun
        | None => satisfy(P.is_letter_or_digit) >>| Char.to_string
        | Some(_) => P.is_letter_or_digit <||> (c => c == '-')
      ),
    )
    <?> "shortname";

  lift2(
    (s1, s2) =>
      switch (s2) {
      | [] => s1
      | s2 => s1 ++ "." ++ String.concat(~sep=".", s2)
      },
    shortname,
    many(string(".") *> shortname),
  )
  <?> "hostname";
};

let hostaddr = {
  let ip4addr =
    sep_by(char('.'), between(~lower=1, ~upper=3, digits) >>| String.concat)
    >>| List.intersperse(~sep=".")
    >>= (
      l =>
        switch (
          Option.try_with(() => Unix.Inet_addr.of_string(String.concat(l)))
        ) {
        | None =>
          fail(sprintf("Failed to parse inet_addr %s", String.concat(l)))
        | Some(inet_addr) => return(inet_addr)
        }
    );

  let ip6addr = fail("IPv6 support unimplemented");
  ip4addr <|> ip6addr <?> "hostaddr";
};

let host =
  Message.Prefix.(
    Host.hostname <$> hostname <|> (Host.inet_addr <$> hostaddr) <?> "host"
  );

let servername = hostname <?> "servername";

let prefix: t(Message.Prefix.t) = (
  {
    open Message.Prefix;
    let server_prefix = lift(s => Server(s), servername) <* space;
    let user_prefix = {
      let nickname =
        lift2(
          (++),
          P.is_letter <||> P.is_special,
          between(
            ~lower=0,
            ~upper=8,
            satisfy(
              fun
              | '-' => true
              | c => P.is_letter(c) || P.is_digit(c) || P.is_special(c),
            ),
          )
          >>| String.of_char_list,
        );

      let user =
        lift2(
          (user, host) => {User.user, host},
          option(None, Option.return <$> char('!') *> user),
          char('@') *> host,
        );

      lift2(
        (nickname, user) => User({nickname, user}),
        nickname,
        option(None, Option.return <$> user),
      )
      <* space;
    };

    string(":") *> (user_prefix <|> server_prefix) <?> "prefix";
  }:
    t(Message.Prefix.t)
);

let params = {
  let middle =
    lift2(
      (first, rest) => Char.to_string(first) ++ rest,
      satisfy(P.is_nospcrlfcl),
      take_while(c => c == ':' || P.is_nospcrlfcl(c)),
    );

  let trailing =
    take_while1(c => P.is_space(c) || c == ':' || P.is_nospcrlfcl(c))
    >>| List.return;

  let variant1 =
    lift2(
      (@),
      at_most(14, space *> middle),
      option([], space *> char(':') *> trailing),
    );

  let variant2 =
    lift2(
      (@),
      count(14, space *> middle),
      option([], space *> at_most(1, char(':')) *> trailing),
    );

  variant1 <|> variant2 <?> "params";
};

let command = {
  let command =
    lift2(
      (maybe_command, peek) =>
        switch (peek) {
        | None => maybe_command
        | Some(c) =>
          if (c == ' ') {
            maybe_command;
          } else {
            [];
          }
        },
      many1(letters)
      <|> (
        count(3, satisfy(P.is_digit)) >>| String.of_char_list >>| List.return
      ),
      peek_char,
    )
    <?> "command";

  String.concat <$> command >>| Message.Command.of_string;
};

let message = {
  let%bind maybe_prefix = option(None, Option.return <$> prefix);

  let%bind command = command;
  let%bind params = option([], params);
  crlf
  *> return({Message.prefix: maybe_prefix, command, params})
  <?> "message";
};
