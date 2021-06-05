open Core;
open Async;

type t = {
  /* TODO: Fill me in */
  todo: unit,
};

let _compilation_fix_you_probably_want_to_use_this_module: module Unit =
  (module Spinner);

let run = (user_input, tty_text, stdin) => {
  let t = {todo: ()};
  let () = t |> (ignore: t => unit);
  let () = tty_text |> (ignore: Tty_text.t => unit);
  let () = stdin |> (ignore: Reader.t => unit);
  let () =
    user_input |> (ignore: Pipe.Reader.t(Tty_text.User_input.t) => unit);
  Render.every(
    ~how_often_to_render=Time.Span.of_sec(0.1),
    ~render=
      () =>
        /* TODO: Determine when rendering actually needs to occur, and call render. */
        Deferred.unit,
    () =>
      /* TODO : Process events from new lines on [stdin], as well as [user_input]. */
      return(`Finished(None)),
  );
};

let () =
  Command.run @@
  Command.Let_syntax.(
    Command.async(
      ~summary="Custom fzf",
      {
        let%map_open () = return();
        () => {
          open Deferred.Let_syntax;
          /* TODO: Determine if [stdin] is a tty (see [Unix.isatty],) and if it is,
             do not process anything from it. If this guard is not in place,
             when no stdin is provided to fzf, alternating keypresses will seem to
             disappear. */
          let stdin = force(Reader.stdin);
          switch%bind (
            Tty_text.with_rendering(((input, tty_text)) =>
              run(input, tty_text, stdin)
            )
          ) {
          | None => Deferred.unit
          | Some(output) =>
            let stdout = force(Writer.stdout);
            Writer.write_line(stdout, output);
            Writer.flushed(stdout);
          };
        };
      },
    )
  );
