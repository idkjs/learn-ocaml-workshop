open Base;
open Lib;

/* This is the core logic that actually runs the game.  We have implemented all
   of this for you, but feel free to read this as a reference.  */

let every = (seconds, ~f, ~stop) => {
  open Async;
  open Core;
  let rec loop = () =>
    if (stop^) {
      return();
    } else {
      Clock.after(Time.Span.of_sec(seconds))
      >>= (
        () => {
          f();
          loop();
        }
      );
    };

  don't_wait_for(loop());
};

/* run_sweeper sets up a loop that steps the sweeper forward
   and redraws the game */
let run_sweeper = (game: Game.t) =>
  every(
    ~stop=game.game_over,
    Sweeper.seconds_per_step(game.sweeper),
    ~f=() => {
      Sweeper.step(game.sweeper);
      Lumines_graphics.draw(game);
    },
  );

let handle_keys = (game: Game.t) =>
  every(~stop=game.game_over, 0.01, ~f=() =>
    switch (Lumines_graphics.read_key()) {
    | None => ()
    | Some(key) =>
      let update =
        switch (key) {
        | 'a' =>
          Game.move_left(game);
          true;
        | 'd' =>
          Game.move_right(game);
          true;
        | 'w' =>
          Game.rotate_left(game);
          true;
        | 's' =>
          Game.rotate_right(game);
          true;
        | ' ' =>
          Game.drop(game);
          true;
        | _ => false
        };

      if (update && ! game.game_over^) {
        Lumines_graphics.draw(game);
      };
    }
  );

let handle_clock_tick = (game: Game.t) =>
  every(
    ~stop=game.game_over,
    1.,
    ~f=() => {
      Game.tick(game);
      Lumines_graphics.draw(game);
    },
  );

/* this is the core loop that powers the game */
let run = () => {
  let game = Game.create(~height=14, ~width=16, ~seconds_per_sweep=3.);
  Lumines_graphics.init_exn(game);
  handle_keys(game);
  run_sweeper(game);
  handle_clock_tick(game);
};

let () = {
  run();
  Core_kernel.never_returns(Async.Scheduler.go());
};
