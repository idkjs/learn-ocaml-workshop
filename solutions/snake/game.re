open! Base;

[@deriving sexp_of]
type t = {
  mutable snake: Snake.t,
  mutable apple: Apple.t,
  mutable game_state: Game_state.t,
  height: int,
  width: int,
  amount_to_grow: int,
};

/* TODO: Implement [in_bounds]. */
let in_bounds = (t, {Position.row, col}) =>
  col >= 0 && col < t.width && row >= 0 && row < t.height;

/* TODO: Implement [create].

   Make sure that the game returned by [create] is in a valid state. In particular, we
   should fail with the message "unable to create initial apple" if [Apple.create] is
   unsuccessful, and "unable to create initial snake" if the initial snake is invalid
   (i.e. goes off the board). */
let create = (~height, ~width, ~initial_snake_length, ~amount_to_grow) => {
  let snake = Snake.create(~length=initial_snake_length);
  let apple =
    Apple.create(~height, ~width, ~invalid_locations=Snake.locations(snake));
  switch (apple) {
  | None => failwith("unable to create initial apple")
  | Some(apple) =>
    let t = {
      snake,
      apple,
      game_state: In_progress,
      height,
      width,
      amount_to_grow,
    };
    if (List.exists(Snake.locations(snake), ~f=pos => !in_bounds(t, pos))) {
      failwith("unable to create initial snake");
    } else {
      t;
    };
  };
};

let snake = t => t.snake;
let apple = t => t.apple;
let game_state = t => t.game_state;

/* TODO: Implement [set_direction]. */
let set_direction = (t, direction) =>
  t.snake = Snake.set_direction(t.snake, direction);

/* TODO: Implement [step].

   [step] should:
   - move the snake forward one square
   - check for collisions (end the game with "Wall collision" or "Self collision")
   - if necessary:
     -- consume apple
     -- if apple cannot be regenerated, win game; otherwise, grow the snake */
let maybe_consume_apple = (t, head) =>
  if (![%compare.equal: Position.t](head, Apple.location(t.apple))) {
    ();
  } else {
    let snake = Snake.grow_over_next_steps(t.snake, t.amount_to_grow);
    let apple =
      Apple.create(
        ~height=t.height,
        ~width=t.width,
        ~invalid_locations=Snake.locations(snake),
      );

    switch (apple) {
    | None => t.game_state = Win
    | Some(apple) =>
      t.snake = snake;
      t.apple = apple;
    };
  };

let step = t =>
  switch (Snake.step(t.snake)) {
  | None => t.game_state = Game_over("Self collision")
  | Some(snake) =>
    t.snake = snake;
    let head = Snake.head_location(snake);
    if (!in_bounds(t, head)) {
      t.game_state = Game_over("Wall collision");
    } else {
      maybe_consume_apple(t, head);
    };
  };

module For_testing = {
  let create_apple_force_location_exn = (~height, ~width, ~location) => {
    let invalid_locations =
      List.init(height, ~f=row =>
        List.init(width, ~f=col => {Position.row, col})
      )
      |> List.concat
      |> List.filter(~f=pos => ![%compare.equal: Position.t](location, pos));

    switch (Apple.create(~height, ~width, ~invalid_locations)) {
    | None =>
      failwith("[Apple.create] returned [None] when [Some _] was expected!")
    | Some(apple) => apple
    };
  };

  let create_apple_and_update_game_exn = (t, ~apple_location) => {
    let apple =
      create_apple_force_location_exn(
        ~height=t.height,
        ~width=t.width,
        ~location=apple_location,
      );

    t.apple = apple;
  };

  let create_game_with_apple_exn =
      (
        ~height,
        ~width,
        ~initial_snake_length,
        ~amount_to_grow,
        ~apple_location,
      ) => {
    let t = create(~height, ~width, ~initial_snake_length, ~amount_to_grow);
    create_apple_and_update_game_exn(t, ~apple_location);
    t;
  };
};
