open Base;
open Scaffold;

module Direction = {
  type t =
    | Up
    | Down
    | Left
    | Right;
};

module Frog = {
  [@deriving fields]
  type t = {
    position: Position.t,
    facing: Direction.t,
  };

  let create = Fields.create;
};

module Non_frog_character = {
  module Kind = {
    type t =
      | Car
      | Log;
  };

  [@deriving fields]
  type t = {
    horizontal_speed: int,
    position: Position.t,
    kind: Kind.t,
    image: Image.t,
  };

  let create = Fields.create;
};

module Game_state = {
  type t =
    | Playing
    | Won
    | Dead;
};

module World = {
  [@deriving fields]
  type t = {
    state: Game_state.t,
    frog: Frog.t,
    nfcs: list(Non_frog_character.t),
  };

  let create = Fields.create;
};

let create_frog = () => {
  let position = Position.create(~x=Scaffold.Board.num_cols / 2, ~y=0);

  Frog.create(~position, ~facing=Direction.Up);
};

let create_nfcs = () => {
  let max_speed = 1;
  List.mapi(
    Scaffold.Board.rows,
    ~f=(idx, row) => {
      let make_nfc = (kind, col, direction_sign) => {
        let horizontal_speed = direction_sign * (1 + Random.int(max_speed));

        let position = Position.create(~x=col, ~y=idx);
        let image =
          switch ((kind: Non_frog_character.Kind.t)) {
          | Car =>
            let dir = (left, right) =>
              if (horizontal_speed < 0) {
                left;
              } else {
                right;
              };
            switch (Random.int(3)) {
            | 0 => dir(Image.car1_left, Image.car1_right)
            | 1 => dir(Image.car2_left, Image.car2_right)
            | 2 => dir(Image.car3_left, Image.car3_right)
            | _ => assert(false)
            };
          | Log =>
            switch (Random.int(3)) {
            | 0 => Image.log1
            | 1 => Image.log2
            | 2 => Image.log3
            | _ => assert(false)
            }
          };

        Non_frog_character.create(
          ~kind,
          ~horizontal_speed,
          ~position,
          ~image,
        );
      };

      let make_one_row = kind => {
        let num_nfcs_per_row = 3;
        let max_gap = 3;
        let start_pos = Random.int(Board.num_cols);
        let gap_to_leave_between_nfcs =
          switch ((kind: Non_frog_character.Kind.t)) {
          | Car => 1 + Random.int(max_gap)
          | Log => 0
          };

        let sign = 2 * (idx % 2) - 1; /* Alternating directions */
        List.init(num_nfcs_per_row, ~f=idx =>
          make_nfc(
            kind,
            (start_pos + idx * (gap_to_leave_between_nfcs + 1))
            % Board.num_cols,
            sign,
          )
        );
      };

      switch (row) {
      | Safe_strip => []
      | Road => make_one_row(Non_frog_character.Kind.Car)
      | River => make_one_row(Non_frog_character.Kind.Log)
      };
    },
  )
  |> List.concat;
};

let create = () =>
  World.create(
    ~state=Game_state.Playing,
    ~frog=create_frog(),
    ~nfcs=create_nfcs(),
  );

let rec detect_collision = (frog_pos: Position.t, nfcs) => {
  let is_colliding = (nfc: Non_frog_character.t) =>
    if (Int.(!=)(frog_pos.y, nfc.position.y)) {
      false;
    } else {
      let width =
        switch (nfc.kind) {
        | Car => 1
        | Log => 1
        };

      nfc.position.x <= frog_pos.x && frog_pos.x < nfc.position.x + width;
    };

  switch (nfcs) {
  | [] => None
  | [nfc, ...rest] =>
    if (is_colliding(nfc)) {
      Some(nfc);
    } else {
      detect_collision(frog_pos, rest);
    }
  };
};

let pos_is_in_river = (pos: Position.t) =>
  switch (List.nth_exn(Scaffold.Board.rows, pos.y)) {
  | Safe_strip
  | Road => false
  | River => true
  };

let should_die = (frog_pos, collision_result) => {
  let frog_is_in_river = pos_is_in_river(frog_pos);
  switch ((collision_result: option(Non_frog_character.t))) {
  | Some({kind: Car, _}) => true
  | Some({kind: Log, _}) => false
  | None => frog_is_in_river
  };
};

let should_win = (frog_pos: Position.t) =>
  Int.(==)(frog_pos.y, List.length(Scaffold.Board.rows) - 1);

let compute_new_game_state = (frog_pos, collision_result) =>
  if (should_die(frog_pos, collision_result)) {
    Game_state.Dead;
  } else if (should_win(frog_pos)) {
    Won;
  } else {
    Playing;
  };

let tick = (world: World.t) =>
  switch (world.state) {
  | Won
  | Dead => world
  | Playing =>
    let new_nfcs =
      List.map(
        world.nfcs,
        ~f=nfc => {
          let new_position =
            Position.create(
              ~x=
                (nfc.position.x + nfc.horizontal_speed)
                % Scaffold.Board.num_cols,
              ~y=nfc.position.y,
            );

          {...nfc, position: new_position};
        },
      );

    let collision_result_before =
      detect_collision(world.frog.position, world.nfcs);
    let new_frog = {
      let new_frog_position = {
        let dx =
          switch (collision_result_before) {
          | Some({kind: Log, horizontal_speed, _}) => horizontal_speed
          | _ => 0
          };

        Position.create(
          ~x=world.frog.position.x + dx,
          ~y=world.frog.position.y,
        );
      };

      {...world.frog, position: new_frog_position};
    };

    let collision_result_after =
      detect_collision(new_frog.position, new_nfcs);
    let new_game_state =
      compute_new_game_state(new_frog.position, collision_result_after);
    World.create(~state=new_game_state, ~frog=new_frog, ~nfcs=new_nfcs);
  };

let clamp = (~min, ~max, x) =>
  if (x < min) {
    min;
  } else if (x > max) {
    max;
  } else {
    x;
  };

let handle_input = (world: World.t, key) => {
  let num_rows = List.length(Scaffold.Board.rows);
  let num_cols = Scaffold.Board.num_cols;
  switch (world.state) {
  | Won
  | Dead => world
  | Playing =>
    let new_frog = {
      let (new_pos, new_dir) = {
        let old_pos = world.frog.position;
        switch (key) {
        | Key.Arrow_up => (
            {...old_pos, y: clamp(~min=0, ~max=num_rows - 1, old_pos.y + 1)},
            Direction.Up,
          )
        | Key.Arrow_down => (
            {...old_pos, y: clamp(~min=0, ~max=num_rows - 1, old_pos.y - 1)},
            Direction.Down,
          )
        | Key.Arrow_left => (
            {...old_pos, x: clamp(~min=0, ~max=num_cols - 1, old_pos.x - 1)},
            Direction.Left,
          )
        | Key.Arrow_right => (
            {...old_pos, x: clamp(~min=0, ~max=num_cols - 1, old_pos.x + 1)},
            Direction.Right,
          )
        };
      };

      Frog.create(~position=new_pos, ~facing=new_dir);
    };

    let new_game_state = {
      let collision_result = detect_collision(new_frog.position, world.nfcs);
      compute_new_game_state(new_frog.position, collision_result);
    };

    World.create(~state=new_game_state, ~frog=new_frog, ~nfcs=world.nfcs);
  };
};

let draw = (world: World.t) => {
  let draw_frog_command = {
    let frog_image =
      switch (world.state) {
      | Dead => Image.skull_and_crossbones
      | Won => Image.confetti
      | Playing =>
        switch (world.frog.facing) {
        | Up => Image.frog_up
        | Down => Image.frog_down
        | Left => Image.frog_left
        | Right => Image.frog_right
        }
      };

    (frog_image, world.frog.position);
  };

  let draw_nfc = (nfc: Non_frog_character.t) => (nfc.image, nfc.position);
  List.map(world.nfcs, ~f=draw_nfc) @ [draw_frog_command];
};

let handle_event = (world, event) =>
  switch ((event: Event.t)) {
  | Tick => tick(world)
  | Keypress(k) => handle_input(world, k)
  };

let finished = world =>
  switch (World.state(world)) {
  | Playing => false
  | Won
  | Dead => true
  };
