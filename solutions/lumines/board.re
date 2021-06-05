open Base;

type t = {
  board: array(array(option(Filled_square.t))),
  height: int,
  width: int,
};

let create = (~height, ~width) => {
  board: Array.make_matrix(~dimx=width, ~dimy=height, None),
  height,
  width,
};

let get = (t, {Point.col, row}) => t.board[col][row];
let set = (t, {Point.col, row}, value) => t.board[col][row] = value;

let mark_squares_that_are_sweepable = t => {
  /* TODO: at the end of this function, all filled_squares that are part of
     completed squares (i.e. four tiles in a square arrangement that are all of
     the same colors) should be in sweeper state [To_sweep], and all other
     squares should be [Unmarked].

     Note that, for example, a 2x3 rectangle of all the same color should also
     be marked by these criteria. */
  List.iter(List.range(0, t.width), ~f=col =>
    List.iter(List.range(0, t.height), ~f=row =>
      switch (get(t, {col, row})) {
      | None => ()
      | Some(filled_square) => Filled_square.unmark(filled_square)
      }
    )
  );
  List.iter(List.range(0, t.width - 1), ~f=col =>
    List.iter(
      List.range(0, t.height - 1),
      ~f=row => {
        let coords = Moving_piece.coords(~bottom_left={Point.row, col});
        let colors =
          List.map(coords, ~f=get(t))
          |> List.fold(~init=[], ~f=(acc, t) =>
               switch (t) {
               | None => [None, ...acc]
               | Some(t) =>
                 let color = Some(t.Filled_square.color);
                 switch (acc) {
                 | [] => [color]
                 | [c] =>
                   if (Option.equal(Color.equal, color, c)) {
                     acc;
                   } else {
                     [color, ...acc];
                   }
                 | _ => acc
                 };
               }
             );

        switch (colors) {
        | [Some(_)] =>
          List.iter(coords, ~f=point =>
            switch (get(t, point)) {
            | None => ()
            | Some(filled_square) => Filled_square.to_sweep(filled_square)
            }
          )
        | _ => ()
        };
      },
    )
  );
};

let remove_squares = t => {
  /* TODO: remove any squares marked as [Swept] from the board.  Gravity should
     be applied appropriately. This is the function that is called by the
     [Sweeper.t] to clear squares from the board.

     At the end of this function, we should call
     [mark_squares_that_are_sweepable] so that we ensure that we leave the board
     in a valid state.  */
  let squares_to_remove =
    List.fold(List.range(0, t.width), ~init=[], ~f=(acc, col) =>
      List.fold(
        List.range(0, t.height),
        ~init=acc,
        ~f=(acc, row) => {
          let point = {Point.col, row};
          switch (get(t, point)) {
          | Some(filled_square) =>
            if (Filled_square.Sweeper_state.equal(
                  filled_square.sweeper_state,
                  Swept,
                )) {
              [point, ...acc];
            } else {
              acc;
            }
          | _ => acc
          };
        },
      )
    )
    |> List.sort(~compare=(p1, p2) => Point.compare_by_row(p2, p1));

  List.iter(
    squares_to_remove,
    ~f=({Point.col, row}) => {
      List.iter(List.range(row, t.height - 1), ~f=row1 =>
        set(t, {Point.row: row1, col}, get(t, {Point.row: row1 + 1, col}))
      );
      set(t, {Point.row: t.height - 1, col}, None);
    },
  );
  mark_squares_that_are_sweepable(t);
};

let add_piece_and_apply_gravity = (t, ~moving_piece, ~col) => {
  /* TODO: insert (affix) the moving piece into the board, applying gravity
     appropriately. Make sure to leave the board in a valid state. */
  let find_row = (~col) =>
    List.find(List.range(0, t.height), ~f=i =>
      switch (get(t, {Point.row: i, col})) {
      | None => true
      | Some(_) => false
      }
    );

  let left_row = find_row(~col);
  let right_row = find_row(~col=col + 1);
  switch (left_row, right_row) {
  | (None, _)
  | (_, None) => false
  | (Some(left_row), Some(right_row)) =>
    if (left_row < t.height - 1 && right_row < t.height - 1) {
      set(
        t,
        {row: left_row, col},
        Some(moving_piece.Moving_piece.bottom_left),
      );
      set(
        t,
        {row: left_row + 1, col},
        Some(moving_piece.Moving_piece.top_left),
      );
      set(
        t,
        {row: right_row, col: col + 1},
        Some(moving_piece.Moving_piece.bottom_right),
      );
      set(
        t,
        {row: right_row + 1, col: col + 1},
        Some(moving_piece.Moving_piece.top_right),
      );
      mark_squares_that_are_sweepable(t);
      true;
    } else {
      false;
    }
  };
};

let is_empty = (t, point) =>
  switch (get(t, point)) {
  | None => true
  | Some(_) => false
  };

/* Tests */
let is_filled_with_color = (t, ~row, ~col, color) =>
  switch (get(t, {Point.row, col})) {
  | None => false
  | Some(square) => Color.equal(color, square.color)
  };

let is_marked = (t, ~row, ~col) =>
  switch (get(t, {Point.row, col})) {
  | None => false
  | Some(square) =>
    Filled_square.Sweeper_state.equal(
      square.Filled_square.sweeper_state,
      Filled_square.Sweeper_state.To_sweep,
    )
  };

let test_piece = {
  Moving_piece.top_left: Filled_square.create(Color.Orange),
  top_right: Filled_square.create(Color.White),
  bottom_left: Filled_square.create(Color.White),
  bottom_right: Filled_square.create(Color.White),
};

let%test "Testing add_piece_and_apply_gravity add one..." = {
  let t = create(~height=4, ~width=4);
  add_piece_and_apply_gravity(t, ~moving_piece=test_piece, ~col=0)
  && is_filled_with_color(t, ~row=0, ~col=0, Color.White)
  && is_filled_with_color(t, ~row=0, ~col=1, Color.White)
  && is_filled_with_color(t, ~row=1, ~col=0, Color.Orange)
  && is_filled_with_color(t, ~row=1, ~col=1, Color.White);
};

let%test "Testing add_piece_and_apply_gravity add many..." = {
  let t = create(~height=4, ~width=4);
  add_piece_and_apply_gravity(t, ~moving_piece=test_piece, ~col=0)
  && add_piece_and_apply_gravity(t, ~moving_piece=test_piece, ~col=0)
  && !add_piece_and_apply_gravity(t, ~moving_piece=test_piece, ~col=0);
};

let test_removable_piece = {
  Moving_piece.top_left: Filled_square.create(Color.White),
  top_right: Filled_square.create(Color.White),
  bottom_left: Filled_square.create(Color.White),
  bottom_right: Filled_square.create(Color.White),
};

let%test "Testing mark_squares_that_are_sweepable..." = {
  let t = create(~height=4, ~width=4);
  assert(
    add_piece_and_apply_gravity(
      t,
      ~moving_piece=test_removable_piece,
      ~col=0,
    ),
  );
  assert(add_piece_and_apply_gravity(t, ~moving_piece=test_piece, ~col=0));
  mark_squares_that_are_sweepable(t);
  is_marked(t, ~row=0, ~col=0)
  && is_marked(t, ~row=0, ~col=1)
  && is_marked(t, ~row=1, ~col=0)
  && is_marked(t, ~row=1, ~col=1)
  && is_marked(t, ~row=2, ~col=0)
  && is_marked(t, ~row=2, ~col=1)
  && !is_marked(t, ~row=3, ~col=0)
  && !is_marked(t, ~row=3, ~col=1);
};

let sweep_board = t =>
  Array.iter(t.board, ~f=row =>
    Array.iter(row, ~f=square =>
      Option.iter(square, ~f=square => ignore(Filled_square.sweep(square)))
    )
  );

let%test "Testing Remove_squares..." = {
  let t = create(~height=4, ~width=4);
  assert(
    add_piece_and_apply_gravity(
      t,
      ~moving_piece=test_removable_piece,
      ~col=0,
    ),
  );
  assert(add_piece_and_apply_gravity(t, ~moving_piece=test_piece, ~col=0));
  mark_squares_that_are_sweepable(t);
  sweep_board(t);
  remove_squares(t);
  is_filled_with_color(t, ~row=0, ~col=0, Color.Orange)
  && is_filled_with_color(t, ~row=0, ~col=1, Color.White);
};
