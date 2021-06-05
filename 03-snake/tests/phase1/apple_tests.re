open! Base;
open! Snake_lib;
open Apple;

let%expect_test "Testing [Apple.create]..." = {
  let apple = create(~height=10, ~width=10, ~invalid_locations=[]);
  switch (apple) {
  | None => failwith("[create] returned [None] when [Some _] was expected!")
  | Some(apple) =>
    let {Position.row, col} = location(apple);
    if (row < 0 || row >= 10 || col < 0 || col >= 10) {
      failwith("[create] returned an invalid apple!");
    } else {
      ();
    };
  };
};

let%expect_test "Testing [Apple.create]..." = {
  let invalid_locations =
    List.init(10, ~f=row => List.init(10, ~f=col => {Position.row, col}))
    |> List.concat;

  let apple = create(~height=10, ~width=10, ~invalid_locations);
  Stdio.printf("%{sexp: t option}\n%!"^, apple);
  %expect
  {| () |};
};
