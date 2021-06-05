open! Base;

[@deriving sexp_of]
type t = {location: Position.t};

let location = t => t.location;

/* TODO: Implement [create].

   Make sure to inspect the mli to understand the signature of [create]. [create] will
   take in the height and width of the board area, as well as a list of locations where
   the apple cannot be generated, and create a [t] with a random location on the board. */
let create = (~height, ~width, ~invalid_locations) => {
  let possible_locations =
    List.concat_map(List.range(0, height), ~f=row =>
      List.map(List.range(0, width), ~f=col => {Position.row, col})
    )
    |> List.filter(~f=pos =>
         !
           List.mem(
             invalid_locations,
             pos,
             ~equal=[%compare.equal: Position.t],
           )
       );

  switch (possible_locations) {
  | [] => None
  | _ => Some({location: List.random_element_exn(possible_locations)})
  };
};
