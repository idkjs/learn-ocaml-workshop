open! Base;
open! Js_of_ocaml;
open! Import;

open Scaffold;

module Screen = {
  type t = {
    context: Js.t(Html.canvasRenderingContext2D),
    width: int,
    height: int,
  };
};

module Image_impl = {
  type t = {image_element: Js.t(Html.imageElement)};

  let create = path => {
    let image_element = Html.createImg(document);
    image_element##.src := Js.string(path);
    {image_element: image_element};
  };

  /* If we were using a concurrency library like [Async] or [Lwt], we would want
     to make [width] and [height] members of the record. But they can only be
     read after the image has loaded. */
  let width = t => jsoptdef_value_exn(t.image_element##.naturalWidth);
  let height = t => jsoptdef_value_exn(t.image_element##.naturalHeight);

  let draw = (screen: Screen.t, t, x, y, img_width, img_height) => {
    let f = Int.to_float;
    screen.context##drawImage_full(
      t.image_element,
      0.,
      0.,
      width(t) |> f,
      height(t) |> f,
      x,
      y,
      f(img_width),
      f(img_height),
    );
  };
};

module Wad = {
  [@deriving fields]
  type t = {
    background: Image_impl.t,
    skull_and_crossbones: Image_impl.t,
    frog_up: Image_impl.t,
    frog_down: Image_impl.t,
    frog_left: Image_impl.t,
    frog_right: Image_impl.t,
    car1_left: Image_impl.t,
    car2_left: Image_impl.t,
    car1_right: Image_impl.t,
    car2_right: Image_impl.t,
    car3_left: Image_impl.t,
    car3_right: Image_impl.t,
    confetti: Image_impl.t,
    log1: Image_impl.t,
    log2: Image_impl.t,
    log3: Image_impl.t,
  };

  let create = (_config: Config.t) => {
    let background = Image_impl.create("assets/background.png");
    let skull_and_crossbones = Image_impl.create("assets/skull.png");
    let frog_up = Image_impl.create("assets/camel-up.png");
    let frog_down = Image_impl.create("assets/camel-down.png");
    let frog_left = Image_impl.create("assets/camel-left.png");
    let frog_right = Image_impl.create("assets/camel-right.png");
    let car1_left = Image_impl.create("assets/buggy-left.png");
    let car1_right = Image_impl.create("assets/buggy-right.png");
    let car2_left = Image_impl.create("assets/truck-left.png");
    let car2_right = Image_impl.create("assets/truck-right.png");
    let car3_left = Image_impl.create("assets/police-car-left.png");
    let car3_right = Image_impl.create("assets/police-car-right.png");
    let log1 = Image_impl.create("assets/carpet_blue.png");
    let log2 = Image_impl.create("assets/carpet_green.png");
    let log3 = Image_impl.create("assets/carpet_red.png");
    let confetti = Image_impl.create("assets/confetti.png");
    {
      background,
      skull_and_crossbones,
      frog_up,
      frog_down,
      frog_left,
      frog_right,
      car1_left,
      car2_left,
      car1_right,
      car2_right,
      car3_left,
      car3_right,
      confetti,
      log1,
      log2,
      log3,
    };
  };

  let lookup_image = (t, image: Image.t) =>
    switch (image) {
    | Frog_up => t.frog_up
    | Frog_down => t.frog_down
    | Frog_left => t.frog_left
    | Frog_right => t.frog_right

    | Car1_left => t.car1_left
    | Car1_right => t.car1_right
    | Car2_left => t.car2_left
    | Car2_right => t.car2_right
    | Car3_left => t.car3_left
    | Car3_right => t.car3_right

    | Log1 => t.log1
    | Log2 => t.log2
    | Log3 => t.log3

    | Confetti => t.confetti
    | Skull_and_crossbones => t.skull_and_crossbones
    };
};
