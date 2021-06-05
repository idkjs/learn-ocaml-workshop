open Base;
open Js_of_ocaml;
/* ## - method will get called as soon I deref.
      `##.prop_name := ` to set a property
      ##.prop_name to read (no deref)

      put a table here ; bg here ; each cell has an id
   */

let get_foo_div = () =>
  Option.value_exn(
    Js.Opt.to_option(Dom_html.document##getElementById(Js.string("foo"))),
  );

let () = {
  Dom_html.window##.onload :=
    Dom.handler(_ =>
      let foo_div = get_foo_div();
      foo_div##.textContent := Js.Opt.return(Js.string("Hello, world!"));
      Js._true;
    );
  Dom_html.window##.onkeydown :=
    Dom.handler(key_event =>
      let foo_div = get_foo_div();
      let key = Option.value_exn(Js.Optdef.to_option(key_event##.key));
      let () =
        switch (Js.to_string(key)) {
        | "ArrowUp"
        | "ArrowDown"
        | "ArrowLeft"
        | "ArrowRight" => foo_div##.textContent := Js.Opt.return(key)
        | _ => ()
        };

      Js._true;
    );
  let ticktock = ref("tick");
  let _ =
    Dom_html.window##setInterval(
      Js.wrap_callback(() =>
        let foo_div = get_foo_div();
        foo_div##.textContent := Js.Opt.return(Js.string(ticktock^));
        ticktock :=
          (
            switch (ticktock^) {
            | "tick" => "tock"
            | "tock" => "tick"
            | _ => "error"
            }
          );
      ),
      1000.0,
    );

  ();
};
