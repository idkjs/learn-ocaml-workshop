open Core;
open Async;

type t = {
  mutable items: list(string),
  mutable filtered_items: list(string),
  mutable selected: option(string),
  spinner: Spinner.t,
  mutable entered_text: option(string),
};

let create = () => {
  items: [],
  filtered_items: [],
  selected: None,
  spinner: Spinner.create(~spin_every=sec(0.2)),
  entered_text: None,
};

let filter_items_and_selection = (t, entered_text) => {
  let {items, filtered_items: _, selected: _, spinner: _, entered_text: _} = t;
  t.entered_text = entered_text;
  let filtered_items =
    switch (entered_text) {
    | None => items
    | Some(text) =>
      let pattern = String.Search_pattern.create(text);
      items
      |> List.filter(~f=item =>
           Option.is_some @@ String.Search_pattern.index(~in_=item, pattern)
         );
    };

  let (filtered_items, selected) =
    switch (filtered_items) {
    | [selection, ...filtered_items] => (filtered_items, Some(selection))
    | [] => ([], None)
    };

  t.filtered_items = filtered_items;
  t.selected = selected;
};

let widget = (t, screen) => {
  open Tty_text;
  let {items: _, filtered_items, selected, spinner, entered_text} = t;
  let prompt_size = 1;
  let item_count = screen.Screen_dimensions.height - prompt_size;
  let everything_but_selection = List.take(filtered_items, item_count);

  let editor = Widget.text("> " ++ Option.value(~default="", entered_text));

  let lines =
    List.map(everything_but_selection, ~f=text => Widget.text(text));

  let selected =
    Option.map(~f=Widget.text /*~background:Color.grey*/, selected);

  let spinner =
    Option.map(Spinner.to_char(spinner), ~f=c =>
      Widget.text(String.of_char_list([c]))
    );

  let prompt =
    [spinner, Some(editor)] |> List.filter_opt |> Widget.horizontal_group;

  let padding =
    List.init(item_count - List.length(everything_but_selection), ~f=_ =>
      Widget.text("")
    );

  Widget.vertical_group(
    padding @ lines @ List.filter_opt([selected]) @ [prompt],
  );
};

let handle_input = (t, input) =>
  switch (input) {
  | Tty_text.User_input.Backspace =>
    switch (t.entered_text) {
    | None => `Continue(t)
    | Some(text) =>
      let new_entered_text =
        if (Int.(==)(String.length(text), 1)) {
          None;
        } else {
          Some(String.sub(text, ~pos=0, ~len=String.length(text) - 1));
        };

      filter_items_and_selection(t, new_entered_text);
      `Continue(t);
    }
  | Ctrl_c => `Finished(None)
  | Char(x) =>
    let text =
      switch (t.entered_text) {
      | None => String.of_char_list([x])
      | Some(text) => String.(text ++ of_char_list([x]))
      };

    filter_items_and_selection(t, Some(text));
    `Continue(t);
  | Return => `Finished(t.selected)
  | Escape => `Finished(None)
  };

let run = (user_input, tty_text, stdin) => {
  let stdin_reader = Pipe.map(~f=x => `Stdin(x), Reader.lines(stdin));

  let stdin_closed =
    Pipe.create_reader(
      ~close_on_exception=true,
      w => {
        let%bind _ = Reader.close_finished(stdin);
        Pipe.write(w, `Stdin_closed);
      },
    );

  let interleaved =
    Pipe.interleave([
      stdin_reader,
      Pipe.map(user_input, ~f=x => `Input(x)),
      stdin_closed,
    ]);

  let t = create();
  let last_rendered: ref((option(string), list(string))) = (
    ref((None, [])): ref((option(string), list(string)))
  );
  Render.every(
    ~how_often_to_render=Time.Span.of_sec(10.),
    ~render=
      () =>
        if ([%compare.equal: (option(string), list(string))](
              last_rendered^,
              (t.entered_text, t.filtered_items),
            )) {
          Deferred.unit;
        } else {
          last_rendered := (t.entered_text, t.filtered_items);
          Tty_text.render(
            tty_text,
            widget(t, Tty_text.screen_dimensions(tty_text)),
          );
        },
    () =>
      switch%bind (Pipe.read(interleaved)) {
      | `Eof => raise_s([%message "impossible?"])
      | `Ok(`Stdin_closed) =>
        Spinner.finish(t.spinner);
        return(`Repeat());
      | `Ok(`Stdin(x)) =>
        t.items = [x, ...t.items];
        filter_items_and_selection(t, t.entered_text);
        return(`Repeat());
      | `Ok(`Input(user_input)) =>
        switch (handle_input(t, user_input)) {
        | `Finished(None) => return(`Finished(None))
        | `Finished(Some(x)) => return(`Finished(Some(x)))
        | `Continue(_) => return(`Repeat())
        }
      },
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
