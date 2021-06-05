open Core;
open Async;

module User_input = {
  [@deriving sexp_of]
  type t =
    | Ctrl_c
    | Escape
    | Backspace
    | Return /* Enter key */
    | Char(char);
};

module Configure_terminal = {
  type t = {
    attr_in: Unix.Terminal_io.t,
    attr_out: Unix.Terminal_io.t,
  };

  let setattr_out = (fd, ~attr_out) =>
    Unix.Terminal_io.tcsetattr(
      attr_out,
      ~mode=Unix.Terminal_io.TCSAFLUSH,
      fd,
    );

  let setattr_in = (fd, ~attr_in) =>
    Unix.Terminal_io.tcsetattr(attr_in, ~mode=Unix.Terminal_io.TCSADRAIN, fd);

  let get_current_settings = (~input, ~output, ()) => {
    let%bind attr_in = Unix.Terminal_io.tcgetattr(input);
    let%bind attr_out = Unix.Terminal_io.tcgetattr(output);
    return({attr_in, attr_out});
  };

  let set = (~input, ~output, t) => {
    let%bind () = setattr_in(input, ~attr_in=t.attr_in);
    let%bind () = setattr_out(output, ~attr_out=t.attr_out);
    return();
  };

  let map_termio = (attrs: Core.Unix.Terminal_io.t) => {
    ...attrs,
    Core.Unix.Terminal_io.c_ignbrk: false,
    c_brkint: false,
    c_parmrk: false,
    c_istrip: false,
    c_inlcr: false,
    c_igncr: false,
    c_icrnl: false,
    c_ixon: false,
    c_opost: false,
    c_echo: false,
    c_echonl: false,
    c_icanon: false,
    c_isig: false,
    c_csize: 8,
    c_parenb: false,
    c_vmin: 1,
    c_vtime: 0,
  };

  let to_drawing = t => {
    attr_in: map_termio(t.attr_in),
    attr_out: map_termio(t.attr_out),
  };
};

let esc = rest => "\027[" ++ rest;

module Direction = {
  [@deriving enumerate]
  type t =
    | Up
    | Down
    | Left
    | Right;

  let escape =
    fun
    | Up => esc("A")
    | Down => esc("B")
    | Right => esc("C")
    | Left => esc("D");
};

module Action = {
  type t =
    | Clear_screen
    | Move_cursor_to_home
    | Next_line
    | Move(Direction.t)
    | Switch_to_alternate_buffer
    | Switch_from_alternate_buffer
    | Erase_to_end_of_line;

  let _compilation_fix_for_unused_constructor = Move(Left);

  let to_string =
    fun
    | Clear_screen => esc("2J")
    | Erase_to_end_of_line => esc("K")
    | Move_cursor_to_home => esc("H")
    | Next_line => "\r\n"
    | Move(dir) => Direction.escape(dir)
    | Switch_to_alternate_buffer => esc("?1049h")
    | Switch_from_alternate_buffer => esc("?1049l");
};

let do_action = (writer, action) => {
  Writer.write(writer, Action.to_string(action));
  Writer.flushed(writer);
};

type t = {
  dimensions: Screen_dimensions.t,
  writer: Writer.t,
};

let screen_dimensions = ({dimensions, _}) => dimensions;

let stop_rendering = t => do_action(t, Switch_from_alternate_buffer);

let with_rendering = f => {
  let%bind tty_reader = Reader.open_file(~buf_len=1, "/dev/tty");
  let%bind tty_writer = Writer.open_file("/dev/tty");
  let input = Reader.fd(tty_reader);
  let output = Writer.fd(tty_writer);
  let%bind original =
    Configure_terminal.get_current_settings(~input, ~output, ());
  let restore = () => Configure_terminal.set(original, ~input, ~output);

  let%bind dimensions = {
    let%map output =
      Process.run_exn(
        (),
        ~prog="stty",
        /* NOTE: for people on Mac OS X, use ~args:[ "-f"; "/dev/tty"; "size" ] */
        ~args=["size", "-F", "/dev/tty"],
      );

    switch (output |> String.strip |> String.split(~on=' ')) {
    | [height, width] => {
        Screen_dimensions.height: Int.of_string(height),
        width: Int.of_string(width),
      }
    | _ => raise_s([%message "Could not determine terminal size"])
    };
  };

  Monitor.protect(
    ~finally=restore,
    () => {
      let t = {dimensions, writer: tty_writer};
      let%bind () =
        Configure_terminal.to_drawing(original)
        |> Configure_terminal.set(~input, ~output);

      let%bind () = do_action(tty_writer, Switch_to_alternate_buffer);
      let%bind () = do_action(tty_writer, Clear_screen);
      let user_input =
        Pipe.create_reader(
          ~close_on_exception=true,
          w => {
            let repeat = x => {
              let%bind () = Pipe.write(w, x);
              return(`Repeat());
            };

            let b = Bytes.create(1);
            Deferred.repeat_until_finished((), () =>
              switch%bind (Reader.really_read(~len=1, tty_reader, b)) {
              | `Eof(_) => return(`Finished())
              | `Ok =>
                switch (Char.to_int(Bytes.get(b, 0))) {
                | 3 /* CTRL + C */ =>
                  let%bind () = Pipe.write(w, User_input.Ctrl_c);
                  return(`Finished());
                | 0O177 => repeat(Backspace)
                | 0O015 => repeat(Return)
                | 0O33 => repeat(Escape)
                | _ => repeat(Char(Bytes.get(b, 0)))
                }
              }
            );
          },
        );

      let%bind to_return = f((user_input, t));
      let%bind () = restore();
      let%bind () = stop_rendering(t.writer);
      return(to_return);
    },
  );
};

module Widget = {
  type t =
    | Text(string)
    | Group_horizontally(list(t))
    | Stack_vertically(list(t));

  let text = text => Text(text);
  let horizontal_group = ts => Group_horizontally(ts);
  let vertical_group = ts => Stack_vertically(ts);

  let render = (elts, writer) => {
    let rec process =
      fun
      | Text(x) => Writer.writef(writer, "%s", x)
      | Group_horizontally(xs) => List.iter(xs, ~f=process)
      | Stack_vertically(xs) =>
        xs
        |> List.map(~f=(x, ()) => process(x))
        |> List.intersperse(~sep=() =>
             Writer.writef(
               writer,
               "%{Action}%{Action}"^,
               Erase_to_end_of_line,
               Next_line,
             )
           )
        |> List.iter(~f=f => f());

    Writer.writef(writer, "%{Action}"^, Move_cursor_to_home);
    process(elts);
    Writer.writef(writer, "%{Action}"^, Erase_to_end_of_line);
    Writer.flushed(writer);
  };
};

let render = (t, w) => Widget.render(w, t.writer);
