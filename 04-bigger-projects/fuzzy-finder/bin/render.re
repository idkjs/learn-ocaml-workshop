open! Core;
open! Async;

let every = (~how_often_to_render, ~render, f) => {
  let init = Deferred.unit;
  let next_render_time = () => Clock.after(how_often_to_render);
  Deferred.repeat_until_finished((init, f()), ((next_render, f_call)) =>
    switch%bind (
      Deferred.choose([
        choice(next_render, () => `Render),
        choice(f_call, x => `F_call(x)),
      ])
    ) {
    | `Render =>
      let%map () = render();
      `Repeat((next_render_time(), f_call));
    | `F_call(`Finished(_) as x) => return(x)
    | `F_call(`Repeat ()) =>
      let next_f = f();
      return(`Repeat((next_render, next_f)));
    }
  );
};
