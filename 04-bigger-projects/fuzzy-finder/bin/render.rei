open! Core;
open! Async;

/** Not every application solely relies on user input in order to
    change it's state (e.g. network connections, stdin.)

    [every how_often_to_call_render render f] is a simple
    helper function that allows users to call [render] at most
    [how_often_to_call_render] per second.

    This is a simple helper function so that users may limit
    how often screen rendering is performed, e.g. so that
    if stdin contains lots of input, rendering does not always
    occur on the event of a new line being read.
*/

let every:
  (
    ~how_often_to_render: Time.Span.t,
    ~render: unit => Deferred.t(unit),
    unit => Deferred.t([ | `Finished('a) | `Repeat(unit)])
  ) =>
  Deferred.t('a);
