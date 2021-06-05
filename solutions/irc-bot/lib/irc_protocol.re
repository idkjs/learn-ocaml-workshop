module Irc_message = {
  include Message;
  let parser_ = Parser.message;
};
