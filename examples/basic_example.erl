-module(basic_example).

-export([open/0, close/1, send/1, listen/0]).

open() ->
  SerialPort = serial:start([{open, "/dev/ttyAMA0"}, {speed, 9600}]),
  {ok, SerialPort}.

close(SerialPort) ->
  SerialPort ! {close},
  ok.
  
send(SerialPort) ->
  SerialPort ! {send, <<16#68040468FFCE01018F9C93F516:13/unit:8>>},
  ok.

listen() ->
  receive
    % Receive data from the serial port on the caller's PID.
    {data, Bytes} ->
      io:format("~s", [[io_lib:format("~2.16.0B",[X]) || <<X:8>> <= Bytes ]]),
      listen()
  after
    % Stop listening after 5 seconds of inactivity.
    5000 ->
      io:format("~n"),
      ok
  end.
