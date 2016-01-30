-module(basic_example).

-export([open/0, close/1, send/1, listen/0, measure_losses/4]).

open() ->
  SerialPort = serial:start([{open, "/dev/ttyAMA0"}, {speed, 115200}]),
  {ok, SerialPort}.

close(SerialPort) ->
  SerialPort ! {close},
  ok.
  
send(SerialPort) ->
  SerialPort ! {send, <<16#68040468FFCE01018F9C93F516:13/unit:8>>},
  ok.

listen() -> listen(<<>>).

listen(ReceivedBytes) ->
  receive
    % Receive data from the serial port on the caller's PID.
    {data, NewlyReceivedBytes} ->
      listen(<<ReceivedBytes/binary,NewlyReceivedBytes/binary>>)
  after
    % Stop listening after 100 milliseconds of inactivity.
    100 ->
      ReceivedBytes
  end.

measure_losses(SerialPort, Request, CorrectReply, RequestsRemaining) ->
    measure_losses(SerialPort, Request, CorrectReply, 0, RequestsRemaining).

measure_losses(_SerialPort, _Request, _CorrectReply, NumCorrectReplies, 0) ->
    io:format("Correct replies: ~B~n", [NumCorrectReplies]),
    NumCorrectReplies;
measure_losses(SerialPort, Request, CorrectReply, NumCorrectReplies, RequestsRemaining) ->
    timer:sleep(50),
    SerialPort ! {send, Request},
    Reply = listen(),
    io:format("Received ~s~n", [[io_lib:format("~2.16.0B",[X]) || <<X:8>> <= Reply ]]),
    case Reply of
        CorrectReply ->
            measure_losses(SerialPort, Request, CorrectReply, NumCorrectReplies + 1, RequestsRemaining - 1);
        _AnythingElse ->
            measure_losses(SerialPort, Request, CorrectReply, NumCorrectReplies, RequestsRemaining - 1)
    end.
