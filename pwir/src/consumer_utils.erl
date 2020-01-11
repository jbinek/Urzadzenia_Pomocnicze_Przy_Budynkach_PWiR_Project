-module(consumer_utils).
-export([listen/1]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helpful functions that simplify communication with UDP protocol.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function: read
%% Purpose: Reads data send using UDP protocol on a given port for given amount of milliseconds.
%% Arguments: Port and amount of milliseconds to wait.
%% Returns: Tuple {SenderAddress, SenderPort, Data} or {}
%%     if error has occurred or no data has been read.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
listen(Port, Timeout) ->
    case gen_udp:open(Port, [binary, {active, false}]) of
        {ok, Socket} ->
            Return = listenSocket(Socket, Timeout);
        {error, eaddrinuse} ->
            io:format("Port ~p is being used by other process!~n", [Port]),
            Return = {error, eaddrinuse};
        {error, Reason} ->
            Return = {error, Reason}
    end,
    Return.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function: read
%% Purpose: Reads data send using UDP protocol on a given port for 100000 milliseconds.
%% Arguments: Port.
%% Returns: Tuple {SenderAddress, SenderPort, Data} or {}
%%     if error has occurred or no data has been read.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
listen(Port) ->
    listen(Port, 100000).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function: recv
%% Purpose: Reads data send using UDP protocol on a given socket for given amount of milliseconds.
%% Arguments: Socket and amount of milliseconds to wait.
%% Returns: Tuple {SenderAddress, SenderPort, Data} or {}
%%     if error has occurred or no data has been read.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
listenSocket(Socket, Timeout) ->
    case gen_udp:recv(Socket, 0, Timeout) of
        {ok, {Address, Port, Packet}} ->
            Return = {Address, Port, binary_to_term(Packet)};
        {error, Reason} ->
            io:format("Error while listening to ~p: ~p~n", [Socket, Reason]),
            Return = {error, Reason}
    end,
    gen_tcp:close(Socket),
    Return.
