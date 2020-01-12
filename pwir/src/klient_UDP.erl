-module(klient_UDP).
-export([listen/1]).

% Funkcje pomocne przy komunikacji protokołem UDP

% LISTEN
% Nasłuchuje danych wysłanych przez UDP na podanym porcie przez konkretny czas w ms

listen(Port, Timeout) ->
    case gen_udp:open(Port, [binary, {active, false}]) of
        {ok, Socket} ->
            Return = listenSocket(Socket, Timeout);
        {error, eaddrinuse} ->
            io:format("Port ~p jest używany przez inny proces!~n", [Port]),
            Return = {error, eaddrinuse};
        {error, Reason} ->
            Return = {error, Reason}
    end,
    Return.

% LISTEN
% Nasłuchuje danych wysłanych przez UDP na podanym porcie przez 100000 ms

listen(Port) ->
    listen(Port, 100000).

% LISTENSOCKET
% Nasłuchuje danych wysłanych przez UDP na podanym gnieździe przez konkretny czas w ms

listenSocket(Socket, Timeout) ->
    case gen_udp:recv(Socket, 0, Timeout) of
        {ok, {Address, Port, Packet}} ->
            Return = {Address, Port, binary_to_term(Packet)};
        {error, Reason} ->
            io:format("Blad z podanym gniazdem: ~p: ~p~n", [Socket, Reason]),
            Return = {error, Reason}
    end,
    gen_tcp:close(Socket),
    Return.
