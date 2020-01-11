-module(air_conditioning).
-export([start/0, stop/0]).

% air_conditioning symuluje zachowanie klimatyzacji

port() -> 8081.
id() -> ac.


% START
% Rejestruje id klimatyzacji w centrum kontroli na serwerze
% Zaczyna działanie klimatyzacji na podanym porcie

start() ->
    try
        io:format("Klimatyzacja uruchamia się, ID = ~p...~n", [id()]),
        emitter_utils:register(controller:address(), controller:port(), id(), port()),
        process_manager:register(id(), self()),
        listen(),
        start
    catch
        _:_ -> io:format("Za dużo procesów przypisanych do jednego czujnika!~n", []),
        error
    end.

% STOP
% Zatrzymuje klimatyzację

stop() ->
    try
        emitter_utils:unregister(controller:address(), controller:port(), id()),
        io:format("Klimatyzacja o ID = ~p kończy pracę ~n", [id()]),
        process_manager:kill(id())
    catch
        _:_ -> io:format("Klimatyzacja nie jest uruchomiona!~n"),
        error
    end.

% LISTEN
% Czeka na informację o włączeniu lub wyłączeniu klimatyzacji

listen() ->
    case consumer_utils:listen(port()) of
        {_, _, on} ->
            io:format("Klimatyzacja włącza się ~n");
        {_, _, off} ->
            io:format("Klimatyzacja wyłącza się ~n");
        _ ->
            nil
    end,
    listen().
