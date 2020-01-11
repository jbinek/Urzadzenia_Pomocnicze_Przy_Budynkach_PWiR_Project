-module(fire_sprinkler).
-export([start/0, stop/0]).

% symuluje działanie zraszacza przeciwpożarowego

port() -> 8089.
id() -> sprinkler.

% rejestruje zraszacz na serwerze, przypisuje ID, uruchamia zraszacz na porcie
start() ->
    try
        io:format("Zraszacz przeciwpożarowy uruchamia się, ID = ~p...~n", [id()]),
        emitter_utils:register(controller:address(), controller:port(), id(), port()),
        process_manager:register(id(), self()),
        listen(),
        start
    catch
        _:_ -> io:format("Za duzo procesow przypisanych do jednego zraszacza!~n", []),
            error
    end.

% kończy pracę zraszacza
stop() ->
    try
        emitter_utils:unregister(controller:address(), controller:port(), id()),
        io:format("Zraszacz przecipożarowy o ID = ~p kończy pracę! ~n", [id()]),
        process_manager:kill(id())
    catch
        _:_ -> io:format("Żaden zraszacz nie jest uruchomiony!~n"),
            error
    end.


% decyduje czy załączyć zraszacz czy nie
listen() ->
    case consumer_utils:listen(port()) of
        {_, _, on} ->
            io:format("Zraszacz przeciwpożarowy uruchamia się ~n");
        {_, _, off} ->
            io:format("Zraszacz przeciwpożarowy wyłącza się ~n");
        _ ->
            nil
    end,
    listen().
