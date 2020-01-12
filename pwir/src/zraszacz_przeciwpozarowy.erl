-module(zraszacz_przeciwpozarowy).
-export([start/0, stop/0]).

% symuluje działanie zraszacza przeciwpożarowego

port() -> 8089.
id() -> zraszacz.

% START
% rejestruje zraszacz na serwerze, przypisuje ID, uruchamia zraszacz na porcie
start() ->
    try
        io:format("Zraszacz przeciwpozarowy uruchamia sie, ID = ~p...~n", [id()]),
        czujniki_UDP:register(centrum_kontroli:address(), centrum_kontroli:port(), id(), port()),
        kontroler_pid:register(id(), self()),
        nasluchuj(),
        start
    catch
        _:_ -> io:format("Za duzo procesow przypisanych do jednego zraszacza!~n", []),
            error
    end.

% STOP
% kończy pracę zraszacza
stop() ->
    try
        czujniki_UDP:unregister(centrum_kontroli:address(), centrum_kontroli:port(), id()),
        io:format("Zraszacz przecipozarowy o ID = ~p konczy prace! ~n", [id()]),
        kontroler_pid:kill(id())
    catch
        _:_ -> io:format("Zaden zraszacz nie jest uruchomiony!~n"),
            error
    end.


% NASLUCHUJ
% decyduje czy załączyć zraszacz czy nie
nasluchuj() ->
    case klient_UDP:nasluchuj(port()) of
        {_, _, on} ->
            io:format("Zraszacz przeciwpozarowy uruchamia sie ~n");
        {_, _, off} ->
            io:format("Zraszacz przeciwpozarowy wylacza sie ~n");
        _ ->
            nil
    end,
    nasluchuj().
