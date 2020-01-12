-module(klimatyzacja).
-export([start/0, stop/0]).

% air_conditioning symuluje zachowanie klimatyzacji

port() -> 8081.
id() -> klima.


% START
% Rejestruje id klimatyzacji w centrum kontroli na serwerze
% Zaczyna działanie klimatyzacji na podanym porcie

start() ->
    try
        io:format("Klimatyzacja uruchamia sie, ID = ~p...~n", [id()]),
        czujniki_UDP:register(centrum_kontroli:address(), centrum_kontroli:port(), id(), port()),
        kontroler_pid:register(id(), self()),
        nasluchuj(),
        start
    catch
        _:_ -> io:format("Za duzo procesow przypisanych do jednego czujnika!~n", []),
        error
    end.

% STOP
% Zatrzymuje klimatyzację

stop() ->
    try
        czujniki_UDP:unregister(centrum_kontroli:address(), centrum_kontroli:port(), id()),
        io:format("Klimatyzacja o ID = ~p konczy prace ~n", [id()]),
        kontroler_pid:kill(id())
    catch
        _:_ -> io:format("Klimatyzacja nie jest uruchomiona!~n"),
        error
    end.

% NASLUCHUJ
% Czeka na informację o włączeniu lub wyłączeniu klimatyzacji

nasluchuj() ->
    case klient_UDP:nasluchuj(port()) of
        {_, _, on} ->
            io:format("Klimatyzacja wlacza sie ~n");
        {_, _, off} ->
            io:format("Klimatyzacja wylacza sie ~n");
        _ ->
            nil
    end,
    nasluchuj().
