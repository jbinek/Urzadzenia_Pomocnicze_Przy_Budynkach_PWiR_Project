-module(czujnik_antywlamaniowy).
-export([start/0, stop/0]).

% symuluje zachowanie czujnika antywłamaniowego

id() -> czujnik_antywlam.

% START
% Rejestruje id czujnika antywłamaniowego w centrum kontroli na serwerze
% Zaczyna działanie czujnika na podanym porcie

start() ->
    try
        io:format("Czujnik antywlamaniowy uruchamia sie, ID = ~p...~n", [id()]),
        czujniki_UDP:register(centrum_kontroli:address(), centrum_kontroli:port(), id(), 0),
        kontroler_pid:register(id(), self()),
        emit()
    catch
        _:_ -> io:format("Za duzo procesow przypisanych do jednego czujnika!~n", []),
        error
    end.

% STOP
% Zatrzymuje czujnik antywłamaniowy

stop() ->
    try
        czujniki_UDP:unregister(centrum_kontroli:address(), centrum_kontroli:port(), id()),
        io:format("Czujnik antywlamaniowy o ID = ~p kończy pracę ~n", [id()]),
        kontroler_pid:kill(id())
    catch
        _:_ -> io:format("Czujnik antywlamaniowy nie jest uruchomiony!~n"),
        error
    end.

% EMIT
% Czujnik co 25 sekund wysyła do centrum kontroli informację o potencjalnym włamaniu

emit() ->
    czujniki_UDP:sendData(centrum_kontroli:address(), centrum_kontroli:port(), id(), rand:uniform(24)),
    timer:sleep(timer:seconds(25)),
    emit().
