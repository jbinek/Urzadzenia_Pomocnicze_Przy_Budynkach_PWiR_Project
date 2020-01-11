-module(anti_intrusion_sensor).
-export([start/0, stop/0]).

% anti_intrusion_sensor symuluje zachowanie czujnika antywłamaniowego

id() -> intrusion.

% START
% Rejestruje id czujnika antywłamaniowego w centrum kontroli na serwerze
% Zaczyna działanie czujnika na podanym porcie

start() ->
    try
        io:format("Czujnik antywłamaniowy uruchamia się, ID = ~p...~n", [id()]),
        emitter_utils:register(controller:address(), controller:port(), id(), 0),
        process_manager:register(id(), self()),
        emit()
    catch
        _:_ -> io:format("Za dużo procesów przypisanych do jednego czujnika!~n", []),
        error
    end.

% STOP
% Zatrzymuje czujnik antywłamaniowy

stop() ->
    try
        emitter_utils:unregister(controller:address(), controller:port(), id()),
        io:format("Czujnik antywłamaniowy o ID = ~p kończy pracę ~n", [id()]),
        process_manager:kill(id())
    catch
        _:_ -> io:format("Czujnik antywłamaniowy nie jest uruchomiony!~n"),
        error
    end.

% EMIT
% Czujnik co 25 sekund wysyła do centrum kontroli informację o potencjalnym włamaniu

emit() ->
    case rand:uniform(1) of
        1 ->
            emitter_utils:sendData(controller:address(), controller:port(), id(), yes);
        _ ->
        emitter_utils:sendData(controller:address(), controller:port(), id(), no)
    end,
    timer:sleep(timer:seconds(25)),
    emit().
