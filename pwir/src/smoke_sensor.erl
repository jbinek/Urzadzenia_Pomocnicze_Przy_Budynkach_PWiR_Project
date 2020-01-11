-module(smoke_sensor).
-export([start/0, stop/0]).

% symuluje działanie detektora dymu

id() -> smoke.

% rejestruje detektor na serwerze, przypisuje ID, uruchamia detektor na porcie
start() ->
    try
        io:format("Detektor dymu uruchamia się, ID = ~p...~n", [id()]),
        emitter_utils:register(controller:address(), controller:port(), id(), 0),
        process_manager:register(id(), self()),
        emit()
    catch
        _:_ -> io:format("Za duzo procesow przypisanych do jednego detektora!~n", []),
            error
    end.

% kończy pracę detektora
stop() ->
    try
        emitter_utils:unregister(controller:address(), controller:port(), id()),
        io:format("Detektor dymu o ID = ~p kończy pracę! ~n", [id()]),
        process_manager:kill(id())
    catch
        _:_ -> io:format("Żaden detektor nie jest uruchomiony!~n"),
            error
    end.

% co 10 sekund losuje czy wyslac powiadomienie do centrum kontroli o wykryciu dymu czy nie
emit() ->
    case rand:uniform(2) of
        1 ->
            emitter_utils:sendData(controller:address(), controller:port(), id(), yes);
        _ ->
            emitter_utils:sendData(controller:address(), controller:port(), id(), no)
    end,
    timer:sleep(timer:seconds(10)),
    emit().
